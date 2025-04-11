# given a table name, return a function that takes a dataset reference and
# returns data from the given table in that dataset
table_getter <- function(table_name, max_results = NULL) {
  \(dataset) {
    message(glue("--Fetching table {table_name}"))
    suppressWarnings(
      dataset$table(table_name)$to_tibble(max_results = max_results)
    )
  }
}

# given a sql query, return a function that takes a dataset reference and
# returns data from executing that sql query in that dataset
query_getter <- function(query_str, max_results = NULL) {
  message(glue("--Executing SQL query"))
  \(dataset) {
    suppressWarnings(
      dataset$query(query_str)$to_tibble(max_results = max_results)
    )
  }
}

# given a dataset spec (list of lists of name and version strings)
# and a function that takes a dataset and returns a tibble
# run the function for each dataset and combine results into one tibble
get_datasets_data <- function(dataset_spec, dataset_fun) {

  # get reference to organization
  org <- redivis::redivis$organization("levante")

  # get reference to each dataset in dataset_spec
  datasets <- dataset_spec |>
    rlang::set_names(map_chr(dataset_spec, \(dn) dn[["name"]])) |>
    map(\(dn) org$dataset(name = dn$name, version = dn$version))

  # fetch each dataset to populate its properties
  walk(datasets, \(ds) ds$get())

  # get each dataset's canonical reference (name + persistent ID + version)
  dataset_refs <- datasets |> map(\(ds) ds$scoped_reference)

  # apply dataset_fun to each dataset
  dataset_data <- imap(datasets, \(dataset, dataset_name) {
    message(glue::glue("Fetching data for {dataset_name}"))
    dataset_fun(dataset) |>
      mutate(dataset = dataset_refs[[dataset_name]], .before = everything())
  })

  # combine data over datasets
  bind_rows(dataset_data)
}

#' Get participants
#'
#' @inheritParams get_runs
#'
#' @export
get_participants <- function(dataset_spec, max_results = NULL) {

  # get data for the tables with needed user data
  user_tables <- c("groups", "runs", "users", "user_groups")
  user_data <- user_tables |> rlang::set_names() |>
    map(\(table_name) get_datasets_data(dataset_spec,
                                        table_getter(table_name, max_results)))

  # filter users to only children and replace invalid birth months/years with NA
  users <- user_data$users |>
    filter(.data$user_type %in% c("guest", "student")) |>
    mutate(across(contains("birth"), \(v) v |> na_if(0) |> na_if(10000)))

  # extract non-missing birth month + year
  births <- users |>
    select("user_id", "birth_month", "birth_year") |>
    filter(!is.na(.data$birth_month), !is.na(.data$birth_year))

  # combine runs (which have date of test) with births and compute age
  ages <- user_data$runs |>
    select("user_id", "run_id", "time_started") |>
    inner_join(births, by = "user_id") |>
    mutate(age = compute_age(.data$birth_month, .data$birth_year, .data$time_started))

  # collapse run ages for each user
  user_ages <- ages |>
    select("user_id", "run_id", "age") |>
    nest(ages = -.data$user_id)

  # groups
  groups <- user_data$group |> distinct() |>
    select("group_id", group_name = "name",
           group_abbreviation = "abbreviation")

  # combine user to group mapping with groups, collapse groups for each user
  user_groups <- user_data$user_groups |> distinct() |>
    select("user_id", "group_id") |>
    left_join(groups, by = "group_id") |>
    nest(groups = -.data$user_id)

  # add ages and groups back into users, keep only needed columns
  users |>
    left_join(user_groups, by = "user_id") |>
    left_join(user_ages, by = "user_id") |>
    select("dataset", "user_id", "birth_month", "birth_year", "sex", "grade",
           matches("_id"), "groups", "ages")
}

# construct SQL WHERE clause out of a variable name and vector of allowed values
build_filter <- function(var, vals) {
  vals_str <- glue("'{vals}'") |> paste(collapse = ", ")
  if (is.null(vals)) "" else glue("WHERE {var} IN ({vals_str})")
}

#' Get trial data
#'
#' @inheritParams get_runs
#' @param remove_invalid_trials Boolean indicating whether to drop trials that
#'   were marked as invalid (defaults to FALSE).
#' @param tasks Character vector of tasks to include.
#' @param participants (Optional) Data frame that includes the columns "dataset"
#'   and "user_id", if supplied trial data will be filtered to only those user
#'   IDs.
#'
#' @export
get_trials <- function(dataset_spec,
                       remove_incomplete_runs = TRUE,
                       remove_invalid_runs = TRUE,
                       remove_invalid_trials = FALSE,
                       tasks = NULL, # all tasks if null
                       participants = NULL, # all participants if null
                       max_results = NULL) {

  where_str <- build_filter("task_id", tasks)
  query_str <- glue("SELECT * FROM trials {where_str}") |> stringr::str_trim()

  trials <- get_datasets_data(dataset_spec, query_getter(query_str, max_results))

  # if participants supplied, filter to trials for only those participants
  if (!is.null(participants)) {
    trials <- trials |> semi_join(participants, by = c("user_id", "dataset"))
  }

  # if run filters supplied, get corresponding runs and filter to their trials
  if (any(remove_incomplete_runs, remove_invalid_runs)) {
    runs <- get_runs(dataset_spec,
                     remove_incomplete_runs = remove_incomplete_runs,
                     remove_invalid_runs = remove_invalid_runs)
    trials |> semi_join(runs, by = c("run_id"))
  }

  # filter to valid trials
  if (remove_invalid_trials) trials <- trials |> filter(.data$valid_trial)

  trials |>
    remove_practice_trials() |>
    select("dataset", "task_id", "user_id", "run_id", "trial_id",
           # item_id_original = item_id, item_original = item,
           "response", "correct", "rt", "server_timestamp") |>
    convert_rts() |>
    add_trial_items() |>
    add_trial_numbers() |>
    code_numberline() |>
    arrange(.data$dataset, .data$task_id, .data$user_id, .data$run_id, .data$trial_number) |>
    select("dataset", "task_id", "user_id", "run_id", "trial_id", "trial_number",
           "item_uid", "item_group", "item", #item_id_original,
           "correct", "rt", "rt_numeric",
           timestamp = "server_timestamp")
}

#' Get run data
#'
#' @param dataset_spec List of dataset names and versions to retrieve.
#' @param remove_incomplete_runs Boolean indicating whether to drop runs that
#'   were marked as incomplete (defaults to TRUE).
#' @param remove_invalid_runs Boolean indicating whether to drop runs that were
#'   marked as invalid (defaults to TRUE).
#' @param max_results Max number of records to load for each table (defaults to
#'   entire table).
#'
#' @export
get_runs <- function(dataset_spec,
                     remove_incomplete_runs = TRUE, remove_invalid_runs = TRUE,
                     max_results = NULL) {

  runs <- get_datasets_data(dataset_spec, table_getter("runs", max_results))

  if (remove_invalid_runs) runs <- runs |> filter(.data$valid_run)
  if (remove_incomplete_runs) runs <- runs |> filter(.data$completed)

  runs |> select("user_id", "run_id")
}

#' Get survey data
#'
#' @inheritParams get_runs
#' @param survey_types Character vector of survey types to include (caregiver,
#'   student, teacher).
#' @param remove_incomplete_surveys Boolean indicating whether to drop surveys
#'   that were marked as incomplete (defaults to FALSE).
#'
#' @export
get_surveys <- function(dataset_spec,
                        survey_types = c("caregiver", "student", "teacher"),
                        remove_incomplete_surveys = FALSE,
                        max_results = NULL) {

  where_str <- build_filter("survey_id", survey_types)
  query_str <- glue("SELECT * FROM survey_responses {where_str}") |> stringr::str_trim()

  surveys <- get_datasets_data(dataset_spec, query_getter(query_str, max_results))
  if (remove_incomplete_surveys) surveys <- surveys |> filter(.data$is_complete)

  surveys |>
    add_survey_items() |>
    code_survey_data()
}

get_metadata_table <- function(table_name) {
  metadata <- redivis::redivis$organization("levante")$dataset("metadata")
  message(glue::glue("Fetching item metadata for {table_name}"))
  suppressWarnings(
    metadata$table(table_name)$to_tibble()
  )
}

#' Get metadata for trial items
#'
#' @export
get_trial_items <- function() {
  get_metadata_table("trial_items")
}

#' Get metadata for survey items
#'
#' @export
get_survey_items <- function() {
  get_metadata_table("survey_items")
}
