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
    set_names(map_chr(dataset_spec, \(dn) dn[["name"]])) |>
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

#' @export
get_participants <- function(dataset_spec, max_results = NULL) {

  # get data for the tables with needed user data
  user_tables <- c("groups", "runs", "users", "user_groups")
  user_data <- user_tables |> set_names() |>
    map(\(table_name) get_datasets_data(dataset_spec,
                                        table_getter(table_name, max_results)))

  # filter users to only children and replace invalid birth months/years with NA
  users <- user_data$users |>
    filter(user_type %in% c("guest", "student")) |>
    mutate(across(contains("birth"), \(v) v |> na_if(0) |> na_if(10000)))

  # extract non-missing birth month + year
  births <- users |>
    select(.data$user_id, .data$birth_month, .data$birth_year) |>
    filter(!is.na(.data$birth_month), !is.na(.data$birth_year))

  # combine runs (which have date of test) with births and compute age
  ages <- user_data$runs |>
    select(.data$user_id, .data$run_id, .data$time_started) |>
    inner_join(births, by = "user_id") |>
    mutate(age = compute_age(birth_month, birth_year, time_started))

  # collapse run ages for each user
  user_ages <- ages |>
    select(.data$user_id, .data$run_id, .data$age) |>
    nest(ages = -.data$user_id)

  # groups
  groups <- user_data$group |> distinct() |>
    select(.data$group_id, group_name = .data$name,
           group_abbreviation = .data$abbreviation)

  # combine user to group mapping with groups, collapse groups for each user
  user_groups <- user_data$user_groups |> distinct() |>
    select(.data$user_id, .data$group_id) |>
    left_join(groups, by = "group_id") |>
    nest(groups = -.data$user_id)

  # add ages and groups back into users, keep only needed columns
  users |>
    left_join(user_groups, by = "user_id") |>
    left_join(user_ages, by = "user_id") |>
    select(dataset, user_id, birth_month, birth_year, sex, grade, matches("_id"), groups, ages)
}

# construct SQL WHERE clause out of a variable name and vector of allowed values
build_filter <- function(var, vals) {
  vals_str <- glue("'{vals}'") |> paste(collapse = ", ")
  if (is.null(vals)) "" else glue("WHERE {var} IN ({vals_str})")
}

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
  # TODO: would be more efficient to do runs filter in sql

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
  if (remove_invalid_trials) trials <- trials |> filter(valid_trial)

  trials |>
    remove_practice_trials() |>
    select(dataset, task_id, user_id, run_id, trial_id,
           item_id_original = item_id, correct, rt, server_timestamp) |>
    # mutate(has_item_info = !is.na(item_id) | item != "" | str_length(answer) > 0) |>
    convert_rts() |>
    add_trial_items() |>
    add_trial_numbers() |>
    arrange(dataset, task_id, user_id, run_id, trial_number) |>
    select(dataset, task_id, user_id, run_id, trial_id, trial_number,
           item_uid, item_group, item, correct, rt, rt_numeric,
           timestamp = server_timestamp)
}

#' @export
get_runs <- function(dataset_spec,
                     remove_incomplete_runs = TRUE, remove_invalid_runs = TRUE,
                     max_results = NULL) {

  runs <- get_datasets_data(dataset_spec, table_getter("runs", max_results))

  if (remove_invalid_runs) runs <- runs |> filter(valid_run)
  if (remove_incomplete_runs) runs <- runs |> filter(completed)

  runs |> select(user_id, run_id)
}

#' @export
get_surveys <- function(dataset_spec,
                        survey_types = c("caregiver", "student", "teacher"),
                        remove_incomplete = FALSE,
                        max_results = NULL) {

  where_str <- build_filter("survey_id", survey_types)
  query_str <- glue("SELECT * FROM survey_responses {where_str}") |> stringr::str_trim()

  surveys <- get_datasets_data(dataset_spec, query_getter(query_str, max_results))
  if (remove_incomplete) surveys <- surveys |> filter(is_complete)

  surveys |>
    add_survey_items() |>
    code_survey_data()
}

get_metadata_table <- function(table_name) {
  metadata <- redivis::redivis$organization("levante")$dataset("metadata")
  metadata$table(table_name)$to_tibble()
}

#' @export
get_trial_items <- function() {
  get_metadata_table("trial_items")
}

#' @export
get_survey_items <- function() {
  get_metadata_table("survey_items")
}
