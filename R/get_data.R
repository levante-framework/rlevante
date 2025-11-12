# given a table name, return a function that takes a dataset reference and
# returns data from the given table in that dataset
table_getter <- function(table_name, max_results = NULL) {
  \(dataset, dataset_table_names) {
    message(glue("--Fetching table {table_name}"))
    if (!(table_name %in% dataset_table_names)) return(tibble())
    suppressWarnings(
      dataset$table(table_name)$to_tibble(max_results = max_results)
    )
  }
}

# given a sql query, return a function that takes a dataset reference and
# returns data from executing that sql query in that dataset
query_getter <- function(table_name, query_str, max_results = NULL) {
  message(glue("--Executing SQL query"))
  \(dataset, dataset_table_names) {
    if (!(table_name %in% dataset_table_names)) return(tibble())
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
    dataset_table_names <- dataset$list_tables() |> map_chr(\(tbl) tbl$name)
    dataset_fun(dataset, dataset_table_names) |>
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
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' participants <- get_participants(dataset_spec)
#' }
get_participants <- function(dataset_spec, max_results = NULL) {

  # get data for the tables with needed user data
  user_tables <- c("groups", "users", "user_groups")
  user_data <- user_tables |> rlang::set_names() |>
    map(\(table_name) get_datasets_data(dataset_spec,
                                        table_getter(table_name, max_results)))

  # filter users to only children and replace invalid birth months/years with NA
  users <- user_data$users |>
    filter(.data$user_type %in% c("guest", "student")) |>
    mutate(birth_month = validate_birth_month(.data$birth_month),
           birth_year = validate_birth_year(.data$birth_year))

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
    select("dataset", "user_id", "birth_month", "birth_year", "sex", "grade",
           matches("_id"), "groups")
}

# construct SQL WHERE clause out of a variable name and vector of allowed values
build_filter <- function(var, vals) {
  vals_str <- glue("'{vals}'") |> paste(collapse = ", ")
  if (is.null(vals)) "" else glue("WHERE {var} IN ({vals_str})")
}

#' Get trial data
#'
#' @inheritParams get_trials
#'
#' @export
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' trials_prelim <- get_trials_prelim(dataset_spec)
#' }
get_trials_prelim <- function(dataset_spec,
                              remove_incomplete_runs = TRUE,
                              remove_invalid_runs = TRUE,
                              remove_invalid_trials = FALSE,
                              tasks = NULL, # all tasks if null
                              participants = NULL, # all participants if null
                              max_results = NULL) {

  where_str <- build_filter("task_id", tasks)
  query_str <- glue("SELECT * FROM trials {where_str}") |> str_trim()

  trials <- get_datasets_data(dataset_spec,
                              query_getter("trials", query_str, max_results))

  # if participants supplied, filter to trials for only those participants
  if (!is.null(participants)) {
    trials <- trials |> semi_join(participants, by = c("user_id", "dataset"))
  }

  # if run filters supplied, get corresponding runs and filter to their trials
  if (any(remove_incomplete_runs, remove_invalid_runs)) {
    runs <- get_runs(dataset_spec,
                     remove_incomplete_runs = remove_incomplete_runs,
                     remove_invalid_runs = remove_invalid_runs)
    trials <- trials |> semi_join(runs, by = c("run_id"))
  }

  # filter to valid trials
  if (remove_invalid_trials) trials <- trials |> filter(.data$valid_trial)

  trials |>
    filter(.data$trial_id != "schema_row") |>
    remove_practice_trials() |>
    # select("dataset", "task_id", "user_id", "run_id", "trial_id",
    #        "item_id", "item_uid", "answer", # item_original = "item",
    #        "response", "correct", "rt", "server_timestamp",
    #        "valid_trial", "validation_msg_trial") |>
    convert_rts() |>
    mutate(response = .data$response |> na_if("nan"))
  # add_trial_items() |>
    # add_trial_numbers() |>
    # arrange(.data$dataset, .data$task_id, .data$user_id, .data$run_id, .data$trial_number)
    # select("dataset", "task_id", "user_id", "run_id", "trial_id", "trial_number",
    #        "item_uid", "item_task", "item_group", "item", "chance", #item_id_original,
    #        "correct", "rt", "rt_numeric", "response", "answer",
    #        timestamp = "server_timestamp", "valid_trial", "validation_msg_trial")
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
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' trials <- get_trials(dataset_spec)
#' }
get_trials <- function(dataset_spec,
                       remove_incomplete_runs = TRUE,
                       remove_invalid_runs = TRUE,
                       remove_invalid_trials = FALSE,
                       tasks = NULL, # all tasks if null
                       participants = NULL, # all participants if null
                       max_results = NULL) {

  trials <- get_trials_prelim(dataset_spec = dataset_spec,
                              remove_incomplete_runs = remove_incomplete_runs,
                              remove_invalid_runs = remove_invalid_runs,
                              remove_invalid_trials = remove_invalid_trials,
                              tasks = tasks,
                              participants = participants,
                              max_results = max_results)
  trials |>
    add_item_ids() |>
    add_item_metadata() |>
    add_trial_numbers() |>
    arrange(.data$dataset, .data$task_id, .data$user_id, .data$run_id, .data$trial_number) |>
    select("dataset", "task_id", "user_id", "run_id", "trial_id",
           "trial_number", "item_uid", "item_task", "item_group", "item",
           "chance", "correct", "rt", "rt_numeric", "response", "item_original",
           "answer", "distractors", timestamp = "server_timestamp",
           "valid_trial", "validation_msg_trial")
}

#' Get raw trial data
#'
#' @inheritParams get_trials
#'
#' @export
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' trials_raw <- get_trials_raw(dataset_spec)
#' }
get_trials_raw <- function(dataset_spec) {
  get_datasets_data(dataset_spec, table_getter("trials"))
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
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' runs <- get_runs(dataset_spec)
#' }
get_runs <- function(dataset_spec,
                     remove_incomplete_runs = TRUE,
                     remove_invalid_runs = TRUE,
                     max_results = NULL) {

  run_vars <- c("run_id", "runs.user_id", "runs.task_id", "runs.task_version",
                "runs.variant_id", "variants.name AS variant_name",
                "variants.language",
                "runs.administration_id", "administrations.public_name AS administration_name",
                "test_comp_theta_estimate", "test_comp_theta_se",
                "time_started", "completed", "valid_run", "validation_msg_run")
  user_vars <- c("birth_month", "birth_year")
  query_str <- glue("SELECT {paste(c(run_vars, user_vars), collapse = ', ')}
                    FROM runs
                    LEFT JOIN users ON runs.user_id = users.user_id
                    LEFT JOIN variants ON runs.variant_id = variants.variant_id
                    LEFT JOIN administrations ON runs.administration_id = administrations.administration_id")

  # runs <- get_datasets_data(dataset_spec, table_getter("runs", max_results))
  runs <- get_datasets_data(dataset_spec,
                            query_getter("runs", query_str, max_results))

  runs <- runs |> filter(!(.data$task_id %in% c("intro", "schema_row")))
  if (remove_invalid_runs) runs <- runs |> filter(.data$valid_run)
  if (remove_incomplete_runs) runs <- runs |> filter(.data$completed)

  runs |>
    mutate(adaptive = .data$variant_name |>
             str_to_lower() |> str_detect("adaptive"),
           .after = .data$variant_name) |>
    # select(-name) |>
    mutate(birth_month = validate_birth_month(.data$birth_month),
           birth_year = validate_birth_year(.data$birth_year),
           age = compute_age(.data$birth_month, .data$birth_year, .data$time_started)) |>
    select(-c("birth_month", "birth_year")) |>
    arrange(.data$time_started)
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
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' surveys <- get_surveys(dataset_spec)
#' }
get_surveys <- function(dataset_spec,
                        survey_types = c("caregiver", "student", "teacher"),
                        remove_incomplete_surveys = FALSE,
                        max_results = NULL) {

  where_str <- build_filter("survey_id", survey_types)
  query_str <- glue("SELECT * FROM survey_responses {where_str}") |> str_trim()

  surveys <- get_datasets_data(dataset_spec,
                               query_getter("survey_responses", query_str, max_results))
  if (nrow(surveys) == 0) return()

  if (remove_incomplete_surveys) surveys <- surveys |> filter(.data$is_complete)

  surveys |>
    # mutate(survey_part = if_else(is.na(survey_part), survey_type, survey_part)) |>
    add_survey_items() |>
    code_survey_data()
}

get_metadata_table <- function(table_name) {
  metadata <- redivis::redivis$organization("levante")$dataset("item_metadata")
  message(glue::glue("Fetching item metadata for {table_name}"))
  suppressWarnings(
    metadata$table(table_name)$to_tibble()
  )
}

#' Get metadata for trial items
#'
#' @export
#' @examples
#' \dontrun{
#' trial_items <- get_trial_items()
#' }
get_trial_items <- function() {
  get_metadata_table("trial_items")
}

#' Get metadata for mapping items
#'
#' @export
#' @examples
#' \dontrun{
#' mapping_items <- get_mapping_items()
#' }
get_mapping_items <- function() {
  get_metadata_table("mapping_items")
}

#' Get metadata for corpus items
#'
#' @export
#' @examples
#' \dontrun{
#' corpus_items <- get_corpus_items()
#' }
get_corpus_items <- function() {
  get_metadata_table("corpus_items")
}

#' Get metadata for survey items
#'
#' @export
#' @examples
#' \dontrun{
#' survey_items <- get_survey_items()
#' }
get_survey_items <- function() {
  get_metadata_table("survey_items") |>
    arrange(.data$survey_type, .data$variable_order)
}

#' Get score data
#'
#' @inheritParams get_runs
#'
#' @export
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' scores <- get_scores(dataset_spec)
#' }
get_scores <- function(dataset_spec) {
  scores <- get_datasets_data(dataset_spec, table_getter("scores"))
}
