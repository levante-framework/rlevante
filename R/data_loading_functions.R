#' Get data from one or more datasets
#'
#' @param dataset_names Character vector of dataset names to fetch.
#' @param org_name Redivis organization name (defaults to "levante")
#' @param tables Character vector of table names to fetch (default to all
#'   tables).
#' @param max_results Max number of records to load for each table (defaults to
#'   entire table).
#'
#' @return Nested list of datasets, each of which is a list whose names are
#'   table names and values are tibbles of data for that dataset + table.
#' @export
#'
#' @examples
#' dataset_names <- list(list(name = "ca_western_pilot:97mt", version = "v3_1"))
#' get_datasets(dataset_names, tables = "users", max_results = 5)
get_datasets <- function(dataset_names, org_name = "levante", tables = NULL,
                         max_results = NULL) {

  # get reference to organization
  org <- redivis::organization(org_name)

  # TODO: make robust to flat list (i.e. only one dataset)
  # get reference to each dataset in dataset_names
  datasets <- dataset_names |>
    set_names(map_chr(dataset_names, \(dn) dn[["name"]])) |>
    map(\(dn) org$dataset(name = dn$name, version = dn$version))

  # fetch each dataset to populate its properties
  walk(datasets, \(ds) ds$get())

  # get each dataset's canonical reference (name + persistent ID + version)
  dataset_refs <- datasets |> map(\(ds) ds$scoped_reference)

  # construct list of table names to fetch for a given dataset
  # use "tables" if provided, otherwise list all tables for dataset
  get_table_names <- \(ds) {
    if (!is.null(tables)) tables else ds$list_tables() |> map(\(t) t$name)
  }

  # get data for a given dataset and its name
  get_dataset_tables <- \(ds, dn) {
    ds |> get_table_names() |> set_names() |> map(\(tn) {
      message(glue::glue("Fetching data for dataset {dn} table {tn}"))
      ds_table <- ds$table(tn)$to_tibble(max_results = max_results)
      ds_table |> mutate(dataset = dataset_refs[[dn]],
                         .before = everything()) # add column with reference
    })
  }

  # get data for each dataset
  imap(datasets, get_dataset_tables)
}

#' Combine tables across datasets
#'
#' Transforms a nested list of datasets, each of which is a list whose names are
#' table names and values are tibbles of data for that dataset + table, into a
#' flat list whose names are table names and values are tibbles of data for that
#' table, combined across datasets.
#'
#' @param dataset_tables List of lists of tibbles as returned by get_datasets().
#' @return List of tibbles.
#'
#' @export
#' @examples
#' ds <- get_datasets(dataset_names = c("ca_western_pilot", "de_leipzig_pilot"),
#'              tables = c("users", "user_groups"), max_results = 5)
#' ds_combined <- combine_datasets(ds)
combine_datasets <- function(dataset_tables) {

  # get list of all table names present in any datasets
  all_table_names <- map(dataset_tables, names) |> unlist() |> unique()

  dataset_tables |>
    # transpose datasets > tables into tables > datasets
    list_transpose(template = all_table_names, simplify = FALSE) |>
    # combines across datasets
    map(\(dt) list_rbind(dt)) |> #, names_to = "dataset")) |>
    # remove error messages
    map(\(ds) ds |> select(-matches("validation_err_msg")))
}

#' Collect user data
#'
#' Calculate each user's age(s) and collect their user groups.
#'
#' @param dataset_data List of tibbles as returned by combine_datasets().
#'
#' @return Users table from dataset_data, with additional list columns with
#'   tibbles of their user groups and their age at each run.
#'
#' @export
#' @examples
#' ds <- get_datasets(dataset_names = "ca_western_pilot",
#'                    tables = c("users", "groups", "user_groups", "runs"),
#'                    max_results = 10)
#' ds_combined <- combine_datasets(ds)
#' ds_users <- collect_users(ds_combined)
collect_users <- function(dataset_data) {

  # TODO: check that all needed tables are in data (users, groups, user_groups, runs)
  # TODO: don't error if all birth years/months are missing

  births <- dataset_data$users |>
    dplyr::select(.data$user_id, .data$birth_month, .data$birth_year) |>
    mutate(across(contains("birth"), \(v) na_if(v, 0))) |>
    filter(!is.na(.data$birth_month), !is.na(.data$birth_year)) |>
    mutate(dob = lubridate::ym(paste(.data$birth_year, .data$birth_month, sep = "-")))

  ages <- dataset_data$runs |>
    select(.data$user_id, .data$run_id, .data$time_started) |>
    inner_join(births) |>
    mutate(age = as.numeric(
      difftime(.data$time_started, .data$dob, units = "days")) / 365.25
    )

  user_ages <- ages |>
    select(.data$user_id, .data$run_id, .data$age) |>
    nest(ages = -.data$user_id)

  groups <- distinct(dataset_data$groups) |>
    select(.data$group_id, group_name = .data$name,
           group_abbreviation = .data$abbreviation)

  user_groups <- distinct(dataset_data$user_groups) |>
    select(.data$user_id, .data$group_id) |>
    left_join(groups, by = "group_id") |>
    tidyr::nest(groups = -.data$user_id)

  dataset_data$users |>
    left_join(user_groups, by = "user_id") |>
    left_join(user_ages, by = "user_id")
}

#' Remove practice trials from trial data
#'
#' @param trials Tibble of trial data as returned by combine_datasets().
#'
#' @return Trials tibble with any non-test trials removed (instructions,
#'   practice, training).
#' @export
#'
#' @examples
#' ds <- get_datasets("ca_western_pilot", tables = "trials", max_results = 5)
#' ds_combined <- combine_datasets(ds)
#' ds_trials <- remove_practice_trials(ds_combined$trials)
remove_practice_trials <- function(trials) {
  trials |>
    mutate(practice = .data$is_practice_trial |
             str_detect(.data$assessment_stage, "practice") |
             str_detect(.data$assessment_stage, "instructions") |
             str_detect(.data$corpus_trial_type, "training") |
             str_detect(.data$corpus_trial_type, "practice")) |>
    filter(is.na(.data$practice) | !.data$practice) |>
    select(-.data$practice, -.data$is_practice_trial)
}
