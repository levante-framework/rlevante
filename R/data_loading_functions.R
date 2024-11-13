#' Get redivis datasets
#'
#' Retrieve LEVANTE project datasets from Redivis and prepare them for further analysis.
#'
#' @return A list of one or more datasets from a specific organization in the Redivis repository. In our case that is normally "levante"
#' @export
#' @examples

# remember to specify at least one table in addition to the dataset name(s)
get_datasets <- function(dataset_names, org_name = "levante", tables = NULL) {
  org <- redivis::organization(org_name)

  datasets <- dataset_names |> rlang::set_names() |> purrr::map(\(dn) org$dataset(dn))

  get_table_names <- \(ds) if (!is.null(tables)) tables else ds$list_tables() |> purrr::map(\(t) t$name)

  get_dataset_tables <- \(ds) ds |> get_table_names() |> rlang::set_names() |> purrr::map(\(tn) ds$table(tn)$to_tibble())

  purrr::map(datasets, get_dataset_tables)
}

#' Fix some stuff in tables
#'
#' There can be some anomalies in LEVANTE data stored in Redivis. This function
#' will clean up some of the most common
#'
#' @return A Levante specific function to clean up some known potential data issues.
#' @export
#' @examples

fix_table_types <- function(table_data) {
  table_data |>
    dplyr::mutate(dplyr::across(dplyr::where(rlang::is_character),
                         \(x) x |> dplyr::na_if("null") |> dplyr::na_if("None"))) |>
    dplyr::mutate(dplyr::across(dplyr::matches("birth_"), as.integer),
                  dplyr::across(dplyr::matches("difficulty"), as.double),
                  dplyr::across(dplyr::matches("rt"), as.character),
                  dplyr::across(dplyr::matches("email_verified|is_reliable|is_bestrun"), as.logical))
}

#' Combine tables into a single megatable
#'
#' longer description
#'
#' @return For the case where the data you're using is in multiple tables, this
#'  function will combine them into one single large table if needed.
#'
#' @export
#' @examples

combine_datasets <- function(dataset_tables) {
  all_table_names <- purrr::map(dataset_tables, names) |> unlist() |> unique()
  dataset_tables |>
    purrr::map(\(ds) ds |> purrr::map(\(t) fix_table_types(t))) |>
    purrr::list_transpose(template = all_table_names, simplify = FALSE) |>
    purrr::map(list_rbind)
  # map(\(dt) list_rbind(dt, names_to = "dataset_name"))
}

#' Collect user data
#'
#' longer description
#'
#' @return Assemble & join user data with groups
#'
#' @export
#' @examples

collect_users <- function(dataset_data) {
  dplyr::distinct(dataset_data$users) |>
    # Using left_join keeps all the data in $user_groups
    # while connecting them to groups. Using many-to-many
    # ensures that we can have users in multiple groups,
    # as well as (of course) many users in a group
    dplyr::left_join(dplyr::distinct(dataset_data$user_groups),
                     by = "user_id", relationship = "many-to-many") |>
    dplyr::left_join(dplyr::distinct(dataset_data$groups),
                     by = "group_id", suffix = c("_user", "_group"))
}
