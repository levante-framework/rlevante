#' Get redivis datasets
#'
#' longer description
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @export
#' @examples

get_datasets <- function(dataset_names, org_name = "levante", tables = NULL) {
  org <- redivis::organization(org_name)

  datasets <- dataset_names |> rlang::set_names() |> purrr::map(\(dn) org$dataset(dn))

  get_table_names <- \(ds) if (!is.null(tables)) tables else ds$list_tables() |> purrr::map(\(t) t$name)

  get_dataset_tables <- \(ds) ds |> get_table_names() |> rlang::set_names() |> purrr::map(\(tn) ds$table(tn)$to_tibble())

  purrr::map(datasets, get_dataset_tables)
}

#' Fix some stuff in tables
#'
#' longer description
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
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
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @export
#' @examples

combine_datasets <- function(dataset_tables) {
  all_table_names <- purrr::map(dataset_tables, names) |> unlist() |> unique()
  dataset_tables |>
    purrr::map(\(ds) ds |> purrr::map(\(t) fix_table_types(t))) |>
    purrr::list_transpose(template = all_table_names) |>
    purrr::map(purrr::list_rbind)
  # map(\(dt) list_rbind(dt, names_to = "dataset_name"))
}

#' Collect user data
#'
#' longer description
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @export
#' @examples

collect_users <- function(dataset_data) {
  dplyr::distinct(dataset_data$users) |>
    dplyr::left_join(dplyr::distinct(dataset_data$user_groups),
                     by = "user_id", relationship = "many-to-many") |>
    dplyr::left_join(dplyr::distinct(dataset_data$groups),
                     by = "group_id", suffix = c("_user", "_group"))
}
