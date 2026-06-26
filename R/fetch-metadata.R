fetch_metadata_table <- function(table_name) {
  metadata <- redivis::redivis$organization("levante")$dataset("levante_metadata_items:czjv")
  message(glue::glue("Fetching item metadata from {table_name}"))
  suppressWarnings(
    metadata$table(table_name)$to_tibble()
  )
}

#' Get item UIDs mapped by trial_id
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' mapping_trial <- fetch_item_mapping_trial()
#' }
fetch_item_mapping_trial <- function() {
  fetch_metadata_table("item_mapping_trial:hjas")
}

#' Get item UIDs mapped by fields
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' mapping_fields <- fetch_item_mapping_fields()
#' }
fetch_item_mapping_fields <- function() {
  fetch_metadata_table("item_mapping_fields:6v86")
}

#' Get item UIDs mapped by item_id
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' mapping_id <- fetch_item_mapping_id()
#' }
fetch_item_mapping_id <- function() {
  fetch_metadata_table("item_mapping_id:tma7")
}

#' Get metadata for corpus items
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' corpus_items <- fetch_corpus_items()
#' }
fetch_corpus_items <- function() {
  fetch_metadata_table("corpus_items:ezfc")
}

#' Get metadata for survey items
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' survey_items <- fetch_survey_items()
#' }
fetch_survey_items <- function() {
  fetch_metadata_table("survey_items:tfw0") |>
    arrange(.data$survey_type, .data$variable_order)
}

#' Get item parameters
#'
#' `get_item_parameters()` returns the IRT item parameters used in LEVANTE scoring as a data frame. See the [rlevante documentation](https://levante-framework.github.io/rlevante/index.html) for more information about how to access LEVANTE datasets and codebooks.
#' @param version Version of the Redivis scoring metadata dataset.
#' @returns A data frame where each row is an item parameter estimate. See our [Scoring and Psychometrics page](https://researcher.levante-network.org/measures/scoring-and-psychometrics) to learn how to interpret these values.
#' @export
#' @examples
#' \dontrun{
#' item_parameters <- get_item_parameters()
#' }
get_item_parameters <- function(version = "current") {
  ds <- redivis::redivis$organization("levante")$dataset("levante_metadata_scoring:e97h", version = version)
  message(glue::glue("Fetching item parameters"))
  suppressWarnings(
    ds$table("item_parameters:4cvk")$to_tibble()
  )
}
