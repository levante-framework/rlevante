fetch_metadata_table <- function(table_name) {
  metadata <- redivis::redivis$organization("levante")$dataset("levante_metadata_items:czjv")
  message(glue::glue("Fetching item metadata for {table_name}"))
  suppressWarnings(
    metadata$table(table_name)$to_tibble()
  )
}

#' Get metadata for trial items
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' trial_items <- fetch_trial_items()
#' }
fetch_trial_items <- function() {
  fetch_metadata_table("trial_items:hjas")
}

#' Get metadata for mapping items
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' mapping_items <- fetch_mapping_items()
#' }
fetch_mapping_items <- function() {
  fetch_metadata_table("mapping_items:6v86")
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
