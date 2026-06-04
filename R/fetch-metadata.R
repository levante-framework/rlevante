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

#' Get item parameters
#'
#' `get_item_parameters()` returns the IRT item parameters used in LEVANTE scoring as a data frame. See the [rlevante documentation](https://levante-framework.github.io/rlevante/index.html) for more information about how to access LEVANTE datasets and codebooks.
#'
#' This table reports `difficulty` and `discrimination` only; it **omits the
#' guessing parameter** `g`. The deployed models use a fixed guessing lower
#' asymptote at each item's chance level, so to reconstruct an item response
#' function you must include it (`P = g + (1 - g) * plogis(a * (theta - b))`).
#' The authoritative, complete parameter set is `model_vals(record)`
#' (`mirt::mod2values()`), which includes `g`; see
#' `vignette("scoring-and-model-registry")`.
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
