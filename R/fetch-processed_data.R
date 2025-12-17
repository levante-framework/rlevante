#' Get participants
#'
#' @param data_source Name of Redivis dataset
#' @param version Version of Redivis dataset
#'
#' @export
#' @examples
#' \dontrun{
#' participants <- get_participants(dataset = "levante-data-example:bm7r", version = "current")
#' }
get_participants <- function(data_source, version = "current") {
  dataset_spec <- list(list(name = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter("participants"))
}

#' Get scores
#'
#' @inheritParams get_participants
#'
#' @export
#' @examples
#' \dontrun{
#' scores <- get_scores(dataset = "levante-data-example:bm7r", version = "current")
#' }
get_scores <- function(data_source, version = "current") {
  dataset_spec <- list(list(name = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter("scores"))
}

#' Get surveys
#'
#' @inheritParams get_participants
#'
#' @export
#' @examples
#' \dontrun{
#' surveys <- get_surveys(dataset = "levante-data-example:bm7r", version = "current")
#' }
get_surveys <- function(data_source, version = "current") {
  dataset_spec <- list(list(name = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter("surveys"))
}

#' Get trials
#'
#' @inheritParams get_participants
#'
#' @export
#' @examples
#' \dontrun{
#' trials <- get_trials(dataset = "levante-data-example:bm7r", version = "current")
#' }
get_trials <- function(data_source, version = "current") {
  dataset_spec <- list(list(name = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter("trials"))
}

