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
