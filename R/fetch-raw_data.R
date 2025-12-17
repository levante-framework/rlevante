#' Get raw table
#'
#' @inheritParams get_participants
#' @param table_name String indicating name of table to get
#'
#' @export
#' @examples
#' \dontrun{
#' runs <- get_raw_table(table_name = "runs", dataset = "levante-data-example:bm7r")
#' }
get_raw_table <- function(table_name, data_source, version = "current") {
  dataset_spec <- list(list(dataset = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter(table_name))
}
