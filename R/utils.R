# validate_birth_month <- function(birth_month) {
#   if_else(birth_month >= 1 & birth_month <= 12, birth_month, NA)
# }
#
# validate_birth_year <- function(birth_year) {
#   if_else(birth_year >= 2000 & birth_year <= 2050, birth_year, NA)
# }
#
# # compute age in years from birth month/year and date of test
# compute_age <- function(birth_month, birth_year, test_date) {
#   # if (any(is.na(c(birth_month, birth_year, test_date)))) return(NA)
#   birth_date <- lubridate::ym(paste(birth_year, birth_month, sep = "-"), quiet = TRUE)
#   age_days <- as.numeric(difftime(test_date, birth_date, units = "days"))
#   round(age_days / 365.25, 1)
# }

#' Determine dataset of current notebook on Redivis
#'
#' @param redivis Redivis reference object.
#'
#' @export
notebook_dataset <- function(redivis) {
  source_table <- redivis$table("_source_")$get()
  source_dataset <- source_table$properties$container$scopedReference
  dataset_spec <- list(list(name = source_dataset, version = "current"))
}
