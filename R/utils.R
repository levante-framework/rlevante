# compute age in years from birth month/year and date of test
compute_age <- function(birth_month, birth_year, test_date) {
  # if (any(is.na(c(birth_month, birth_year, test_date)))) return(NA)
  birth_date <- lubridate::ymd(paste(birth_year, birth_month, 15, sep = "-"), quiet = TRUE)
  age_days <- as.numeric(difftime(test_date, birth_date, units = "days"))
  round(age_days / 365.25, 1)
}

#' Determine dataset of current notebook on Redivis
#' @keywords internal
#'
#' @param redivis Redivis reference object.
#' @export
notebook_dataset <- function(redivis) {
  source_table <- redivis$table("_source_")$get()
  source_dataset <- source_table$properties$container$scopedReference
  dataset_spec <- list(list(name = source_dataset, version = "current"))
}
