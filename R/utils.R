# compute age in years from birth month/year and date of test
compute_age <- function(birth_month, birth_year, test_date) {
  birth_date <- lubridate::ym(paste(birth_year, birth_month, sep = "-"))
  age_days <- as.numeric(difftime(test_date, birth_date, units = "days"))
  age_days / 365.25
}
