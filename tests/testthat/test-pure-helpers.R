# Unit tests for small pure helpers used across the package.

test_that("compute_age() returns age in years from birth month/year", {
  # born June 2018 (assumed day 15), tested exactly six years later
  expect_equal(compute_age(6, 2018, as.Date("2024-06-15")), 6.0)
  # half a year later than birth midpoint
  expect_equal(compute_age(1, 2020, as.Date("2020-07-15")), 0.5)
  # missing inputs propagate to NA (parsed quietly)
  expect_true(is.na(compute_age(NA, 2018, as.Date("2024-06-15"))))
})

test_that("build_filter() builds SQL WHERE clauses", {
  expect_equal(build_filter("task_id", NULL), "")
  expect_equal(as.character(build_filter("task_id", "math")),
               "WHERE task_id IN ('math')")
  expect_equal(as.character(build_filter("task_id", c("math", "vocab"))),
               "WHERE task_id IN ('math', 'vocab')")
  expect_equal(
    as.character(build_filter("survey_type", "caregiver", allow_null = TRUE)),
    "WHERE survey_type IS NULL OR survey_type IN ('caregiver')"
  )
})

test_that("reverse_value() reverses within an ordered scale", {
  scale <- 1:5
  expect_equal(reverse_value(2, scale), 4)
  expect_equal(reverse_value(1, scale), 5)
  expect_equal(reverse_value(3, scale), 3)
  # value not on the scale, or a scale with NA -> NA
  expect_true(is.na(reverse_value(9, scale)))
  expect_true(is.na(reverse_value(2, c(1, NA, 3))))
})
