context("burden_estimate_expectation")

# Here, we do tests on:

# burden_estimate_expectation.csv
# Cols: year_min_inclusive, year_max_inclusive
#       age_min_inclusive, age_max_inclusive
#       cohort_min_inclusive, cohort_max_inclusive
#       description, version
#

test_responsibilities <- function(test_name, ...) {
  con <- test_db_connection()
  path <- test_path("resp", test_name)
  test_prepare(path, con)
  c(test_run_import(path, con, ...), con = con)
}

test_that("New expectation, existing touchstone name", {
  res <- test_responsibilities("add_resp")
})
