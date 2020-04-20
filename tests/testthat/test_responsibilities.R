context("responsibility")

# Here, we do tests on:

# burden_estimate_expectation.csv
# Cols: year_min_inclusive, year_max_inclusive
#       age_min_inclusive, age_max_inclusive
#       cohort_min_inclusive, cohort_max_inclusive
#       description, version
#

create_responsibilities <- function(test, modelling_group, touchstone, scenario,
                                    age_min_inclusive, age_max_inclusive,
                                    cohort_min_inclusive, cohort_max_inclusive,
                                    year_min_inclusive, year_max_inclusive,
                                    countries, outcomes, description, db = FALSE) {
  write.csv(
    data_frame(
      modelling_group = modelling_group,
      touchstone = touchstone,
      scenario = scenario,
      age_min_inclusive = age_min_inclusive,
      age_max_inclusive = age_max_inclusive,
      cohort_min_inclusive = cohort_min_inclusive,
      cohort_max_inclusive = cohort_max_inclusive,
      year_min_inclusive = year_min_inclusive,
      year_max_inclusive = year_max_inclusive,
      countries = countries,
      outcomes = outcomes,
      description = description),
    file.path(test$path, "meta",
      db_file(db, "responsibilities.csv")),
    row.names = FALSE
  )
}


test_responsibilities <- function(test_name, ...) {
  con <- test_db_connection()
  path <- test_path("resp", test_name)
  test_prepare(path, con)
  c(test_run_import(path, con, ...), con = con)
}

test_that("New responsibility", {
  test <- new_test()
  standard_disease_touchstones(test)
  create_responsibilities(test, "LAP-elf", "nevis-1", "hot_chocolate", 0, 100, 1900, 2100, 2000, 2100,
                          "AFG", "cases", "FLU:LAP-elf:standard", FALSE)
  do_test(test)


})
