context("scenario_description")

# Here, we do tests on:

# scenario_description.csv
# Cols: id, description, disease

test_scenario_description <- function(test_name, no_con = FALSE, ...) {
  con <- NULL
  if (!no_con) con <- test_db_connection()
  path <- test_path("sd", test_name)
  test_prepare(path, con)
  c(test_run_import(path, con, ...), con = con)
}

test_that("A new scenario_description", {
  res <- test_scenario_description("add_sd")
  test_compare_csv_db(res$con, res$e$scenario_description_csv,
                      "scenario_description")
})

test_that("Invalid disease", {
  expect_error(test_scenario_description("invalid_disease"),
               "Diseases in scenario_description are valid isn't true",
               class = "expectation_failure")
})

test_that("Identical scenario_description", {
  res <- test_scenario_description("identical_sd")
  test_compare_csv_db(res$con, res$e$scenario_description_csv,
                      "scenario_description")
})

test_that("Duplicate ids in scenario_description.csv", {
  expect_error(test_scenario_description("duplicate_id"),
               "Duplicate ids in scenario_description.csv isn't false.",
               class = "expectation_failure")
})

test_that("Edit scenario description for in-prep touchstone", {
  res <- test_scenario_description("edit_in_prep")
  test_compare_csv_db(res$con, res$e$scenario_description_csv,
                      "scenario_description")
})

test_that("Edit scenario description for open touchstone", {
  res <- test_scenario_description("edit_not_in_prep",
           allow_overwrite_scenario_description = TRUE)
  test_compare_csv_db(res$con, res$e$scenario_description_csv,
                      "scenario_description")

  expect_error(test_scenario_description("edit_not_in_prep",
           allow_overwrite_scenario_description = FALSE),
    "Can't edit scenario_description with id hot_chocolate. Already exists with open/finished touchstone versions.")
})

test_that("Edit scenario description for unused scenario description", {
  res <- test_scenario_description("edit_unused")
  test_compare_csv_db(res$con, res$e$scenario_description_csv,
                      "scenario_description")
})

test_that("Invalid columns", {
  expect_error(test_scenario_description("invalid_columns"),
               "Column names correct in scenario_description.csv isn't true",
               class = "expectation_failure")
})
