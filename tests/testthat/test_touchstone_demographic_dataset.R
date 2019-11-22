context("touchstone_demographic_dataset")

# Here, we do tests on:

# touchstone_demographic_dataset.csv
# Cols: touchstone,demographic_source,demographic_statistic_type
#       (all strings, not numerical ids)

test_touchstone_demographic_dataset <- function(test_name, ...) {
  con <- test_db_connection()
  path <- test_path("tdd", test_name)
  test_prepare(path, con)
  c(test_run_import(path, con, ...), con = con)
}

compare_tdd_db_with_csv <- function(con, test_name) {
  path <- test_path("tdd", test_name)
  expected <- read_meta(path, "expected_result.csv")
  db_tdd <- db_tdd_for_touchstones(con, unique(expected$touchstone))
  db_tdd$mash <- mash(db_tdd, c("dsource_code", "dtype_code"))
  expected$mash <- mash(expected, c("demographic_source",
                                    "demographic_statistic_type"))
  match(expected$mash, db_tdd$mash)

  expect_equal(nrow(expected), nrow(db_tdd))
  expect_true(all(sort(db_tdd$mash) == sort(expected$mash)))
}

test_that("A new touchstone demographic dataset - existing touchstone", {
  res <- test_touchstone_demographic_dataset("new_tdd")
  compare_tdd_db_with_csv(res$con, "new_tdd")
})

test_that("DB Serial Corruption is detected", {
  con <- test_db_connection()
  path <- test_path("tdd", "new_tdd")
  test_prepare(path, con)
  df <- data_frame(id = 2, code = 'source2', name = 'Name 2')
  DBI::dbWriteTable(con, "demographic_source", df, append = TRUE)
  expect_error(test_run_import(path, con),
               "Error - db serial numbers were corrupted")
})


test_that("A new touchstone demographic dataset - touchstone in same import", {
  res <- test_touchstone_demographic_dataset("new_tdd_with_touchstone")
  compare_tdd_db_with_csv(res$con, "new_tdd")
})

test_that("Touchstone demographic dataset - touchstone not exist", {
  expect_error(test_touchstone_demographic_dataset("new_tdd_no_touchstone"),
               "Touchstones in touchstone_demographic_dataset exist isn't true.",
               class = "expectation_failure")
})

test_that("Touchstone demographic dataset - demographic source not exist", {
  expect_error(test_touchstone_demographic_dataset("new_tdd_no_dsource"),
               "demographic sources in touchstone_demographic_dataset exist isn't true.",
               class = "expectation_failure")
})

test_that("Touchstone demographic dataset - demographic stat type not exist", {
  expect_error(test_touchstone_demographic_dataset("new_tdd_no_dstype"),
               "demog. statistic types in touchstone_demographic_dataset exist isn't true.",
               class = "expectation_failure")
})

test_that("Touchstone demographic dataset - demographic dataset not exist", {
  expect_error(test_touchstone_demographic_dataset("new_tdd_no_dataset"),
               "Demographic datasets already exist isn't true.",
               class = "expectation_failure")
})

test_that("Touchstone demographic dataset - invalid csv format", {
  expect_error(test_touchstone_demographic_dataset("invalid_cols"),
               "Incorrect columns in touchstone_demographic_dataset.csv",
               class = "simpleError")
  expect_error(test_touchstone_demographic_dataset("invalid_cols2"),
               "Incorrect columns in touchstone_demographic_dataset.csv",
               class = "simpleError")
  expect_error(test_touchstone_demographic_dataset("invalid_cols3"),
               "demographic_source in touchstone_demographic_dataset.csv must be character",
               class = "simpleError")
  expect_error(test_touchstone_demographic_dataset("invalid_cols4"),
               "demographic_statistic_type in touchstone_demographic_dataset.csv must be character",
               class = "simpleError")
})

test_that("Duplicate of db entry", {
  res <- test_touchstone_demographic_dataset("new_tdd_dup")
  compare_tdd_db_with_csv(res$con, "new_tdd_dup")
})

test_that("Update in-prep", {
  res <- test_touchstone_demographic_dataset("update_tdd_inprep")
  compare_tdd_db_with_csv(res$con, "update_tdd_inprep")
})

test_that("Update non in-prep", {
  expect_error(test_touchstone_demographic_dataset("update_tdd_notinprep"),
               "Can't edit touchstone id (.*). Already exists with open/finished status.",
               class = "simpleError")
})
