context("touchstone_demographic_dataset")

# Here, we do tests on:

# touchstone_demographic_dataset.csv
# Cols: touchstone,demographic_source,demographic_statistic_type
#       (all strings, not numerical ids)

test_that("A new touchstone demographic dataset - existing touchstone", {
  test <- new_test()
  standard_disease_touchstones(test)
  demog <- standard_demography(test)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  res <- do_test(test)

  new_dset <- DBI::dbGetQuery(res$con, "
    SELECT * FROM touchstone_demographic_dataset
     WHERE demographic_dataset = $1", demog$dset_id)

  expect_equal(nrow(new_dset), 1)
  expect_equal(new_dset$touchstone, "nevis-1")
})

test_that("DB Serial Corruption is detected", {
  test <- new_test()
  demog <- standard_demography(test)
  DBI::dbExecute(test$con, "INSERT INTO demographic_source
                 (id, code, name) VALUES ($1,$2,$3)",
    list(demog$source_id + 5, "S2", "Source 2"))

  expect_error(do_test(test),
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
