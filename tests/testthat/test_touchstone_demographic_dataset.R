context("touchstone_demographic_dataset")

# Here, we do tests on:

# touchstone_demographic_dataset.csv
# Cols: touchstone,demographic_source,demographic_statistic_type
#       (all strings, not numerical ids)

test_dset <- function(test, dsets, tstones) {
  new_dset <- DBI::dbGetQuery(test$con, sprintf("
    SELECT * FROM touchstone_demographic_dataset
     WHERE demographic_dataset IN %s", sql_in(dsets)))
  expect_equal(nrow(new_dset), length(dsets))
  expect_true(all(sort(tstones) == sort(new_dset$touchstone)))
}

test_that("A new touchstone demographic dataset - existing touchstone", {
  test <- new_test()
  standard_disease_touchstones(test)
  demog <- standard_demography(test)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  test_dset(do_test(test), demog$dset_id, "nevis-1")
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
  test <- new_test()
  demog <- standard_demography(test)
  standard_disease_touchstones(test, db = FALSE)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  test_dset(do_test(test), demog$dset_id, "nevis-1")
})

test_that("Touchstone demographic dataset - touchstone not exist", {
  test <- new_test()
  standard_demography(test)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  expect_error(do_test(test),
    "Touchstones in touchstone_demographic_dataset exist isn't true.",
    class = "expectation_failure")
})

test_that("Touchstone demographic dataset - demographic source not exist", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  standard_demography(test, make_source = FALSE)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  expect_error(do_test(test),
    "demographic sources in touchstone_demographic_dataset exist isn't true.",
    class = "expectation_failure")
})

test_that("Touchstone demographic dataset - demographic stat type not exist", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  standard_demography(test, make_type = FALSE)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  expect_error(do_test(test),
    "demog. statistic types in touchstone_demographic_dataset exist isn't true.",
    class = "expectation_failure")
})

test_that("Touchstone demographic dataset - demographic dataset not exist", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  standard_demography(test, make_dataset = FALSE)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  expect_error(do_test(test),
    "Demographic datasets already exist isn't true.",
    class = "expectation_failure")
})

test_that("Touchstone demographic dataset - extra column", {
  test <- new_test()
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  mess_with(test$path, "touchstone_demographic_dataset.csv",
                       "another", 1, "elf")
  expect_error(do_test(test),
    "Incorrect columns in touchstone_demographic_dataset.csv",
    class = "simpleError")
})

test_that("Touchstone demographic dataset - misnamed column", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  standard_demography(test)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  mess_with(test$path, "touchstone_demographic_dataset.csv",
                       "touchstone", 0, "z")
  expect_error(do_test(test),
    "Incorrect columns in touchstone_demographic_dataset.csv",
    class = "simpleError")
})

test_that("Touchstone demographic dataset - numeric source", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  standard_demography(test)
  create_ts_dds(test$path, "nevis-1", 1, "T1")
  expect_error(do_test(test),
    "demographic_source in touchstone_demographic_dataset.csv must be character",
    class = "simpleError")
})

test_that("Touchstone demographic dataset - numeric type", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  standard_demography(test)
  create_ts_dds(test$path, "nevis-1", "S1", 1)
  expect_error(do_test(test),
    "demographic_statistic_type in touchstone_demographic_dataset.csv must be character",
    class = "simpleError")
})

test_that("Duplicate of db entry", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  demog <- standard_demography(test)

  # Bit messy this one - have to mimic what's in the db table,
  # which has integer demographic_dataset

  write.csv(
    data_frame(touchstone = "nevis-1",
               demographic_dataset = demog$dset_id),
    file.path(test$path, "meta", "db_touchstone_demographic_dataset.csv"),
    row.names = FALSE)

  create_ts_dds(test$path, "nevis-1", "S1", "T1", db = FALSE)
  test_dset(do_test(test), demog$dset_id, "nevis-1")
})

test_that("Update in-prep", {
  test <- new_test()
  create_disease_csv(test$path, "flu", "Elf flu", db = TRUE)
  create_touchstone_csv(test$path, "nevis", c(1, 2), db = TRUE)
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)

  demog <- standard_demography(test, rows = 2)
  write.csv(
    data_frame(touchstone = "nevis-1",
               demographic_dataset = demog$dset_id[1]),
    file.path(test$path, "meta", "db_touchstone_demographic_dataset.csv"),
    row.names = FALSE)

  create_ts_dds(test$path, "nevis-2", "S2", "T2", db = FALSE)
  test_dset(do_test(test), demog$dset_id[2], "nevis-2")
})

test_that("Update non in-prep", {
  test <- new_test()
  create_disease_csv(test$path, "flu", "Elf flu", db = TRUE)
  create_touchstone_csv(test$path, "nevis", c(1, 2),
                        status = "finished",db = TRUE)
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)

  demog <- standard_demography(test, rows = 2)
  write.csv(
    data_frame(touchstone = "nevis-1",
               demographic_dataset = demog$dset_id[1]),
    file.path(test$path, "meta", "db_touchstone_demographic_dataset.csv"),
    row.names = FALSE)

  create_ts_dds(test$path, "nevis-2", "S2", "T2", db = FALSE)

  expect_error(do_test(test),
    "Can't edit touchstone id (.*). Already exists with open/finished status.",
    class = "simpleError")
})
