context("touchstone")

# Here, we do tests with these two CSV files...

# touchstone.csv
# Cols: id, touchstone_name, version, description, status, comment

# touchstone_name.csv
# Cols: id, description, comment

test_that("Empty import should succeed trivially without needing db", {
  path <- test_path("touchstone", "empty")
  test_prepare(path)
  expect_silent(test_run_import(path))
})


test_that("A new touchstone and name", {
  con <- test_db_connection()
  path <- test_path("touchstone", "new_touchstone_and_name")
  test_prepare(path, con)
  res <- test_run_import(path, con)
  test_compare_csv_db(con, res$e$touchstone_csv, "touchstone")
  test_compare_csv_db(con, res$e$touchstone_name_csv, "touchstone_name")
})

test_that("Two new touchstones and names", {
  con <- test_db_connection()
  path <- test_path("touchstone", "two_touchstones_and_names")
  test_prepare(path, con)
  res <- test_run_import(path, con)
  e <-expect_true(test_compare_csv_db(con, res$e$touchstone_csv, "touchstone"))
  expect_true(test_compare_csv_db(con, res$e$touchstone_name_csv, "touchstone_name"))
})

test_that("Two new touchstones, one touchstone name", {
  con <- test_db_connection()
  path <- test_path("touchstone", "two_touchstones_one_name")
  test_prepare(path, con)
  res <- test_run_import(path, con)
  expect_true(test_compare_csv_db(con, res$e$touchstone_csv, "touchstone"))
  expect_true(test_compare_csv_db(con, res$e$touchstone_name_csv, "touchstone_name"))
})


# Tests:
# 1. touchstone_csv id is not in database, and gets added.
# 2. touchstone_csv id is in database, is in-preparation and an edit occurs
# 3. touchstone_csv id is in database, is not in-preparation - > error

