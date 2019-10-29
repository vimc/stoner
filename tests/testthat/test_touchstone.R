context("touchstone")

# Here, we do tests with these two CSV files...

# touchstone.csv
# Cols: id, touchstone_name, version, description, status, comment

# touchstone_name.csv
# Cols: id, description, comment

test_touchstone <- function(test_name, no_con = FALSE) {
  con <- NULL
  if (!no_con) con <- test_db_connection()
  path <- test_path("touchstone", test_name)
  test_prepare(path, con)
  c(test_run_import(path, con), con = con)
}

test_that("Empty import should succeed trivially without needing db", {
  res <- test_touchstone("empty", no_con = TRUE)
  expect_equal(length(res$e), 0)
  expect_equal(length(res$t), 0)
})

test_that("A new touchstone and name", {
  res <- test_touchstone("new_touchstone_and_name")
  test_compare_csv_db(res$con, res$e$touchstone_csv, "touchstone")
  test_compare_csv_db(res$con, res$e$touchstone_name_csv, "touchstone_name")
})

test_that("Two new touchstones and names", {
  res <- test_touchstone("two_touchstones_and_names")
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_csv, "touchstone"))
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_name_csv, "touchstone_name"))
})

test_that("Two new touchstones, one touchstone name", {
  res <- test_touchstone("two_touchstones_one_name")
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_csv, "touchstone"))
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_name_csv, "touchstone_name"))
})

test_that("New touchstone, touchstone name in db, not csv", {
  res <- test_touchstone("new_touchstone_existing_name")
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_csv, "touchstone"))
})

test_that("Update existing touchstone_name (no refs)", {
  res <- test_touchstone("update_touchstone_name_no_refs")
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_name_csv, "touchstone_name"))
})

test_that("Update existing touchstone_name (in prep)", {
  res <- test_touchstone("update_touchstone_name_in_prep")
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_name_csv, "touchstone_name"))
})

test_that("Update existing touchstone details (in prep)", {
  res <- test_touchstone("update_touchstone_in_prep")
   expect_true(test_compare_csv_db(res$con, res$e$touchstone_csv, "touchstone"))
})
