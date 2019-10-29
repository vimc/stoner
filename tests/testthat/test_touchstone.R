context("touchstone")

# Here, we do tests on  these two CSV files...

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

test_that("Update and add touchstone and touchstone_name (in prep)", {
  res <- test_touchstone("update_and_add_in_prep")
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_csv, "touchstone"))
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_name_csv, "touchstone_name"))
})

test_that("Exact match", {
  res <- test_touchstone("exact_match")
  expect_equal(nrow(res$t$touchstone), 0)
  expect_equal(nrow(res$t$touchstone_name), 0)
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_csv, "touchstone"))
  expect_true(test_compare_csv_db(res$con, res$e$touchstone_name_csv, "touchstone_name"))
})

## Tests that should fail:

test_that("Add touchstone, name not found", {
  expect_error(test_touchstone("add_touchstone_no_touchstone_name"),
    "All touchstone.touchstone_name are known isn't true",
    class = "expectation_failure")
})

test_that("Add touchstone, bad version", {
  expect_error(test_touchstone("add_touchstone_bad_version"),
    "All touchstone.description are formatted correctly isn't true",
               class = "expectation_failure")
})

test_that("Add touchstone, bad id format", {
  expect_error(test_touchstone("add_touchstone_bad_id_format"),
               "All touchstone.id are touchstone_name-version isn't true",
               class = "expectation_failure")
})

test_that("Edit touchstone name - not in-preparation", {
  expect_error(test_touchstone("update_touchstone_name_not_prep"),
               "Can't edit touchstone_name id (.*). Already exists with open/finished touchstone versions",
               class = "simpleError")
})

test_that("Edit touchstone - not in-preparation", {
  expect_error(test_touchstone("update_touchstone_not_in_prep"),
               "Can't edit touchstone id (.*). Already exists with open/finished status.",
               class = "simpleError")
})

test_that("touchstone CSV invalid", {
  expect_error(test_touchstone("touchstone_csv_invalid"),
               "Correct columns in touchstone.csv not equal to (.*)",
               class = "expectation_failure")
})

test_that("touchstone_name CSV invalid", {
  expect_error(test_touchstone("touchstone_name_csv_invalid"),
               "Correct columns in touchstone_name.csv not equal to (.*)",
               class = "expectation_failure")
})

test_that("Duplicate touchstone id in csv", {
  expect_error(test_touchstone("duplicate_touchstone_id"),
               "No duplicate ids in touchstone.csv isn't false.",
               class = "expectation_failure")
})

test_that("Duplicate touchstone_name in csv", {
  expect_error(test_touchstone("duplicate_touchstone_name"),
               "No duplicate ids in touchstone_name.csv isn't false.",
               class = "expectation_failure")
})

test_that("Duplicate touchstone_name in csv", {
  expect_error(test_touchstone("duplicate_touchstone_name"),
               "No duplicate ids in touchstone_name.csv isn't false.",
               class = "expectation_failure")
})

test_that("Add touchstone, bad status", {
  expect_error(test_touchstone("add_bad_status"),
               "All touchstone.status are valid isn't true.",
               class = "expectation_failure")
})

test_that("Edit touchstone, bad status", {
  expect_error(test_touchstone("edit_bad_status"),
               "All touchstone.status are valid isn't true.",
               class = "expectation_failure")
})

