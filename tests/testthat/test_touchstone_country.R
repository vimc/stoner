context("touchstone_country")

# Here, we do tests on:

# touchstone_country.csv
# Cols: touchstone,countries,diseases

test_touchstone_country <- function(test_name, ...) {
  con <- test_db_connection()
  path <- test_path("tc", test_name)
  test_prepare(path, con)
  c(test_run_import(path, con, ...), con = con)
}

compare_tc_db_with_csv <- function(con, test_name) {
  path <- test_path("tc", test_name)
  expected <- read_meta(path, "expected_result.csv")
  db_tc <- DBI::dbGetQuery(con, sprintf("
    SELECT * FROM touchstone_country
    WHERE touchstone in %s", sql_in(unique(expected$touchstone))))

  db_tc$mash <- mash(db_tc, c("touchstone", "disease", "country"))
  expected$mash <- mash(expected, c("touchstone", "disease", "country"))
  expect_true(setequal(db_tc$mash, expected$mash))
}

test_that("Invalid columns", {
  expect_error(test_touchstone_country("invalid_columns"),
               "Invalid columns in touchstone_country.csv",
               class = "simpleError")
})

test_that("Invalid country in touchstone_country", {
  expect_error(test_touchstone_country("invalid_country"),
               "All countries in touchstone_country are recognised isn't true",
               class = "expectation_failure")
})

test_that("Invalid disease in touchstone_country", {
  expect_error(test_touchstone_country("invalid_disease"),
               "All diseases in touchstone_country are recognised isn't true",
               class = "expectation_failure")
})

test_that("Invalid touchstone in touchstone_country", {
  expect_error(test_touchstone_country("invalid_touchstone"),
               "All touchstones in touchstone_country are recognised isn't true",
               class = "expectation_failure")
})

test_that("Add touchstone_country - touchstone in db", {
  res <- test_touchstone_country("add_t_db")
  compare_tc_db_with_csv(res$con, "add_t_db")
})

test_that("Add touchstone_country - touchstone in csv", {
  res <- test_touchstone_country("add_t_csv")
  compare_tc_db_with_csv(res$con, "add_t_csv")
})

test_that("Append touchstone_countries", {
  res <- test_touchstone_country("add_t_append")
  compare_tc_db_with_csv(res$con, "add_t_append")
})

test_that("Append touchstone_countries - zero left", {
  res <- test_touchstone_country("add_t_app_zero")
  compare_tc_db_with_csv(res$con, "add_t_app_zero")
})

test_that("Dup entry in CSV", {
  expect_error(test_touchstone_country("dup_csv"),
    "Duplicated entries in new touchstone_country rows",
    class = "simpleError")
})

test_that("Empty disease in CSV", {
  expect_error(test_touchstone_country("empty_disease"),
               "Empty disease entry in touchstone_country",
               class = "simpleError")
})

test_that("Empty country in CSV", {
  expect_error(test_touchstone_country("empty_country"),
               "Empty country entry in touchstone_country",
               class = "simpleError")
})

test_that("Touchstone not in prep", {
  expect_error(test_touchstone_country("not_in_prep"),
               "Can't edit touchstone id (.*). Already exists with open/finished status.",
               class = "simpleError")
})
