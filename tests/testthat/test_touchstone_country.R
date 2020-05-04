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

test_that("Invalid columns", {
  test <- new_test()
  create_ts_country_csv(test$path, "nevis-1", "flu", "ZMB;ZWE")
  mess_with(test$path, "touchstone_country.csv", "extra", 1, "column")
  expect_error(do_test(test),
    "Invalid columns in touchstone_country.csv",
    class = "simpleError")
})

test_that("Invalid country in touchstone_country", {
  test <- new_test()
  standard_disease_touchstones(test)
  create_ts_country_csv(test$path, "nevis-1", "flu", "ZMB;ZWE;ELF")
  expect_error(do_test(test),
    "All countries in touchstone_country are recognised isn't true",
    class = "expectation_failure")
})

test_that("Invalid disease in touchstone_country", {
  test <- new_test()
  standard_disease_touchstones(test)
  create_ts_country_csv(test$path, "nevis-1", "gout", "ZMB;ZWE")
  expect_error(do_test(test),
    "All diseases in touchstone_country are recognised isn't true",
       class = "expectation_failure")
})

test_that("Invalid touchstone in touchstone_country", {
  test <- new_test()
  standard_disease_touchstones(test)
  create_ts_country_csv(test$path, "snowdon-1", "flu", "ZMB;ZWE")
  expect_error(do_test(test),
    "All touchstones in touchstone_country are recognised isn't true",
    class = "expectation_failure")
})

test_tcs <- function(test, countries) {
  res <- DBI::dbGetQuery(test$con, "SELECT * FROM touchstone_country")
  expect_equal(nrow(res), length(countries))
  expect_true(all(res$id > 0))
  expect_true(all(res$touchstone == 'nevis-1'))
  expect_true(all(sort(res$country) == sort(countries)))
  expect_true(all(res$disease == 'flu'))
}

test_that("Add touchstone_country - touchstone in db", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  create_ts_country_csv(test$path, "nevis-1", "flu", "ZWE;ZMB")
  test_tcs(do_test(test), c("ZWE","ZMB"))
})

test_that("Add touchstone_country - touchstone in csv", {
  test <- new_test()
  standard_disease_touchstones(test, db = FALSE)
  create_ts_country_csv(test$path, "nevis-1", "flu", "ZWE;ZMB")
  test_tcs(do_test(test), c("ZWE", "ZMB"))
})

test_that("Append touchstone_countries", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  create_ts_country_csv(test$path, "nevis-1", "flu", "XK", db = TRUE)
  create_ts_country_csv(test$path, "nevis-1", "flu", "AFG")
  test_tcs(do_test(test), c("AFG", "XK"))
})

test_that("Append touchstone_countries - zero left", {
  test <- new_test()
  standard_disease_touchstones(test, db = TRUE)
  create_ts_country_csv(test$path, "nevis-1", "flu", c("ZMB","ZWE"), db = TRUE)
  create_ts_country_csv(test$path, "nevis-1", "flu", "ZMB;ZWE")
  test_tcs(do_test(test), c("ZMB", "ZWE"))
})

test_that("Dup entry in CSV", {
  test <- new_test()
  standard_disease_touchstones(test)
  create_ts_country_csv(test$path, "nevis-1", "flu",
                        c("ZMB;ZWE", "AFG;ZMB"))
  test_tcs(do_test(test), c("AFG", "ZMB", "ZWE"))
})

test_that("Empty disease in CSV", {
  test <- new_test()
  standard_disease_touchstones(test)
  create_ts_country_csv(test$path, "nevis-1", "", "XK")
  expect_error(do_test(test),
    "Empty disease entry in touchstone_country",
    class = "simpleError")
})

test_that("Empty country in CSV", {
  test <- new_test()
  standard_disease_touchstones(test)
  create_ts_country_csv(test$path, "nevis-1", "flu", "")
  expect_error(do_test(test),
    "Empty country entry in touchstone_country",
    class = "simpleError")
})

test_that("Touchstone not in prep", {
  test <- new_test()
  standard_disease_touchstones(test)
  mess_with(test$path, "db_touchstone.csv", "status", 1, "finished")
  create_ts_country_csv(test$path, "nevis-1", "flu", "ZWE")
  expect_error(do_test(test),
    "Can't edit touchstone id (.*). Already exists with open/finished status.",
    class = "simpleError")
})
