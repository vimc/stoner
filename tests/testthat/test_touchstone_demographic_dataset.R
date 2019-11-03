context("touchstone_demographic_dataset")

# Here, we do tests on:

# touchstone_demographic_dataset.csv
# Cols: touchstone,demographic_source,demographic_statistic_type
#       (all strings, not numerical ids)

test_touchstone_demographic_dataset <- function(test_name, ...) {
  con <- test_db_connection()
  path <- test_path("touchstone_demographic_dataset", test_name)
  test_prepare(path, con)
  c(test_run_import(path, con, ...), con = con)
}

compare_tdd_db_with_csv <- function(con, test_name) {
  path <- test_path("touchstone_demographic_dataset", test_name)
  expected <- read_meta(path, "expected_result.csv")
  db_tdd <- db_tdd_for_touchstones(con, unique(expected$touchstone))
  db_tdd$mash <- paste(db_tdd$dsource_code,
                       db_tdd$dtype_code, sep = '\r')
  expected$mash <- paste(expected$demographic_source,
                         expected$demographic_statistic_type, sep = '\r')
  match(expected$mash, db_tdd$mash)

  expect_equal(nrow(expected), nrow(db_tdd))
  expect_true(all(sort(db_tdd$mash) == sort(expected$mash)))
}

test_that("A new touchstone demographic dataset", {
  res <- test_touchstone_demographic_dataset("new_tdd")
  compare_tdd_db_with_csv(res$con, "new_tdd")
})

# touchstone was added in same import
# dataset doesn't yet exist
# exact entry already in db.

# touchstone doesn't exist
# touchstone exists with non in-prep status
#   (we may want to override)
# demographic_source invalid
# demographic_statistic_type invalid
# incorrect columns or types in input




