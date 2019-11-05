test_path <- function(context, path) {
  file.path("examples", context, path)
}

test_prepare <- function(path, con = NULL) {
  db_tables <- c("touchstone_name", "touchstone", "disease",
                 "scenario_description", "scenario")
  for (table in db_tables) {
    csv_file <- read_meta(path, paste0("db_", table, ".csv"))
    if (!is.null(csv_file)) {
      DBI::dbWriteTable(con, table, csv_file, append = TRUE)
    }
  }
}

test_compare_csv_db <- function(con, csv, db) {
  tab <- DBI::dbReadTable(con, db)
  expect_true(all(sort(names(tab)) == sort(names(csv))),
              label = "DB Compare: Column names match")

  expect_equal(nrow(tab), nrow(csv),
               label = "DB Compare: Row count matches")

  cols <- sort(names(tab))
  tab$mash <- mash(tab)
  csv$mash <- mash(csv)
  expect_true(all(sort(tab$mash) == sort(csv$mash)),
              label = "DB Compare: Data matches")
}

test_run_import <- function(path, con = NULL, ...) {
  e <- stone_extract(path, con)
  stone_test_extract(e)
  t <- stone_transform(e)
  stone_test_transform(t)
  l <- stone_load(t, con, ...)
  list(e = e, t = t)
}

