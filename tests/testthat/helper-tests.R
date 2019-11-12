test_path <- function(context, path) {
  file.path("examples", context, path)
}

test_prepare <- function(path, con = NULL) {
  db_tables <- c("touchstone_name", "touchstone", "disease",
                 "scenario_description", "scenario",
                 "demographic_variant", "demographic_source",
                 "demographic_statistic_type",
                 "demographic_dataset",
                 "touchstone_demographic_dataset")
  for (table in db_tables) {
    csv_file <- read_meta(path, paste0("db_", table, ".csv"))
    if (!is.null(csv_file)) {
      DBI::dbWriteTable(con, table, csv_file, append = TRUE)
    }
    if (("id" %in% names(csv_file)) &&
       (is.numeric(csv_file$id))) {
      DBI::dbExecute(con, sprintf("SELECT setval('%s_id_seq', %s)",
        table, max(as.integer(csv_file$id))))
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

