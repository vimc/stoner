test_path <- function(context, path) {
  file.path("examples", context, path)
}

cache_con <<- NULL

new_test <- function() {
  # Create stoner_test/meta in a temporary dir,
  # delete any existing files within it.
  # Also get new db connection, and
  # empty any leftover rows from previous tests.

  res <- list()
  path <- tempdir()
  res$path <- file.path(path, "stoner_test")
  dir.create(res$path, showWarnings = FALSE)
  inner <- file.path(res$path, "meta")
  dir.create(inner, showWarnings = FALSE)
  files <- dir(inner, full.names = TRUE)
  file.remove(files)
  cache_con <<- cache_con %||% test_db_connection()
  res$con <- cache_con
  DBI::dbExecute(res$con, "DELETE FROM touchstone_country")
  DBI::dbExecute(res$con, "DELETE FROM scenario")
  DBI::dbExecute(res$con, "DELETE FROM scenario_description")
  DBI::dbExecute(res$con, "DELETE FROM disease")
  DBI::dbExecute(res$con, "DELETE FROM touchstone")
  DBI::dbExecute(res$con, "DELETE FROM touchstone_name")
  res
}

test_prepare <- function(path, con = NULL) {
  db_tables <- c("touchstone_name", "touchstone", "disease",
                 "scenario_description", "scenario",
                 "demographic_variant", "demographic_source",
                 "demographic_statistic_type",
                 "demographic_dataset",
                 "touchstone_demographic_dataset",
                 "touchstone_country")
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

compare_csv <- function(res, tables) {
  expect_true(all(unlist(lapply(seq_along(tables), function(ti) {
    table <- tables[ti]
    test_compare_csv_db(
      res$con, res$e[[paste0(table, "_csv")]], table)}))))
}

mess_with <- function(path, csv, col, row, text) {
  data <- read.csv(file.path(path, "meta", csv),
                   stringsAsFactors = FALSE)
  data[[col]][row] <- text
  write.csv(data, file.path(path, "meta", csv), row.names = FALSE)
}

db_file <- function(db, f) {
  if (!db) f
  else paste0("db_", f)
}

create_touchstone_csv <- function(path, names, versions,
                                  descriptions = NULL,
                                  comments = NULL,
                                  db = FALSE) {
  comments <- comments %||% paste0("Comment ", names, "-", versions)
  descriptions <- descriptions %||% paste(names,"description")

  write.csv(data_frame(
    id = paste0(names, "-", versions),
    touchstone_name = names,
    version = versions,
    description = paste0(names, " (version ",versions, ")"),
    status = "in-preparation",
    comment = comments),
    file.path(path, "meta", db_file(db, "touchstone.csv")),
    row.names = FALSE)
}

create_touchstone_name_csv <- function(path, names,
                                       descriptions = NULL,
                                       comments = NULL,
                                       db = FALSE) {
  descriptions <- descriptions %||% paste(names, "description")
  comments <- comments %||% paste(names, "comment")

  invisible(write.csv(data_frame(
    id = names,
    description = descriptions,
    comment = paste0(names, " comment")),
    file.path(path, "meta", db_file(db, "touchstone_name.csv")),
    row.names = FALSE))
}

create_disease_csv <- function(path, ids, names, db = TRUE) {
  write.csv(data_frame(
    id = ids, name = names),
    file.path(path, "meta", db_file(db, "disease.csv")), row.names = FALSE)
}

create_scenario_csv <- function(path, ids, touchstones, sds, db = FALSE) {
  write.csv(data_frame(
    id = ids, touchstone = touchstones,
    scenario_description = sds, focal_coverage_set = NA),
    file.path(path, "meta", db_file(db, "scenario.csv")),
    row.names = FALSE)
}

create_scen_desc_csv <- function(path, ids, descs, diseases, db = FALSE) {
  write.csv(data_frame(
    id = ids, description = descs, disease = diseases),
    file.path(path, "meta", db_file(db, "scenario_description.csv")),
    row.names = FALSE)
}

create_ts_country_csv <- function(path, tstones, diseases,
                                  countries, db = FALSE) {
  write.csv(data_frame(
    touchstone = tstones, disease = diseases, country = countries),
    file.path(path, "meta", db_file(db, "touchstone_country.csv")),
    row.names = FALSE)
}

test_run_import <- function(path, con = NULL, ...) {
  e <- stone_extract(path, con)
  stone_test_extract(e)
  t <- stone_transform(e)
  stone_test_transform(t)
  l <- stone_load(t, con, ...)
  list(e = e, t = t)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

do_test <- function(test, ...) {
  test_prepare(test$path, test$con)
  c(test_run_import(test$path, test$con, ...), con = test$con)
}

