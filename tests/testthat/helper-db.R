test_db_start <- function(verbose = FALSE) {
  test_db_script("db_start", verbose)
}

test_db_clear <- function(verbose = FALSE) {
  test_db_script("db_clear", verbose)
}

test_db_stop <- function(verbose = FALSE) {
  test_db_script("db_stop", verbose)
}

test_db_script <- function(name, verbose = FALSE) {
  path <- system.file(file.path("db", name), package = "stoner", mustWork = TRUE)
  sysname <- tolower(Sys.info()[["sysname"]])

  if (tolower(sysname) == "windows") {
    command <- file.path(dirname(dirname(Sys.which("git"))), "bin", "bash.exe")
    args <- path
  } else {
    command <- path
    args = character()
  }

  if (verbose) {
    code <- system2(command, args, stdout = "", stderr = "")
  } else {
    res <- system2(command, args, stdout = TRUE, stderr = TRUE)
    code <- attr(res, "status") %||% 0
  }

  if (code != 0) {
    if (!verbose) {
      cat(res)
    }
    stop(sprintf("DB script %s failed with code %d", name, code))
  }
}

test_db_connection <- function() {
  test_db_clear()
  DBI::dbConnect(RPostgres::Postgres(), dbname = "montagu",
                 user = "vimc", password = "changeme", port = 5432)
}

test_path <- function(context, path) {
  file.path("examples", context, path)
}

test_prepare <- function(path, con = NULL) {
  db_tables <- c("touchstone_name", "touchstone")
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

  # Not sure how to do row ordering, but...
  cols <- sort(names(tab))
  matches <- all(unlist(lapply(cols,
    function(x) all(tab[[x]] == csv[[x]]))))

  expect_true(matches,
    label = "DB Compare: Data matches")
}

test_run_import <- function(path, con = NULL) {
  e <- extract(path, con)
  test_extract(e)
  t <- transform(e)
  test_transform(t)
  l <- load(t, con)
  list(e = e, t = t)
}

