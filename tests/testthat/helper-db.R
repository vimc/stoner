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
  verbose <- TRUE
  path <- system.file(file.path("db", name), package = "stoner", mustWork = TRUE)
  sysname <- tolower(Sys.info()[["sysname"]])

  if (tolower(sysname) == "windows") {
    command <- file.path(dirname(dirname(Sys.which("git"))), "bin", "bash.exe")
    args <- path
  } else {

    command <- path
    args = character()
    message(sprintf("sysname: %s, command=%s", sysname, command))
  }

  if (verbose) {
    code <- system2(command, args, stdout = "", stderr = "")
  } else {
    res <- system2(command, args, stdout = TRUE, stderr = TRUE)
    code <- attr(res, "status") %||% 0
  }

  if (code != 0) {
    if (verbose) {
      cat(res)
    }
    stop(sprintf("DB script %s failed with code %d", name, code))
  }
}

test_db_connection <- function() {
  test_db_clear()
  DBI::dbConnect(RPostgres::Postgres(), dbname = "montagu",
                 user = "vimc", password = "changeme", port = 5435)
}
