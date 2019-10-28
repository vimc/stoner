test_db_start <- function() {
  test_db_script("db_start")
}

test_db_clear <- function() {
  test_db_script("db_clear")
}

test_db_stop <- function() {
  test_db_script("db_stop")
}

test_db_script <- function(name) {
  path <- system.file(file.path("db", name), package = "stoner", mustWork = TRUE)
  sysname <- tolower(Sys.info()[["sysname"]])
  if (tolower(sysname) == "windows") {
    bash <- file.path(dirname(dirname(Sys.which("git"))), "bin", "bash.exe")
    code <- system2(bash, path)
  } else {
    code <- system2(path)
  }
  if (code != 0) {
    stop(sprintf("DB script %s failed with code %d", name, code))
  }
}


test_db_connection <- function() {
  test_db_clear()
  DBI::dbConnect(RPostgres::Postgres(), dbname = "montagu", user = "vimc", password = "changeme", port = 5432)
})
