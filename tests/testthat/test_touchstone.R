context("touchstone")

# Here, we do tests on  these two CSV files...

# touchstone.csv
# Cols: id, touchstone_name, version, description, status, comment

# touchstone_name.csv
# Cols: id, description, comment

test_touchstone <- function(path, con = NULL) {
  con <- con %||% test_db_connection()
  DBI::dbBegin(con)
  test_prepare(path, con)
  c(test_run_import(path, con), con = con)
}

mess_with <- function(path, csv, col, row, text) {
  data <- read.csv(file.path(path, "meta", csv),
                   stringsAsFactors = FALSE)
  data[[col]][row] <- text
  write.csv(data, file.path(path, "meta", csv), row.names = FALSE)
}

create_touchstone_csv <- function(path, names, versions,
                                  descriptions = NULL,
                                  comments = NULL,
                                  db = FALSE) {
  filename <- "touchstone.csv"
  if (db) filename <- paste0("db_", filename)
  comments <- comments %||% paste0("Comment ", names, "-", versions)
  descriptions <- descriptions %||% paste(names,"description")

  stopifnot(length(comments) == length(names))
  stopifnot(length(names) == length(versions))
  stopifnot(length(names) == length(descriptions))

  write.csv(data_frame(
                 id = paste0(names, "-", versions),
    touchstone_name = names,
            version = versions,
        description = paste0(names, " (version ",versions, ")"),
             status = "in-preparation",
            comment = comments),
    file.path(path, "meta", filename), row.names = FALSE)
}

create_touchstone_name_csv <- function(path, names,
                                       descriptions = NULL,
                                       comments = NULL,
                                       db = FALSE) {
  filename <- "touchstone_name.csv"
  if (db) filename <- paste0("db_", filename)

  descriptions <- descriptions %||% paste(names, "description")
  comments <- comments %||% paste(names, "comment")

  stopifnot(length(names) == length(descriptions))
  stopifnot(length(names) == length(comments))

  invisible(write.csv(data_frame(
    id = names,
    description = descriptions,
    comment = paste0(names, " comment")),
      file.path(path, "meta", filename), row.names = FALSE))
}

test_that("Empty import should succeed trivially", {
  con <- test_db_connection()
  res <- test_touchstone(empty_test_dir(), con)
  expect_equal(length(res$t), 0)
  DBI::dbRollback(con)
})

test_that("A new touchstone and name", {
  path <- empty_test_dir()
  create_touchstone_csv(path, "nevis", 1)
  create_touchstone_name_csv(path, "nevis")
  compare_csv(test_touchstone(path), c("touchstone", "touchstone_name"))

})

test_that("Two new touchstones and names", {
  path <- empty_test_dir()
  create_touchstone_csv(path, c("nevis", "kilimanjaro"), c(1, 1))
  create_touchstone_name_csv(path, c("nevis", "kilimanjaro"))
  compare_csv(test_touchstone(path), c("touchstone", "touchstone_name"))
})

test_that("Two new touchstones, one touchstone name", {
  path <- empty_test_dir()
  create_touchstone_csv(path, c("nevis", "nevis"), c(1, 2))
  create_touchstone_name_csv(path, "nevis")
  compare_csv(test_touchstone(path), c("touchstone", "touchstone_name"))
})

test_that("New touchstone, touchstone name in db, not csv", {
  path <- empty_test_dir()
  create_touchstone_csv(path, "kilimanjaro", 1)
  create_touchstone_name_csv(path, "kilimanjaro", db = TRUE)
  compare_csv(test_touchstone(path), "touchstone")
})

test_that("Update existing touchstone_name (no refs)", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "kilimanjaro", db = TRUE)
  create_touchstone_name_csv(path, "kilimanjaro",
                             descriptions = "Updated Description")
  compare_csv(test_touchstone(path), "touchstone_name")
})

test_that("Update existing touchstone_name (in prep)", {
  path <- empty_test_dir()
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_name_csv(path, "nevis",
                             description = "Updated Description")
  compare_csv(test_touchstone(path), "touchstone_name")
})

test_that("Update existing touchstone details (in prep)", {
  path <- empty_test_dir()
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_csv(path, "nevis", 1, comments = "Extra comment")
  res <- test_touchstone(path)
  compare_csv(res, "touchstone")
})

test_that("Update and add touchstone and touchstone_name (in prep)", {
  path <- empty_test_dir()
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_name_csv(path, c("nevis", "kilimanjaro"),
      descriptions = c("Updated description", "new description"),
      comments = c("Updated comment", "first comment"))
  create_touchstone_csv(path, c("nevis", "kilimanjaro"), c(1, 1),
      comments = c("Updated comment", "first comment"))

  compare_csv(test_touchstone(path), c("touchstone", "touchstone_name"))
})

test_that("Exact match", {
  path <- empty_test_dir()
  create_touchstone_csv(path, "nevis", 1)
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(path, "nevis")
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  compare_csv(test_touchstone(path), c("touchstone", "touchstone_name"))
})

## Tests that should fail:

test_that("Add touchstone, name not found", {
  path <- empty_test_dir()
  create_touchstone_csv(path, "nevis", 1)
  con <- test_db_connection()
  expect_error(test_touchstone(path, con),
    "All touchstone.touchstone_name are known isn't true",
    class = "expectation_failure")
  DBI::dbRollback(con)
})

test_that("Add touchstone, bad version", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_csv(path, "nevis", 2)
  mess_with(path, "touchstone.csv", "description", 1, "nevis (version 1)")
  con <- test_db_connection()

  expect_error(test_touchstone(path, con),
    "All touchstone.description are formatted correctly isn't true",
               class = "expectation_failure")
  DBI::dbRollback(con)

})

test_that("Add touchstone, bad id format", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis")
  create_touchstone_csv(path, "nevis", 1)
  mess_with(path, "touchstone.csv", "id", 1, "nevis#1")
  con <- test_db_connection()

  expect_error(test_touchstone(path, con),
               "All touchstone.id are touchstone_name-version isn't true",
               class = "expectation_failure")
})

test_that("Edit touchstone name - not in-preparation", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(path, "nevis", descriptions = "Edited")
  mess_with(path, "db_touchstone.csv", "status", 1, "finished")
  con <- test_db_connection()
  expect_error(test_touchstone(path, con),
    paste0("Can't edit touchstone_name id (.*). ",
           "Already exists with open/finished touchstone versions"),
    class = "simpleError")
  DBI::dbRollback(con)
})

test_that("Edit touchstone - not in-preparation", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  create_touchstone_csv(path, "nevis", 1, comments = "More comments")
  mess_with(path, "db_touchstone.csv", "status", 1, "finished")
  con <- test_db_connection()

  expect_error(test_touchstone(path, con),
    "Can't edit touchstone id (.*). Already exists with open/finished status.",
    class = "simpleError")
  DBI::dbRollback(con)
})

test_that("touchstone CSV invalid", {
  path <- empty_test_dir()
  create_touchstone_csv(path, "nevis", 1)
  mess_with(path, "touchstone.csv", "haggis", 1, "yummy")
  con <- test_db_connection()
  expect_error(test_touchstone(path, con),
               "Correct columns in touchstone.csv not equal to (.*)",
               class = "expectation_failure")
  DBI::dbRollback(con)
})

test_that("touchstone_name CSV invalid", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis", 1)
  mess_with(path, "touchstone_name.csv", "pie_balm", 1, "clogg_banting")
  con <- test_db_connection()
  expect_error(test_touchstone(path, con),
               "Correct columns in touchstone_name.csv not equal to (.*)",
               class = "expectation_failure")
  DBI::dbRollback(con)
})

test_that("Duplicate touchstone id in csv", {
  path <- empty_test_dir()
  create_touchstone_csv(path, c("nevis", "nevis"), c(1, 1))
  create_touchstone_name_csv(path, "nevis")
  con <- test_db_connection()
  expect_error(test_touchstone(path, con),
               "No duplicate ids in touchstone.csv isn't false.",
               class = "expectation_failure")
  DBI::dbRollback(con)
})

test_that("Duplicate touchstone_name in csv", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, c("nevis", "nevis"))
  create_touchstone_csv(path, "nevis", 1)
  con <- test_db_connection()
  expect_error(test_touchstone(path, con),
               "No duplicate ids in touchstone_name.csv isn't false.",
               class = "expectation_failure")
  DBI::dbRollback(con)
})


test_that("Add touchstone, bad status", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis")
  create_touchstone_csv(path, "nevis", 1)
  mess_with(path, "touchstone.csv", "status", 1, "half-baked")
  con <- test_db_connection()
  expect_error(test_touchstone(path, con),
               "All touchstone.status are valid isn't true.",
               class = "expectation_failure")
  DBI::dbRollback(con)
})

test_that("Edit touchstone, bad status", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  create_touchstone_csv(path, "nevis", 1)
  mess_with(path, "touchstone.csv", "status", 1, "gooey")
  con <- test_db_connection()
  expect_error(test_touchstone(path, con),
               "All touchstone.status are valid isn't true.",
               class = "expectation_failure")
  DBI::dbRollback(con)
})

