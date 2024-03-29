extract_touchstone <- function(e, path, con) {

  # Collect db rows for touchstones, and touchstone_names that
  # we are interested in. Start with the CSVs (if any)

  eout <- list()

  ts <- unique(c(e$touchstone_csv$id,
                 e$touchstone_countries_csv$touchstone,
                 e$responsibilities_csv$touchstone)) %||% ""

  tsn <- unique(c(e$touchstone_name_csv$id,
                  e$touchstone_csv$touchstone_name)) %||% ""

  # Query DB for all touchstones that are connected with touchstones
  # or touchstone names

  ts <- c(ts,
    DBI::dbGetQuery(con, sprintf("
      SELECT DISTINCT touchstone.id
        FROM touchstone
        JOIN touchstone_name
          ON touchstone.touchstone_name = touchstone_name.id
       WHERE touchstone_name.id IN %s
          OR touchstone.id IN %s",
             sql_in(tsn), sql_in(ts)))$id)

  eout <- list(touchstone_csv = e$touchstone_csv,
                touchstone = db_get(con, "touchstone", "id", unique(ts)))

  # And also get the touchstone_name info itself

  if (!is.null(tsn)) {
    eout <- c(eout, list(
      touchstone_name_csv = e$touchstone_name_csv,
      touchstone_name = db_get(con, "touchstone_name", "id",
                               unique(tsn))))
  }
  eout

}

test_extract_touchstone <- function(e) {

  test_extract_touchstone_csv <- function(e) {
    ts <- e$touchstone_csv
    testthat::expect_equal(sort(names(ts)),
                 sort(c("id", "touchstone_name", "version", "description",
                        "status", "comment")),
      label = "Correct columns in touchstone.csv")

    testthat::expect_true(all(ts$touchstone_name %in%
                c(e[['touchstone_name_csv']]$id,
                  e[['touchstone_name']]$id)),
      label = "All touchstone.touchstone_name are known")

    testthat::expect_true(all(
      ts$id == sprintf("%s-%s", ts$touchstone_name, ts$version)),
        label = "All touchstone.id are touchstone_name-version")

    testthat::expect_true(all(
      ts$description == sprintf("%s (version %s)", ts$touchstone_name, ts$version)),
        label = "All touchstone.description are formatted correctly")

    testthat::expect_false(any(duplicated(ts$id)),
      label = "No duplicate ids in touchstone.csv")

    testthat::expect_true(all(ts$status %in%
      c("in-preparation", "open", "finished")),
      label = "All touchstone.status are valid")
  }

  test_extract_touchstone_name_csv <- function(e) {
    tsn <- e$touchstone_name_csv
    testthat::expect_equal(sort(names(tsn)),
                 sort(c("id", "description","comment")),
      label = "Correct columns in touchstone_name.csv")

    testthat::expect_false(any(duplicated(tsn$id)),
      label = "No duplicate ids in touchstone_name.csv")
  }

  if (!is.null(e$touchstone_csv)) {
    test_extract_touchstone_csv(e)
  }

  if (!is.null(e$touchstone_name_csv)) {
    test_extract_touchstone_name_csv(e)
  }

  # Touchstones are referred to in other CSV files. Test here
  # whether they are in the DB, or in the touchstone_csv (if provided)

  if (!is.null(e$touchstone_countries_csv)) {
    all_touchstones <- unique(c(e[['touchstone']]$id,
                                e[['touchstone_csv']]$id))

    testthat::expect_true(all(unique(e$touchstone_countries_csv$touchstone)
                              %in% all_touchstones),
                          label = "All touchstones in touchstone_country are recognised")
  }

}

###############################################################################

transform_touchstone <- function(e) {
  t <- list()

  if (!is.null(e$touchstone_csv)) {
    t <- c(t, copy_unique_flag(e, "touchstone"))
  }

  if (!is.null(e$touchstone_name_csv)) {
    t <- c(t, copy_unique_flag(e, "touchstone_name"))
  }

  t
}

test_transform_touchstone <- function(transformed_data) {
  # All useful tests done in extract stage.
}

###############################################################################

load_touchstone_name <- function(transformed_data, con) {
  to_edit <- add_non_serial_rows("touchstone_name", transformed_data, con)

  # For each row in to_edit, do an SQL update, as long as all versions
  # of this touchstone have status "in-preparation".

  for (r in seq_len(nrow(to_edit))) {

    status <- DBI::dbGetQuery(con, "
      SELECT DISTINCT status
        FROM touchstone
       WHERE touchstone_name = $1",
        to_edit$id[r])$status

    # If there are no versions whatsoever, it's safe to edit.

    if (length(status) == 0) {
      status <- "in-preparation"
    }

    if ((length(status) == 1) && (status == 'in-preparation')) {

      DBI::dbExecute(con, "
        UPDATE touchstone_name
           SET description = $1, comment = $2 WHERE id = $3",
         list(to_edit$description[r],
              to_edit$comment[r],
              to_edit$id[r]))
    } else {

      stop(paste("Can't edit touchstone_name id ", to_edit$id[r], ".",
                 "Already exists with open/finished touchstone versions."))
    }
  }
}

test_in_prep <- function(transformed_data, con) {

  # Having uploaded the CSV touchstones, now check status is in-prep
  # for all other references to touchstones.

  ts <- c(transformed_data$touchstone_demographic_dataset$touchstone,
          transformed_data$touchstone_country$touchstone)

  if (is.null(ts)) {
    return()
  }

  db_ts <- db_get(con, "touchstone", "id", unique(ts))
  db_ts <- db_ts[db_ts$status != "in-preparation", ]
  if (nrow(db_ts) > 0) {
    stop(paste(sprintf("Can't edit touchstone id %s.", sql_in(db_ts$id)),
                       "Already exists with open/finished status."))
  }
}

load_touchstone <- function(transformed_data, con) {
  to_edit <- add_non_serial_rows("touchstone", transformed_data, con)

  if (nrow(to_edit) > 0) {
    existing_status <- db_get(con, "touchstone", "id", to_edit$id,
                              "id, status")
  }

  # For each row in to_edit, do an SQL update, as long as the status
  # is in-preparation.

  for (r in seq_len(nrow(to_edit))) {
    touch <- to_edit[r, ]

    if (all(unique(c(touch$status,
                 existing_status$status[existing_status$id == touch$id]))
        == 'in-preparation')) {

      DBI::dbExecute(con, "
        UPDATE touchstone
           SET touchstone_name = $1, version = $2, description = $3,
               status = $4, comment = $5
        WHERE id = $6",
                     list(touch$touchstone_name, touch$version,
                          touch$description, touch$status,
                          touch$comment, touch$id))
    } else {

      stop(paste("Can't edit touchstone id ", touch$id, ".",
                 "Already exists with open/finished status."))
    }
  }

  test_in_prep(transformed_data, con)
}
