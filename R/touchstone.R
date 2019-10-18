###############################################################################

extract_touchstone <- function(path, con) {
  c(
    extract_table("touchstone", "id"),
    extract_table("touchstone_name", "id")
  )
}

test_extract_touchstone <- function(extracted_data) {
  ex_names <- names(extracted_data)
  expect_true(("touchstone" %in% ex_names +
                 "touchstone_csv" %in% ex_names) %in% c(0, 2),
              label = "touchstone csv/db - both or none")

  expect_true(("touchstone_name" %in% ex_names +
                 "touchstone_name_csv" %in% ex_names) %in% c(0, 2),
              label = "touchstone_name csv/db - both or none")

  expect_equal(sort(names(extracted_data[['touchstone']])),
               sort(names(extracted_data[['touchstone_csv']])),
               label = "touchstone csv columns match database")

  expect_equal(sort(names(extracted_data[['touchstone_name']])),
               sort(names(extracted_data[['touchstone_name_csv']])),
               label = "touchstone_name csv columns match database")

  expect_true(all(extracted_data[['touchstone_csv']]$touchstone_version %in%
                  extracted_data[['touchstone_name_csv']]$id))
}

###############################################################################

transform_touchstone <- function(e) {
  c(
    copy_unique_flag(e, "touchstone"),
    copy_unique_flag(e, "touchstone_name")
  )
}

test_transform_touchstone <- function(transformed_data) {
  ts <- transformed_data[['touchstone']]
  if (!is.null(ts)) {
    expect_equal(ts$id, paste0(ts$touchstone_name, "-", ts$version))
    expect_true(all(ts$status %in% c("in-preparation", "open", "finished")))
  }
}

###############################################################################

load_touchstone_name <- function(con, t) {
  tnames <- t[['touchstone_name']]
  ids <- sql_in(tnames$id)
  ids_found <- DBI::dbGetQuery(con, sprintf("
    SELECT id FROM touchstone_name WHERE id IN %s", ids))$id

  to_add <- tnames[!tnames$id %in% ids_found, ]
  to_edit <- tnames[tnames$id %in% ids_found, ]

  DBI::dbWriteTable(con, "touchstone_name", to_add, append = TRUE)

  # For each row in to_edit, do an SQL update, as long as all versions
  # of this touchstone have status "in-preparation".

  for (r in seq_len(nrow(to_edit))) {

    status <- DBI::dbGetQuery(con, "
      SELECT DISTINCT status
        FROM touchstone
       WHERE touchstone_name = $1",
        to_edit$id[r])

    # If there are no versions whatsoever, it's safe to edit.

    if (length(status) == 0) {
      status = 'in-preparation'
    }

    status = as.character(status)

    if ((length(status) == 1) && (status == 'in-preparation')) {

      DBI::dbExecute(con, "
        UPDATE touchstone_name
           SET description = $1, comment = $2 WHERE id = $3",
         list(to_edit$description[r],
              to_edit$comment[r],
              to_edit$id[r]))

    } else {

      stop(paste0("Can't edit touchstone_name id ", to_edit$id[r], ". ",
                  "Already exists with open/finished touchstone versions."))
    }
  }
}

load_touchstone <- function(con, t) {
  ts <- t[['touchstone']]
  ids <- sql_in(ts$id)
  ids_found <- DBI::dbGetQuery(con, sprintf("
    SELECT id FROM touchstone WHERE id IN %s", ids))$id

  to_add <- tnames[!ts$id %in% ids_found, ]
  to_edit <- tnames[ts$id %in% ids_found, ]

  DBI::dbWriteTable(con, "touchstone", to_add, append = TRUE)

  # For each row in to_edit, do an SQL update, as long as the status
  # is in-preparation.

  for (r in seq_len(nrow(to_edit))) {
    touch <- ts[r, ]

    if (touch$status == 'in_preparation') {

      DBI::dbExecute(con, "
        UPDATE touchstone
           SET touchstone_name = $1, version = $2, description = $3,
               status = $4, comment = $5
        WHERE id = $6",
                     list(touch$touchstone_name, touch$version,
                          touch$description, touch$status,
                          touch$comment, touch$id))

    } else {

      stop(paste0("Can't edit touchstone id ", touch$id, ". ",
                  "Already exists with open/finished status."))
    }
  }

}




