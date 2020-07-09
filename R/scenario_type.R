extract_scenario_type <- function(e, path, con) {

  if ((!is.null(e$scenario_type_csv)) || (!is.null(e$scenario_description_csv))) {
    list(
      scenario_type_csv = e$scenario_type_csv,

      scenario_type = db_get(con, "scenario_type", "id",
        unique(c(e$scenario_type_csv$id,
                 e$scenario_description_csv$scenario_type)))

    )

  } else {
    list()
  }
}

test_extract_scenario_type <- function(e) {
  testthat::expect_false(any(duplicated(e[['scenario_type_csv']]$id)),
               label = "Duplicate ids in scenario_type.csv")

  if (!is.null(e$scenario_type_csv)) {
    testthat::expect_true(
      identical(sort(names(e$scenario_type_csv)),
                sort(c("id", "name"))),
      label = "Column names correct in scenario_type.csv")
  }
}

###############################################################################

transform_scenario_type <- function(e) {
  if (!is.null(e$scenario_type_csv)) {
    copy_unique_flag(e, "scenario_type")
  }
}

test_transform_scenario_type <- function(transformed_data) {
  # Nothing useful to do here.
}

###############################################################################

load_scenario_type <- function(transformed_data, con,
                    allow_overwrite_scenario_type = FALSE) {
  to_edit <- add_non_serial_rows("scenario_type", transformed_data, con)

  # For each row in to_edit, do an SQL update, as long as there is no
  # non in-preparation touchstone that refers to this scenario type.

  # scenario_types (like scenario_descriptions) are not touchstone specific.
  # If/when they get edited, it will affect everywhere that scenario is
  # used.

  # By default, stoner will refuse to edit the scenario_type for
  # a scenario that is part of a non in-preparation touchstone. Force
  # this by calling load with allow_overwrite_scenario_type = TRUE.

  for (r in seq_len(nrow(to_edit))) {

    # If the overide is set, then
    # it's ok to edit, so don't bother with SQL look up.

    if (allow_overwrite_scenario_type) {
      status <- 'in-preparation'

    } else {

      status <- DBI::dbGetQuery(con, "
        SELECT DISTINCT status
          FROM touchstone
          JOIN scenario
            ON scenario.touchstone = touchstone.id
          JOIN scenario_description
            ON scenario.scenario_description = scenario_description.id
         WHERE scenario_description.scenario_type = $1",
          to_edit$id[r])$status

      # If no results at all, then it's also ok to edit.

      if (length(status) == 0) {
        status <- "in-preparation"
      }
    }

    if ((length(status) == 1) && (status == 'in-preparation')) {

      DBI::dbExecute(con, "
        UPDATE scenario_type
           SET name = $1
         WHERE id = $2",
         list(to_edit$name[r],
              to_edit$id[r]))
    } else {

      stop(paste0("Can't edit scenario_type with id ",
                  to_edit$id[r], ". ",
                  "Already exists with open/finished touchstone versions."))
    }
  }
}
