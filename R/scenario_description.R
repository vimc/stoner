extract_scenario_description <- function(e, path, con) {
  if (!is.null(e$scenario_description_csv)) {
    e <- c(list(
      scenario_description_csv = e$scenario_description_csv,

      scenario_description = db_get(con, "scenario_description", "id",
        unique(e$scenario_description_csv$id)),

       disease = db_get(con, "disease", "id",
         unique(e$scenario_description_csv$disease), "id")
    ))

  e

  } else {
    list()
  }
}

test_extract_scenario_description <- function(e) {

  expect_true(all(unique(e[['scenario_description_csv']]$disease)
                  %in% e[['disease']]$id),
              label = "Diseases in scenario_description are valid")

  expect_false(any(duplicated(e[['scenario_description_csv']]$id)),
               label = "Duplicate ids in scenario_description.csv")

  if (!is.null(e[['scenario_description_csv']])) {
    expect_true(identical(sort(names(e[['scenario_description_csv']])),
                        sort(c("id", "description", "disease"))),
              label = "Column names correct in scenario_description.csv")
  }
}

###############################################################################

transform_scenario_description <- function(e) {
  copy_unique_flag(e, "scenario_description")
}

test_transform_scenario_description <- function(transformed_data) {
  # Nothing useful to do here.
}

###############################################################################

load_scenario_description <- function(transformed_data, con,
                    allow_overwrite_scenario_description = FALSE) {
  to_edit <- add_return_edits("scenario_description", transformed_data, con)

  # For each row in to_edit, do an SQL update, as long as there is no
  # non in-preparation touchstone that refers to this scenario description.

  # Note that scenario_descriptions are not touchstone specific, so when
  # they get edited, it will affect everywhere where that scenario is
  # noted. But it is a human-readable description, so as long as this is
  # only used to clarify things for modellers, and change the meaning,
  # we can allow updates I suppose.

  # By default, stoner will refuse to edit the scenario_description for
  # a scenario that is part of a non in-preparation touchstone. Force
  # this by calling load with allow_overwrite_scenario_description = TRUE.

  for (r in seq_len(nrow(to_edit))) {

    # If the overide is set, then
    # it's ok to edit, so don't bother with SQL look up.

    if (allow_overwrite_scenario_description) {
      status <- 'in-preparation'

    } else {

      status <- DBI::dbGetQuery(con, "
        SELECT DISTINCT status
          FROM touchstone
          JOIN scenario
            ON scenario.touchstone = touchstone.id
         WHERE scenario.scenario_description = $1",
          to_edit$id[r])$status

      # If no results at all, then it's also ok to edit.

      if (length(status) == 0) {
        status <- "in-preparation"
      }
    }

    if ((length(status) == 1) && (status == 'in-preparation')) {

      DBI::dbExecute(con, "
        UPDATE scenario_description
           SET description = $1, disease = $2 WHERE id = $3",
         list(to_edit$description[r],
              to_edit$disease[r],
              to_edit$id[r]))
    } else {

      stop(paste0("Can't edit scenario_description with id ", to_edit$id[r], ". ",
                  "Already exists with open/finished touchstone versions."))
    }
  }
}
