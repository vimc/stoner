###############################################################################

extract_scenario_description <- function(path, con) {
  e <- extract_table(path, con, "scenario_description", "id")
  disease <- db_get(con, "disease", "id", unique(e$scenario_description_csv$disease), "id")
  c(e, list(disease = disease))
}

test_extract_scenario_description <- function(extracted_data) {

  expect_true(all(unique(extracted_data[['scenario_description_csv']]$disease)
                  %in% extracted_data[['disease']]$id),
              label = "Diseases in scenario_description are valid")

  expect_false(any(is.null(extracted_data[['scenario_description_csv']]$description)))
  expect_false(any(is.na(extracted_data[['scenario_description_csv']]$description)))
  expect_false(any(is.null(extracted_data[['scenario_description_csv']]$id)))
  expect_false(any(is.na(extracted_data[['scenario_description_csv']]$id)))

}

###############################################################################

transform_scenario_description <- function(e) {
  copy_unique_flag(e, "scenario_description")
}

test_transform_scenario_description <- function(transformed_data) {
  # Nothing really useful to do here.
}

###############################################################################

load_scenario_description <- function(transformed_data, con) {
  sds <- transformed_data[['scenario_description']]
  ids_found <- db_get(con, "scenario_description", "id", sds$id, "id")$id

  to_add <- sds[!sds$id %in% ids_found, ]
  to_edit <- sds[sds$id %in% ids_found, ]

  DBI::dbWriteTable(con, "scenario_description", to_add, append = TRUE)

  # For each row in to_edit, do an SQL update, as long as there is no
  # non in-preparation touchstone that refers to this scenario description.
  # This is quite unlikely - both that we'll ever edit a scenario description
  # like this, or that we'll be able to do so without changing live things.
  # But this is written here to fulfil the template of how all the tables
  # can be added/edited.

  for (r in seq_len(nrow(to_edit))) {

    status <- DBI::dbGetQuery(con, "
      SELECT DISTINCT status
        FROM touchstone
        JOIN scenario
          ON scenario.touchstone = touchstone.id
       WHERE scenario.scenario_description = $1",
        to_edit$id[r])

    # If there are no versions whatsoever, it's safe to edit.

    if (length(status) == 0) {
      status = 'in-preparation'
    }

    status = as.character(status)

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
