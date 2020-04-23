###############################################################################
#
# responsibilites.csv may cause additions to these tables:-
#    responsibility
#    responsibility_set
#    burden_estimate_expectation
#    burden_estimate_country_expectation
#    burden_estimate_outcome_expectation
#    scenario

# The responsibilities csv has the following columns - all those that can take
# multiple semi-colon separated arguments are exampled as such below...

# modelling_group      - eg IC-Hallett
# disease              - eg HepB
# touchstone           - eg 201910gavi-5
# scenario             - eg hepb-no-vaccination;hepb-bd-routine-bestcase
# scenario_type        - eg standard (see expectation description)
# age_min_inclusive    - eg 0
# age_max_inclusve     - eg 99
# cohort_min_inclusve  - eg 1901
# cohort_max_inclusive - eg 2100
# year_min_inclusive   - eg 2000
# year_max_inclusive   - eg 2100
# countries            - eg AFG;BEN;COD
# outcomes             - eg dalys;hepb_deaths_hcc;hepb_infections_acute

extract_responsibilities <- function(e, path, con) {

  if (is.null(e$responsibilities_csv)) return(NULL)
  if (nrow(e$responsibilities_csv) == 0) return(NULL)

  res <- list(

    resp_scenarios = DBI::dbGetQuery(con, sprintf("
      SELECT * FROM scenario
       WHERE touchstone IN %s OR
             scenario_description IN %s",
       sql_in(unique(e$responsibilities_csv$touchstone)),
       sql_in(unique(e$responsibilities_csv$scenario)))),

    resp_countries = DBI::dbGetQuery(con, sprintf("
      SELECT * FROM country WHERE id IN %s",
        sql_in(unique(split_semi(e$responsibilities_csv$countries))))),

    resp_outcomes = DBI::dbGetQuery(con, sprintf("
      SELECT * FROM burden_outcome WHERE code IN %s",
        sql_in(unique(split_semi(e$responsibilities_csv$outcomes))))),

    resp_modelling_group = DBI::dbGetQuery(con, sprintf("
      SELECT * FROM modelling_group WHERE id IN %s",
        sql_in(unique(split_semi(e$responsibilities_csv$modelling_group))))),

    resp_responsibility_set = DBI::dbGetQuery(con, sprintf("
      SELECT * FROM responsibility_set
       WHERE modelling_group IN %s
         AND touchstone IN %s",
           sql_in(unique(e$responsibilities_csv$modelling_group)),
           sql_in(unique(e$responsibilities_csv$touchstone)))),

    resp_touchstone = DBI::dbGetQuery(con, sprintf("
      SELECT DISTINCT touchstone_name FROM touchstone
        WHERE id IN %s",
      sql_in(unique(e$responsibilities_csv$touchstone))))$touchstone_name
  )

  res$resp_expectations <- DBI::dbGetQuery(con, sprintf("
    SELECT * FROM burden_estimate_expectation
       WHERE version IN %s",
         sql_in(res$resp_touchstone)))

  res
}

test_extract_responsibilities <- function(e) {
  if (is.null(e$responsibilities_csv)) return()

  if (nrow(e$responsibilities_csv) == 0) return(NULL)

  expect_equal(sort(names(e$responsibilities_csv)),
    c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
      "cohort_min_inclusive", "countries", "description", "modelling_group",
      "outcomes", "scenario", "scenario_type", "touchstone",
      "year_max_inclusive", "year_min_inclusive"),
    label = "Columns in burden_estimate_expectation.csv")

  expect_type(e$responsibilities_csv$age_max_inclusive, "integer")
  expect_type(e$responsibilities_csv$age_min_inclusive, "integer")
  expect_type(e$responsibilities_csv$cohort_max_inclusive, "integer")
  expect_type(e$responsibilities_csv$cohort_min_inclusive, "integer")
  expect_type(e$responsibilities_csv$year_max_inclusive, "integer")
  expect_type(e$responsibilities_csv$year_min_inclusive, "integer")
  expect_type(e$responsibilities_csv$scenario_type, "character")

  if (any(e$responsibilities_csv$year_min_inclusive >
          e$responsibilities_csv$year_max_inclusive)) {
    stop("Responsibility year_min_inclusive must be before year_max_inclusive")
  }

  if (any(e$responsibilities_csv$age_min_inclusive >
          e$responsibilities_csv$age_max_inclusive)) {
    stop("Responsibility age_min_inclusive must be before age_max_inclusive")
  }

  if (any(e$responsibilities_csv$cohort_min_inclusive >
          e$responsibilities_csv$cohort_max_inclusive)) {
    stop(paste0("Responsibility cohort_min_inclusive must be before ",
                "cohort_max_inclusive"))
  }

  if (!all(e$responsibilities_csv$countries %in% e$resp_countries$id)) {
    errs <- which(!e$responsibilities_csv$countries %in% e$resp_countries$id)
    countries <- paste(e$responsibilities_csv$countries[errs], sep = ", ")
    stop(sprintf("Unknown responsibility countries: %s",countries))
  }

  if (!all(e$responsibilities_csv$modelling_group %in%
           e$resp_modelling_group$id)) {
    errs <- which(!e$responsibilities_csv$modelling_group %in%
                   e$resp_modelling_group$id)
    groups <- paste(e$responsibilities_csv$modelling_group[errs], sep = ", ")
    stop(sprintf("Unknown responsibility modelling_groups: %s", groups))
  }

}

###############################################################################

transform_responsibilities <- function(e, t_so_far) {
  if (is.null(e$responsibilities_csv)) return(list())
  if (nrow(e$responsibilities_csv) == 0) return(list())

  res <- list()

  # Adding Modelling groups isn't supported in Stoner (yet) - absent
  # modelling groups will cause a failure in the extract stage.

  # Touchstone existence is already handled in touchstone.R
  # Scenario_description existence is already handled in scenario_description.R
  # but, we might need to new lines in scenario
  # (serial_id, touchstone, scenario_decription)

  # Scenarios can be a semi-colon separated string; explode,
  # combining with touchstone (only one of these), and then assess
  # which pairs of (touchstone, scenario_description) already exist,
  # and which need creating.

  res$scenario <- NULL
  rcsv <- e$responsibilities_csv

  for (r in seq_len(nrow(e$responsibilities_csv))) {
    row <- rcsv[r, ]

    explode_scenarios <- split_semi(row$scenario)

    res$scenario <- rbind(res$scenario, data_frame(
      touchstone = row$touchstone,
      scenario_description = explode_scenarios))
  }

  fields <- c("touchstone", "scenario_description")
  res$scenario <- assign_serial_ids(res$scenario, e$resp_scenarios, "scenario",
                                    fields, fields)

  # Now look up/add any responsibility_set
  # (modelling_group, touchstone)

  res$responsibility_set <- data_frame(
    modelling_group = rcsv$modelling_group,
    touchstone = rcsv$touchstone,
    status = "incomplete")

  fields <- c("modelling_group", "touchstone")
  res$responsibility_set <- assign_serial_ids(
    res$responsibility_set, e$resp_responsibility_set, "responsibility_set",
    fields, fields)

  # Next burden_estimate_expectation. The other tables (responsibility,
  # burden_estimate_country_expectation and
  # burden_estimate_outcome_expectation have foreign keys to
  # burden_estimate_expectation

  # Conventionally, the description in burden_estimate_expectation is
  # in the form disease:modelling_group:scenario_type - where scenario_type
  # is either all, bd, non_bd, standard - ie, where years, ages, countries
  # etc have been the same for different scenarios, they've been grouped
  # into one.

  res$burden_estimate_expectation <- data_frame(
     age_max_inclusive = rcsv$age_max_inclusive,
     age_min_inclusive = rcsv$age_min_inclusive,
  cohort_max_inclusive = rcsv$cohort_max_inclusive,
  cohort_min_inclusive = rcsv$cohort_min_inclusive,
    year_max_inclusive = rcsv$year_max_inclusive,
    year_min_inclusive = rcsv$year_min_inclusive,
           description = paste(rcsv$disease,
                               rcsv$modelling_group,
                               rcsv$scenario_type, sep = ':'),
               version = e$touchstone$touchstone_name[match(
                           rcsv$touchstone, e$touchstone$id)]
  )

  fields <- c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
              "cohort_min_inclusive", "year_max_inclusive",
              "year_min_inclusive", "description", "version")

  res$burden_estimate_expectation <- assign_serial_ids(
    res$burden_estimate_expectation, e$resp_expectations,
      "burden_estimate_expectation", fields, fields)

  # Now we have burden estimate expectation ids, we can add countries
  # and outcomes. both can be semi-colon separated so need
  # exploding... but we might as well do these together.

  res$burden_estimate_country_expectation <- NULL
  res$burden_estimate_outcome_expectation <- NULL

  for (r in seq_len(nrow(rcsv))) {
    row_csv <- rcsv[r, ]
    row_bee <- res$burden_estimate_expectation[r, ]

    explode_countries <- split_semi(row_csv$countries)

    res$burden_estimate_country_expectation <- rbind(
      res$burden_estimate_country_expectation, data_frame(
        burden_estimate_expectation = row_bee$id,
        country = explode_countries
      )
    )

    explode_outcomes <- split_semi(row_csv$outcomes)
    res$burden_estimate_outcome_expectation <- rbind(
      res$burden_estimate_outcome_expectation, data_frame(
        burden_estimate_expectation = row_bee$id,
        outcome = explode_outcomes
      )
    )
  }

  # For now, assume none of these are in the db - we'll
  # check and filter in the load stage when we have a con.

  res$burden_estimate_country_expectation$already_exists_db <- FALSE
  res$burden_estimate_outcome_expectation$already_exists_db <- FALSE

  res
}

test_transform_responsibilities <- function(t) {
  # Nothing useful to do here.
}

###############################################################################

load_responsibilities <- function(transformed_data, con) {
  browser()
  res$burden_estimate_country_expectation <-
    set_unique_flag(con, res$burden_estimate_country_expectation,
                    "burden_estimate_country_expectation")
  res$burden_estimate_outcome_expectation <-
    set_unique_flag(con, res$burden_estimate_outcome_expectation,
                    "burden_estimate_outcome_expectation")

  #add_serial_rows("burden_estimate_expectation", transformed_data, con)
}
