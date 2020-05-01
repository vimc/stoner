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

  ts <- "non_existent_touchstone"
  if (length(res$resp_touchstone) > 0) {
    ts <- res$resp_touchstone
  }

  res$resp_expectations <- DBI::dbGetQuery(con, sprintf("
    SELECT * FROM burden_estimate_expectation
       WHERE version IN %s",
         sql_in(ts)))

  rsid <- (-1)
  if (length(res$resp_responsibility_set$id) >0) {
    rsid <- res$resp_responsibility_set$id
  }

  res[['resp_responsibility']] <- DBI::dbGetQuery(con, sprintf("
    SELECT * FROM responsibility
    WHERE responsibility_set IN %s", sql_in(rsid)))

  res
}

test_extract_responsibilities <- function(e) {
  ecsv <- e$responsibilities_csv
  if (is.null(ecsv)) return(NULL)
  if (nrow(ecsv) == 0) return(NULL)

  expect_equal(sort(names(ecsv)),
    c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
      "cohort_min_inclusive", "countries", "description", "modelling_group",
      "outcomes", "scenario", "scenario_type", "touchstone",
      "year_max_inclusive", "year_min_inclusive"),
    label = "Columns in burden_estimate_expectation.csv")

  expect_type(ecsv$age_max_inclusive, "integer")
  expect_type(ecsv$age_min_inclusive, "integer")
  expect_type(ecsv$cohort_max_inclusive, "integer")
  expect_type(ecsv$cohort_min_inclusive, "integer")
  expect_type(ecsv$year_max_inclusive, "integer")
  expect_type(ecsv$year_min_inclusive, "integer")
  expect_type(ecsv$scenario_type, "character")

  if (any(ecsv$year_min_inclusive >
          ecsv$year_max_inclusive)) {
    stop("Responsibility year_min_inclusive must be before year_max_inclusive")
  }

  if (any(e$ecsv$age_min_inclusive >
          e$ecsv$age_max_inclusive)) {
    stop("Responsibility age_min_inclusive must be before age_max_inclusive")
  }

  if (any(e$ecsv$cohort_min_inclusive >
          e$ecsv$cohort_max_inclusive)) {
    stop(paste0("Responsibility cohort_min_inclusive must be before ",
                "cohort_max_inclusive"))
  }

  if (!all(e$ecsv$countries %in% e$resp_countries$id)) {
    errs <- which(!e$ecsv$countries %in% e$resp_countries$id)
    countries <- paste(e$ecsv$countries[errs], sep = ", ")
    stop(sprintf("Unknown responsibility countries: %s",countries))
  }

  if (!all(e$ecsv$modelling_group %in%
           e$resp_modelling_group$id)) {
    errs <- which(!e$ecsv$modelling_group %in%
                   e$resp_modelling_group$id)
    groups <- paste(e$ecsv$modelling_group[errs], sep = ", ")
    stop(sprintf("Unknown responsibility modelling_groups: %s", groups))
  }

}

###############################################################################

transform_responsibilities <- function(e, t_so_far) {
  ecsv <- e$responsibilities_csv
  if (is.null(ecsv)) return(list())
  if (nrow(ecsv) == 0) return(list())

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
  res$responsibility <- NULL

  for (r in seq_len(nrow(ecsv))) {
    row <- ecsv[r, ]

    explode_scenarios <- split_semi(row$scenario)

    res$scenario <- rbind(res$scenario, data_frame(
      touchstone = row$touchstone,
      scenario_description = explode_scenarios))

    # Note that expectations below is for now the row number in
    # the csv file. It will be updated to the correct id later
    # when the expectations are created or looked up.

    res$responsibility <- rbind(res$responsibility, data_frame(
      responsibility_set = NA,
      scenario = explode_scenarios,
      current_burden_estimate_set = NA,
      current_stochastic_burden_estimate_set = NA,
      is_open = TRUE,
      expectations = r
    ))
  }

  fields <- c("touchstone", "scenario_description")
  res$scenario <- assign_serial_ids(res$scenario, e$resp_scenarios, "scenario",
                                    fields, fields)
  res$responsibility$scenario <- res$scenario$id[
    match(res$responsibility$scenario, res$scenario$scenario_description)]

  # Now look up/add any responsibility_set
  # (modelling_group, touchstone)

  res$responsibility_set <- data_frame(
    modelling_group = ecsv$modelling_group,
    touchstone = ecsv$touchstone,
    status = "incomplete")

  fields <- c("modelling_group", "touchstone")
  res$responsibility_set <- assign_serial_ids(
    res$responsibility_set, e$resp_responsibility_set, "responsibility_set",
    fields, fields)

  # Population res$responsibility$responsibility_set with ids

  for (r in seq_len(nrow(ecsv))) {
    row <- ecsv[r, ]
    res$responsibility$responsibility_set[
      res$responsibility$expectations == r] <-
        res$responsibility_set$id[
          res$responsibility_set$modelling_group == row$modelling_group &
          res$responsibility_set$touchstone == row$touchstone]
  }

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
     age_max_inclusive = ecsv$age_max_inclusive,
     age_min_inclusive = ecsv$age_min_inclusive,
  cohort_max_inclusive = ecsv$cohort_max_inclusive,
  cohort_min_inclusive = ecsv$cohort_min_inclusive,
    year_max_inclusive = ecsv$year_max_inclusive,
    year_min_inclusive = ecsv$year_min_inclusive,
           description = ecsv$description,
               version = e$touchstone$touchstone_name[match(
                           ecsv$touchstone, e$touchstone$id)]
  )

  fields <- c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
              "cohort_min_inclusive", "year_max_inclusive",
              "year_min_inclusive", "description", "version")

  res$burden_estimate_expectation <- assign_serial_ids(
    res$burden_estimate_expectation, e$resp_expectations,
      "burden_estimate_expectation", fields, fields)

  res$responsibility$expectations <-
    res$burden_estimate_expectation$id[res$responsibility$expectations]

  fields <- c("responsibility_set", "scenario")
  res$responsibility <- assign_serial_ids(res$responsibility,
                                          e$resp_responsibility,
                                          "responsibility", fields, fields)

  # Now we have burden estimate expectation ids, we can add countries
  # and outcomes. both can be semi-colon separated so need
  # exploding... but we might as well do these together.

  res$burden_estimate_country_expectation <- NULL
  res$burden_estimate_outcome_expectation <- NULL

  for (r in seq_len(nrow(ecsv))) {
    row_csv <- ecsv[r, ]
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

  load_scenarios <- function(t, con) {
    if (is.null(t[['scenario']])) {
      return(t)
    }

    if (nrow(t$scenario) == 0) {
      return(t)
    }

    res <- add_serial_rows("scenario", t, con)

    # Replace fake ids with real ids in responsibility.scenario

    negs <- which(t[['responsibility']]$scenario < 0)

    t$responsibility$scenario[negs] <-res$adds$id[match(
      t$responsibility$scenario[negs],
      res$adds$fake_ids)]

    t
  }

  ###################################################

  load_responsibility_sets <- function(t, con) {

    if (is.null(t$responsibility_set)) {
      return(t)
    }

    if (nrow(t$responsibility_set) == 0) {
      return(t)
    }
    res <- add_serial_rows("responsibility_set", t, con)

    # Replace fake ids with real ids in responsibility.scenario

    negs <- which(t$responsibility$responsibility_set < 0)

    t[['responsibility']]$responsibility_set[negs] <- res$adds$id[match(
                  t[['responsibility']]$responsibility_set[negs],
                  res$adds$fake_ids)]

    t
  }

  # Add burden_estimate_expectations
  # Update responsibility.expectations with ids
  # Update burden_estimate_country_expectations with ids

  load_expectations <- function(t, con) {
    if (is.null(t$burden_estimate_expectation)) {
      return(t)
    }

    if (nrow(t$burden_estimate_expectation) == 0) {
      return(t)
    }

    res <- add_serial_rows("burden_estimate_expectation", t, con)

    # Replace fake ids with real ids in
    # responsibility.expectation
    # burden_estimate_country_expectation.burden_estimate_expectation
    # burden_estimate_outcome_expectation.burden_estimate_expectation

    negs <- which(
      t[['responsibility']]$expectations < 0)

    t[['responsibility']]$expectations[negs] <-
      res$adds$id[match(
        t[['responsibility']]$expectations[negs],
        res$adds$fake_ids)]

    negs <- which(
      t$burden_estimate_country_expectation$burden_estimate_expectation < 0)

    t$burden_estimate_country_expectation$burden_estimate_expectation[negs] <-
      res$adds$id[match(
        t$burden_estimate_country_expectation$burden_estimate_expectation[negs],
        res$adds$fake_ids)]

    negs <- which(
      t$burden_estimate_outcome_expectation$burden_estimate_expectation < 0)

    t$burden_estimate_outcome_expectation$burden_estimate_expectation[negs] <-
      res$adds$id[match(
        t$burden_estimate_outcome_expectation$burden_estimate_expectation[negs],
        res$adds$fake_ids)]

    t
  }

  # Update burden_estimate_country_expectation.burden_estimate_expectation id
  load_exp_countries <- function(t, con) {
    if (is.null(t$burden_estimate_country_expectation)) {
      return(t)
    }
    if (nrow(t$burden_estimate_country_expectation) == 0) {
      return(t)
    }

    t$burden_estimate_country_expectation <-
      set_unique_flag(con, t$burden_estimate_country_expectation,
                      "burden_estimate_country_expectation")

    t$burden_estimate_country_expectation <-
      t$burden_estimate_country_expectation[
        !t$burden_estimate_country_expectation$already_exists_db, ]

    t$burden_estimate_country_expectation$already_exists_db <- NULL

    DBI::dbAppendTable(con, "burden_estimate_country_expectation",
      t$burden_estimate_country_expectation)

    t
  }

  # Update burden_estimate_outcome_expectation.burden_estimate_expectation id
  load_exp_outcomes <- function(t, con) {
    if (is.null(t$burden_estimate_outcome_expectation)) {
      return(t)
    }
    if (nrow(t$burden_estimate_outcome_expectation) == 0) {
      return(t)
    }

    t$burden_estimate_outcome_expectation <-
      set_unique_flag(con, t$burden_estimate_outcome_expectation,
                      "burden_estimate_outcome_expectation")
    t$burden_estimate_outcome_expectation <-
      t$burden_estimate_outcome_expectation[
        !t$burden_estimate_outcome_expectation$already_exists_db, ]

    t$burden_estimate_outcome_expectation$already_exists_db <- NULL

    DBI::dbAppendTable(con, "burden_estimate_outcome_expectation",
                       t$burden_estimate_outcome_expectation)

    t
  }

  #################################################

  load_responsibility <- function(t, con) {
    res <- add_serial_rows("responsibility", t, con)
    t
  }

  ####################################################################

  transformed_data <- load_scenarios(transformed_data, con)
  transformed_data <- load_responsibility_sets(transformed_data, con)
  transformed_data <- load_expectations(transformed_data, con)
  transformed_data <- load_exp_countries(transformed_data, con)
  transformed_data <- load_exp_outcomes(transformed_data, con)
  transformed_data <- load_responsibility(transformed_data, con)

}
