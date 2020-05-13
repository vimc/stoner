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

extract_responsibilities_csv <- function() {
  csv <- read_meta(path, "responsibilities.csv")
  if (is.null(csv)) return (csv)

  # It will make things more pleasant to multiplying out
  # some of the semi-colons into a longer form here.

  for (field in c("scenario", "touchstone")) {

    while (any(grepl(csv[[field]], ";"))) {
      first <- which(grepl(csv[[field]], ";"))[1]
      first_row <- csv[first, ]
      items <- split_semi(first_row[[field]])
      csv[[field]][first] <- items[1]
      for (others in 2:length(items)) {
        first_row[[field]] <- items[others]
        first <- rbind(first, first_row)
      }
    }
  }

  # Note that all this multiplying out may have created some invalid
  # rows. (eg, mis-matched modelling_group and disease...) We'll
  # deal with that in the next part of the extract.

  csv
}

extract_responsibilities <- function(e, path, con) {

  # Deal with responsibilities csv being non-existent or empty

  if (is.null(e$responsibilities_csv)) return(NULL)
  if (nrow(e$responsibilities_csv) == 0) return(NULL)

  res <- list()

  # Look up all scenario data (touchstone, scenario_description)
  # that match those in the csv, so we know to not add them again.

  res$resp_scenarios <- DBI::dbGetQuery(con, sprintf("
      SELECT *
        FROM scenario
       WHERE CONCAT(touchstone, '\r', scenario_description) IN %s",
        sql_in(unique(paste(e$responsibilities_csv$touchstone,
                            e$responsibilities_csv$scenario, sep = '\r')))))

  # Countries can be left blank, in which case they'll be NA in the
  # csv file. Replace with "" so we can query, and look up all the
  # matching countries. Then non-matches must be invalid.

  e$responsibilities_csv$countries[
    is.na(e$responsibility_csv$countries)] <- ""

  all_countries <- unique(unlist(lapply(e$responsibilities_csv$countries,
                   split_semi)))

  res$resp_countries <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM country WHERE id IN %s",
        sql_in(all_countries)))

  # The same for outcomes - they can be omitted, and will turn up as NA.
  # Replace with "" and lookup.

  e$responsibilities_csv$outcomes[is.na(e$responsibilities_csv$outcomes)] <- ""
    all_outcomes <- unique(unlist(lapply(e$responsibilities_csv$outcomes,
                                          split_semi)))

  res$resp_outcomes <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM burden_outcome WHERE code IN %s",
        sql_in(all_outcomes)))

  # Now look up all modelling groups, so we can detect missing ones...

  res$resp_modelling_group <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM modelling_group WHERE id IN %s",
        sql_in(unique(e$responsibilities_csv$modelling_group))))

  # And look up diseases, to detect missing ones.

  res$resp_diseases <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM disease WHERE id IN %s",
        sql_in(unique(e$responsibilities_csv$disease))))

  # Look up all responsibility_set (modelling_group, touchstone)
  # that matches the csv.

  res$resp_responsibility_set <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM responsibility_set
       WHERE CONCAT(modelling_group, '\r', touchstone) IN %s",
         sql_in(unique(paste(e$responsibilities_csv$modelling_group,
                             e$responsibilities_csv$touchstone, sep = "\r")))))


  # Now look up all expectations that are in the existing responsibility_sets
  # and the responsibility rows too. Initialise with a zero row table, to
  # avoid some issues later...

  if (nrow(res$resp_responsibility_set) > 0) {
    responsibility_ids <- DBI::dbGetQuery(con, sprintf("
      SELECT id
        FROM responsibility
       WHERE responsibility_set IN %s", sql_in(res$resp_responsibility_set$id)))

    if (nrow(responsibility_ids) > 0) {
      res$resp_expectations <- DBI::dbGetQuery(con, sprintf("
        SELECT *
          FROM burden_estimate_expectation
         WHERE id IN %s", sql_in(responsibility_ids$id)))
    }

    res[['resp_responsibility']] <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM responsibility
      WHERE id IN %s", sql_in(responsibility_ids$id)))

  }

  # res$resp_expectations being null is awkward later... I'd prefer a
  # valid table with no rows.

  res$resp_expectations <- res$resp_expectations %||%
    DBI::dbGetQuery(con, sprintf("
      SELECT *
        FROM burden_estimate_expectation
       WHERE id = -1"))

  # And the same for res[['responsibility']]

  res[['resp_responsibility']] <- res[['resp_responsibility']] %||%
    DBI::dbGetQuery(con, sprintf("
      SELECT *
        FROM responsibility
       WHERE id = -1"))

  res
}

test_extract_responsibilities <- function(e) {
  ecsv <- e$responsibilities_csv
  if (is.null(ecsv)) return(NULL)
  if (nrow(ecsv) == 0) return(NULL)

  testthat::expect_equal(sort(names(ecsv)),
    c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
      "cohort_min_inclusive", "countries", "disease", "modelling_group",
      "outcomes", "scenario", "scenario_type", "touchstone",
      "year_max_inclusive", "year_min_inclusive"),
    label = "Columns in burden_estimate_expectation.csv")

  testthat::expect_type(ecsv$age_max_inclusive, "integer")
  testthat::expect_type(ecsv$age_min_inclusive, "integer")
  testthat::expect_type(ecsv$cohort_max_inclusive, "integer")
  testthat::expect_type(ecsv$cohort_min_inclusive, "integer")
  testthat::expect_type(ecsv$year_max_inclusive, "integer")
  testthat::expect_type(ecsv$year_min_inclusive, "integer")
  testthat::expect_type(ecsv$scenario_type, "character")

  if (any(ecsv$year_min_inclusive >
          ecsv$year_max_inclusive)) {
    stop("Responsibility year_min_inclusive must be before year_max_inclusive")
  }

  if (any(ecsv$age_min_inclusive >
          ecsv$age_max_inclusive)) {
    stop("Responsibility age_min_inclusive must be before age_max_inclusive")
  }

  if (any(ecsv$cohort_min_inclusive >
          ecsv$cohort_max_inclusive)) {
    stop(paste0("Responsibility cohort_min_inclusive must be before ",
                "cohort_max_inclusive"))
  }

  if (any(!is.na(ecsv$countries))) {
    all_countries <- ecsv$countries[!is.na(ecsv$countries)]
    all_countries <- sort(unique(unlist(lapply(all_countries, split_semi))))
    if (!all(all_countries %in% e$resp_countries$id)) {
      errs <- which(!all_countries %in% e$resp_countries$id)
      countries <- paste(all_countries[errs], sep = ", ")
      stop(sprintf("Unknown responsibility countries: %s",countries))
    }
  }

  if (any(!is.na(ecsv$outcomes))) {
    all_outcomes <- ecsv$outcomes[!is.na(ecsv$outcomes)]
    all_outcomes <- sort(unique(unlist(lapply(all_outcomes, split_semi))))
    if (!all(all_outcomes %in% e$resp_outcomes$code)) {
      errs <- which(!all_outcomes %in% e$resp_outcomes$code)
      outcomes <- paste(all_outcomes[errs], sep = ", ")
      stop(sprintf("Unknown responsibility outcomes: %s",outcomes))
    }
  }

  all_mgs <- sort(unique(ecsv$modelling_group))
  if (!all(all_mgs %in% e$resp_modelling_group$id)) {
    errs <- which(!all_mgs %in%
                   e$resp_modelling_group$id)
    groups <- paste(all_mgs[errs], sep = ", ")
    stop(sprintf("Unknown responsibility modelling_groups: %s", groups))
  }

  all_diseases <- sort(unique(ecsv$disease))
  if (!all(all_diseases %in% e$resp_diseases$id)) {
    errs <- which(!all_diseases %in% e$resp_diseases$id)
    diseases <- paste(all_diseases[errs], sep = ", ")
    stop(sprintf("Unknown responsibility diseases: %s", diseases))
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

  # So, build scenarios. Unique pairs of (touchstone, scenario_description)
  # Then assign ids to existing ones, negative ids to non-existent, and
  # set the already_in_db flag. (assign_serial_ids does those last things)

  unique_scenarios <- ecsv[!duplicated(
    paste(ecsv$touchstone, ecsv$scenario, sep = "\r")), ]

  res$scenario <- data_frame(
    touchstone = unique_scenarios$touchstone,
    scenario_description = unique_scenarios$scenario,
    focal_coverage_set = NA
  )

  fields <- c("touchstone", "scenario_description")
  res$scenario <- assign_serial_ids(res$scenario, e$resp_scenarios, "scenario",
                                    fields, fields)

  # Build responsiblities table -  (eventually id, responsibility_set, scenario,
  # current_burden_estimate_set, current_stochastic_burden_estimate_set,
  # is_open, and expectations.
  #
  # Start with non-changing stuff - NA
  # for burden_sets, and is_open = TRUE

  res$responsibility <- data_frame(
    is_open = rep(TRUE, nrow(ecsv)),
    current_burden_estimate_set = NA,
    current_stochastic_burden_estimate_set = NA)

  # Look up scenario ids from res$scenario

  res$responsibility$scenario <- res$scenario$id[
    match(paste(ecsv$touchstone, ecsv$scenario, sep = "\r"),
          paste(res$scenario$touchstone, res$scenario$scenario_description,
                sep = "\r"))]

  # Now look up/add any responsibility_sets (id, modelling_group, touchstone)
  # status for new rows will be "incomplete" (and other rows won't get added,
  # so it doesn't matter what their status is).

  res$responsibility_set <- data_frame(
    modelling_group = ecsv$modelling_group,
    touchstone = ecsv$touchstone,
    status = "incomplete")


  # Remove duplicate (modelling_group, touchstone)
  # Then look up existing ids; assign negatives for new ones.l

  res$responsibility_set <- res$responsibility_set[!duplicated(
    paste(res$responsibility_set$modelling_group,
          res$responsibility_set$touchstone, sep = '\r')), ]

  fields <- c("modelling_group", "touchstone")
  res$responsibility_set <- assign_serial_ids(
    res$responsibility_set, e$resp_responsibility_set, "responsibility_set",
    fields, fields)

  # Populate res$responsibility$responsibility_set with ids

  res$responsibility$responsibility_set <-
    res$responsibility_set$id[match(
      paste(ecsv$modelling_group, ecsv$touchstone, sep = '\r'),
      paste(res$responsibility_set$modelling_group,
            res$responsibility_set$touchstone, sep = '\r'))]

  # Next burden_estimate_expectation. The other tables (responsibility,
  # burden_estimate_country_expectation and
  # burden_estimate_outcome_expectation have foreign keys to
  # burden_estimate_expectation

  # Conventionally, the description in burden_estimate_expectation is
  # in the form disease:modelling_group:scenario_type - where scenario_type
  # is either all, bd, non_bd, standard - ie, where years, ages, countries
  # etc have been the same for different scenarios, they've been grouped
  # into one.

  ecsv$description <- paste(ecsv$disease, ecsv$modelling_group,
                            ecsv$scenario_type, sep = ':')
  ecsv$version <- e$touchstone$touchstone_name[match(
    ecsv$touchstone, e$touchstone$id)]

  res$burden_estimate_expectation <- ecsv[,
    c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
      "cohort_min_inclusive", "year_max_inclusive", "year_min_inclusive",
      "description", "version")]


  # Remove any duplicate expectations.

  res$burden_estimate_expectation <- res$burden_estimate_expectation[
    !duplicated(paste(
      res$burden_estimate_expectation$age_max_inclusive,
      res$burden_estimate_expectation$age_min_inclusive,
      res$burden_estimate_expectation$cohort_max_inclusive,
      res$burden_estimate_expectation$cohort_min_inclusive,
      res$burden_estimate_expectation$year_max_inclusive,
      res$burden_estimate_expectation$year_min_inclusive,
      res$burden_estimate_expectation$description,
      res$burden_estimate_expectation$version, sep = '\r')), ]

  # Now lookup any existing serials, or assign negative ones.

  fields <- c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
              "cohort_min_inclusive", "year_max_inclusive",
              "year_min_inclusive", "description", "version")

  res$burden_estimate_expectation <- assign_serial_ids(
    res$burden_estimate_expectation, e$resp_expectations,
      "burden_estimate_expectation", fields, fields)

  message("BEE ids:")
  message(paste(res$burden_estimate_expectation$id, collapse=","))

  # And now assign the expectation id to res$responsibility, which will
  # be a bit messy - multi-column match between the expectation details
  # in ecsv, and those we just made in res$burden_estimate_expectation

  res$responsibility$expectations <-
    res$burden_estimate_expectation$id[match(
      paste(ecsv$age_max_inclusive,
            ecsv$age_min_inclusive,
            ecsv$cohort_max_inclusive,
            ecsv$cohort_min_inclusive,
            ecsv$year_max_inclusive,
            ecsv$year_min_inclusive,
            ecsv$description,
            ecsv$version, sep = '\r'),
      paste(res$burden_estimate_expectation$age_max_inclusive,
            res$burden_estimate_expectation$age_min_inclusive,
            res$burden_estimate_expectation$cohort_max_inclusive,
            res$burden_estimate_expectation$cohort_min_inclusive,
            res$burden_estimate_expectation$year_max_inclusive,
            res$burden_estimate_expectation$year_min_inclusive,
            res$burden_estimate_expectation$description,
            res$burden_estimate_expectation$version, sep = '\r'))]

  # res$responsibility now has all the fields, except the id, so look up
  # to see if they exist, or assign negative ids otherwise. Using [[' ']]
  # here rather than $ because we have responsibility_set as well as
  # responsibility, and we don't want autocomplete to happen.

  fields <- c("responsibility_set", "scenario", "expectations")
  res$responsibility <- assign_serial_ids(res[['responsibility']],
                                          e[['resp_responsibility']],
                                          "responsibility", fields, fields)

  # Now we have burden estimate expectation ids, we can add countries
  # and outcomes. both can be semi-colon separated so need
  # exploding... but we might as well do these together, as the functionality
  # is very similar.

  res$burden_estimate_country_expectation <- NULL
  res$burden_estimate_outcome_expectation <- NULL

  for (r in seq_len(nrow(ecsv))) {
    row_csv <- ecsv[r, ]
    exp_id <- res$burden_estimate_expectation$id[match(
        paste(row_csv$age_max_inclusive,
              row_csv$age_min_inclusive,
              row_csv$cohort_max_inclusive,
              row_csv$cohort_min_inclusive,
              row_csv$year_max_inclusive,
              row_csv$year_min_inclusive,
              row_csv$description,
              row_csv$version, sep = '\r'),
        paste(res$burden_estimate_expectation$age_max_inclusive,
              res$burden_estimate_expectation$age_min_inclusive,
              res$burden_estimate_expectation$cohort_max_inclusive,
              res$burden_estimate_expectation$cohort_min_inclusive,
              res$burden_estimate_expectation$year_max_inclusive,
              res$burden_estimate_expectation$year_min_inclusive,
              res$burden_estimate_expectation$description,
              res$burden_estimate_expectation$version, sep = '\r'))]

    if (!is.na(row_csv$countries)) {
      explode_countries <- split_semi(row_csv$countries)

      res$burden_estimate_country_expectation <- rbind(
        res$burden_estimate_country_expectation, data_frame(
          burden_estimate_expectation = exp_id,
          country = explode_countries
        )
      )
    }

    if (!is.na(row_csv$outcomes)) {
      explode_outcomes <- split_semi(row_csv$outcomes)
      res$burden_estimate_outcome_expectation <- rbind(
        res$burden_estimate_outcome_expectation, data_frame(
          burden_estimate_expectation = exp_id,
          outcome = explode_outcomes
        )
      )
    }
  }

  # For now, I'm going to assume none of these are in the db - we'll
  # check and filter in the load stage when we have a con. I should
  # really have done this in the extract phase, but I
  # think it's not worth it - nasty query to lookup the expectation id
  # to query these two tables with.

  # Also, we might not have anything to add, in which case these
  # two might be null.

  if (!is.null(res$burden_estimate_country_expectation)) {
    res$burden_estimate_country_expectation$already_exists_db <- FALSE
  }

  if (!is.null(res$burden_estimate_outcome_expectation)) {
    res$burden_estimate_outcome_expectation$already_exists_db <- FALSE
  }

  # Final duplicate removal (this will do countries and outcomes, since
  # we don't need to select particular fields - we want the whole rows
  # to be non-duplicates.

  res <- lapply(res, function(x) x[!duplicated(x), ])
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
