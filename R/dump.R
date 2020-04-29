#' Query a database for all information relevant to a particular touchstone
#' (including version), and write to CSV files, which would then be fit for
#' stoner to use as its input to re-population the database with the same
#' touchstone. This would be useful in a workflow where you wanted to
#' clone a touchstone, alter a few things, and upload as a new version.
#'
#' Note that this is not "lossless"; only things that Stoner has the capability
#' of adding, will be stored in the CSVs. For example (at present), Stoner
#' does nothing with coverage sets, so while the scenario_descriptions will
#' be saved (since they can be re-added),
#' scenario_description.focal_coverage_set will not be dumped, because
#' Stoner cannot put it back. Coverage sets will need to be wired up
#' separately after a Stoner import, as before.

#'
#' @title Dump touchstone from database to CSV.
#' @export
#' @param con Active connection to the DB which the data will be dumped from.
#' @param touchstone the touchstone including version.
#' @param path the path where output CSVs should be created.
#' @param include_deps also dump db dependencies that Stoner can't
#' currently add - db_disease.csv, db_modelling_group.csv,
#' db_demographic_statistic_type.csv and db_demographic_source.csv.

stone_dump <- function(con, touchstone, path, include_deps = FALSE) {

  write_csv <- function(...) {
    write.csv(row.names = FALSE, quote = FALSE, ...)
  }

  dump_extra <- function(table, id_field, vals, remove_serial) {
    data <- DBI::dbGetQuery(con, sprintf("
        SELECT * FROM %s
         WHERE %s IN %s", table, id_field, sql_in(vals)))
    if (!is.null(remove_serial)) data[[remove_serial]] <- NULL
    if (nrow(data) == 0) return()
    write_csv(data, file.path(path, paste0("db_", table, ".csv")))
  }

  #########################################################################

  dump_touchstone <- function() {
    csv_touchstone <- DBI::dbGetQuery(con, "
      SELECT *
        FROM touchstone
       WHERE id = $1", touchstone)

    if (nrow(csv_touchstone) == 0) {
      stop(sprintf("Touchstone not found - %s", touchstone))
    }

    csv_touchstone_name <- DBI::dbGetQuery(con, "
      SELECT *
        FROM touchstone_name
       WHERE id = $1", csv_touchstone$touchstone_name)


    write_csv(csv_touchstone, file.path(path, "touchstone.csv"))
    write_csv(csv_touchstone_name, file.path(path, "touchstone_name.csv"))
  }

  #########################################################################

  dump_scenario_description <- function() {

    # Note that focal_coverage_set info is not dumped, since
    # Stoner can't currently do anything with it.

    csv_scenarios <- DBI::dbGetQuery(con, "
      SELECT DISTINCT scenario_description AS id,
                      description, disease
        FROM scenario
        JOIN scenario_description
          ON scenario.scenario_description = scenario_description.id
       WHERE touchstone = $1", touchstone)

    if (nrow(csv_scenarios)>0)
      write_csv(csv_scenarios, file.path(path, "scenario_description.csv"))
  }

  #########################################################################

  dump_touchstone_countries <- function() {

    # CSV format:
    # COl names: touchstone,disease, country
    # where disease and country can both be semi-colon separated list.

    touchstone_countries <- DBI::dbGetQuery(con, "
      SELECT touchstone, country, disease
        FROM touchstone_country
       WHERE touchstone = $1
       ORDER BY disease, country", touchstone)

    if (nrow(touchstone_countries) == 0) return()

    # This will be hundreds of rows, so firstly factorise by disease...

    diseases <- unique(touchstone_countries$disease)

    if (include_deps) dump_extra("disease", "id", diseases, NULL)

    tc_csv <- lapply(diseases, function(x) {
      touchstone_countries[touchstone_countries$disease == x, ] })

    tc_csv <- data_frame(
      disease = diseases,
      country = unlist(lapply(tc_csv,
                              function(x) paste(x$country, collapse = ";")))
    )

    # And we may have the same set of countries for the same diseases.
    # Combine these into disease1;disease2...,country1;country2...

    while (any(duplicated(tc_csv$country))) {
      dup_row <- which(duplicated(tc_csv$country))[1]
      dup_of <- which(tc_csv$country == tc_csv$country[dup_row])[1]
      tc_csv$disease[dup_of] <- paste(tc_csv$disease[dup_of],
                                      tc_csv$disease[dup_row], sep = ";")
      tc_csv <- tc_csv[tc_csv$disease != tc_csv$disease[dup_row], ]
    }

    tc_csv$touchstone <- touchstone

    if (nrow(tc_csv) > 0) {
      write_csv(tc_csv, file.path(path, "touchstone_country.csv"))
    }
  }

  #########################################################################

  dump_touchstone_demographic_datasets <- function() {

    # Format we want:
    # touchstone, demographic_source, demographic_statistic_type
    # where these are friendly strings, not ids.

    tdd <- DBI::dbGetQuery(con, "
      SELECT touchstone, demographic_source.code AS demographic_source,
             demographic_statistic_type.code AS demographic_statistic_type
        FROM touchstone_demographic_dataset
        JOIN demographic_dataset
          ON touchstone_demographic_dataset.demographic_dataset =
               demographic_dataset.id
        JOIN demographic_source
          ON demographic_dataset.demographic_source =
               demographic_source.id
        JOIN demographic_statistic_type
          ON demographic_dataset.demographic_statistic_type =
              demographic_statistic_type.id
       WHERE touchstone = $1", touchstone)

    if (include_deps) {
      dump_extra("demographic_statistic_type", "code",
                 tdd$demographic_statistic_type, "id")
      dump_extra("demographic_source", "code",
                 unique(tdd$demographic_source), "id")
    }

    if (nrow(tdd) == 0) return()
    write_csv(tdd, file.path(path, "touchstone_demographic_dataset.csv"))
  }

  #########################################################################

  dump_responsibilities <- function() {

    # Now... there are multiple rows in the responsibility table that
    # have the same expectations id. The only difference we care
    # about here between those rows is the scenario id, which in our
    # Stoner CSV can be semi-coloned together into a single line.

    expecs <- DBI::dbGetQuery(con, "
      SELECT burden_estimate_expectation.id AS expecid,
             age_min_inclusive, age_max_inclusive,
             cohort_min_inclusive, cohort_max_inclusive,
             year_min_inclusive, year_max_inclusive,
             burden_estimate_expectation.description AS expecdesc,
             modelling_group,
             scenario.scenario_description AS scenario,
             disease
        FROM burden_estimate_expectation
        JOIN responsibility
          ON responsibility.expectations = burden_estimate_expectation.id
        JOIN responsibility_set
          ON responsibility_set.id = responsibility.responsibility_set
        JOIN scenario
          ON scenario.id = responsibility.scenario
        JOIN scenario_description
          ON scenario_description.id = scenario.scenario_description
       WHERE responsibility_set.touchstone = $1", touchstone)

    # Need to extract the "scenario type" out of the expectation description,
    # which (should/will) always be in the form disease:modelling_group:type

    expecs$scenario_type <-
      unlist(lapply(strsplit(expecs$expecdesc, ":"), "[[", 3))

    if (nrow(expecs) == 0) return()

    # Now build the CSV in a nice column order, combining scenarios...

    csv <- data_frame(
      expecid = sort(unique(expecs$expecid)),
      modelling_group = "",
      disease = "",
      scenario = "",
      scenario_type = "",
      age_min_inclusive = 0,
      age_max_inclusive = 0,
      cohort_min_inclusive = 0,
      cohort_max_inclusive = 0,
      year_min_inclusive = 0,
      year_max_inclusive = 0,
      countries = "",
      outcomes = "")

    csv$scenario <- unlist(lapply(seq_len(nrow(csv)), function(x) {
      paste(sort(expecs$scenario[expecs$expecid == x]), collapse = ';')}))

    ensure_same <- function(expecs, csv, name) {
      unlist(lapply(seq_len(nrow(csv)), function(x) {
        vals <- expecs[[name]][expecs$expecid == csv$expecid[x]]
        stopifnot(sum(duplicated(vals)) == length(vals) - 1)
        vals[1]
      }))
    }

    csv$modelling_group <- ensure_same(expecs, csv, "modelling_group")
    csv$disease <- ensure_same(expecs, csv, "disease")

    csv$age_min_inclusive <-
      as.integer(ensure_same(expecs, csv, "age_min_inclusive"))
    csv$age_max_inclusive <-
      as.integer(ensure_same(expecs, csv, "age_max_inclusive"))
    csv$year_min_inclusive <-
      as.integer(ensure_same(expecs, csv, "year_min_inclusive"))
    csv$year_max_inclusive <-
      as.integer(ensure_same(expecs, csv, "year_max_inclusive"))
    csv$cohort_min_inclusive <-
      as.integer(ensure_same(expecs, csv, "cohort_min_inclusive"))
    csv$cohort_max_inclusive <-
      as.integer(ensure_same(expecs, csv, "cohort_max_inclusive"))

    csv$scenario_type <- ensure_same(expecs, csv, "scenario_type")

    # Build countries and outcomes into semi-colon separated list

    countries <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM burden_estimate_country_expectation
       WHERE burden_estimate_expectation IN %s", sql_in(expecs$expecid)))

    outcomes <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM burden_estimate_outcome_expectation
       WHERE burden_estimate_expectation IN %s", sql_in(expecs$expecid)))

    csv$countries <- unlist(lapply(seq_len(nrow(csv)), function(x) {
      paste(
        sort(countries$country[countries$burden_estimate_expectation == x]),
        collapse = ';')
    }))

    csv$outcomes <- unlist(lapply(seq_len(nrow(csv)), function(x) {
      paste(
        sort(outcomes$outcome[outcomes$burden_estimate_expectation == x]),
        collapse = ';')
    }))

    # Done. Clean up.

    csv$expecid <- NULL
    csv$touchstone <- touchstone
    if (include_deps) {
      dump_extra("modelling_group", "id", unique(csv$modelling_group), NULL)
    }

    write_csv(csv, file.path(path, "responsibilities.csv"))

  }

  #########################################################################
  #########################################################################

  dump_touchstone()
  dump_scenario_description()
  dump_touchstone_countries()
  dump_touchstone_demographic_datasets()
  dump_responsibilities()
  invisible(NULL)
}
