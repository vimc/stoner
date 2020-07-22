##' Convert a modelling group's stochastic files into the summary format,
##' ready for later uploading to the Montagu data annex. Four files are
##' produced which reduce age to all-age-total, and under-5-total, by
##' calendar year, or birth-cohort year.
##'
##' @export
##' @title Process stochastic data
##' @param con DBI connection to production. Used for verifying certificate
##' against expected properties
##' @param modelling_group The modelling group id
##' @param disease The disease
##' @param touchstone The touchstone (including version) for these estimates
##' @param scenarios A vector of scenario_descriptions. If the files
##' parameter is of length more than 1, then it must be the same length as
##' the number of scenarios, and a one-to-one mapping between the two is
##' assumed.
##' @param path The folder containing the stochastic files
##' @param files Either a single string containing placeholders to indicate
##' filenames, or a vector of files, one for each scenario. Placeholders can
##' include :group :touchstone :scenario :disease and :index
##' @param cert Name of the certificate file accompanying the estimates
##' @param index_start A scalar or vector matching the length of scenarios.
##' Each entry is either an integer or NA, indicating the first number in a
##' sequence of files. NA implies there is a single file with no sequence.
##' The placeholder :index in the filenames will be replaced with this.
##' @param index_end Similar to index_start, indicating the last number
##' in a sequence of a files. Can be scalar, applying to all scenarios, or
##' a vector with an entry for each scenario, with an integer value or NA
##' in each case.
##' @return A list of named data frames and/or named values representing the extracted data.
stone_stochastic_process <- function(con, modelling_group, disease,
                                     touchstone, scenarios, path, files,
                                     cert, index_start, index_end) {

  # Do all the parameter and file testing upfront, as this can be a very
  # time-consuming process...

  assert_connection(con)

  assert_scalar_character(modelling_group)
  if (!db_exists(con, "modelling_group", "id", modelling_group)) {
    stop(sprintf("Unknown modelling group: %s", modelling_group))
  }

  assert_scalar_character(disease)
  if (!db_exists(con, "disease", "id", disease)) {
    stop(sprintf("Unknown disease: %s", disease))
  }

  assert_scalar_character(touchstone)
  if (!db_exists(con, "touchstone", "id", touchstone)) {
    stop(sprintf("Unknown touchstone: %s", touchstone))
  }

  # Scenario-specific tests:
  # 1. (touchstone, scenario_description) exists in scenario table
  # 2. (scenario_description, disease) exists in scenario_description table
  # 3. (touchstone, modelling_group) exists in responsibility_set table
  # 4. (responsibility_set, scenario) exist in responsibility, where
  #    scenario id comes from #1, and responsibility_set id from #3

  for (scenario in scenarios) {
    scenario_id <- DBI::dbGetQuery(con, "
      SELECT id FROM scenario WHERE touchstone = $1
         AND scenario_description = $2", list(touchstone, scenario))$id
    if (length(scenario_id) != 1) {
      stop(sprintf("scenario %s not found in touchstone %s",
                   scenario, touchstone))
    }

    scenario_descs <- DBI::dbGetQuery(con, "
      SELECT count(*) FROM scenario_description WHERE id = $1
         AND disease = $2", list(scenario, disease))$count
    if (scenario_descs != 1) {
      stop(sprintf("scenario_description %s not valid for disease %s",
                   scenario, disease))
    }

    respset_id <- DBI::dbGetQuery(con, "
      SELECT id FROM responsibility_set WHERE touchstone = $1
         AND modelling_group = $2", list(touchstone, modelling_group))$id
    if (length(respset_id) != 1) {
      stop(sprintf("No responsibility_set for group %s in touchstone %s",
                   modelling_group, touchstone))
    }

    resp_id <- DBI::dbGetQuery(con, "
      SELECT id FROM responsibility WHERE responsibility_set = $1
         AND scenario = $2", list(respset_id, scenario_id))$id
    if (length(resp_id) != 1) {
      stop(sprintf("No responsibility for group %s, scenario %s, touchstone %s",
                   modelling_group, scenario, touchstone))
    }
  }

  # Possibly, we could check that all scenarios are included, but
  # the exceptions are the groups that have to do a VIS report, as they
  # have several additional scenarios for which they provide central but
  # not stochastic estimates. So not sure if this is so easy to insist on.

  assert_scalar_character(path)
  if (!file.exists(path)) {
    stop(sprintf("Input path not found: ", path))
  }
  stopifnot(file.exists(path))

  # Check number of file patterns is 1 for all, or 1 per scenario

  if (!(length(files) %in% c(1, length(scenarios)))) {
    stop(sprintf("Incorrect files param - length should be 1 or %s",
         length(scenarios)))
  }

  # index_start and index_end are either NA, or a single integer,
  # or a vector of integer for each scenario

  if (!(length(index_start) %in% c(1, length(scenarios)))) {
    stop(sprintf("Incorrect index_start - can be NA, or length 1 or %s",
                 length(scenarios)))
  }

  if (!(length(index_end) %in% c(1, length(scenarios)))) {
    stop(sprintf("Incorrect index_end - can be NA, or length 1 or %s",
                 length(scenarios)))
  }

  # Check index_start and index_end are all integer or NA

  all_na_or_int <- function(x) {
    is.numeric(x) || all(is.na(x))
  }

  if (!all_na_or_int(index_start)) {
    stop("index_start must be all NA or integers")
  }

  if (!all_na_or_int(index_end)) {
    stop("index_end must be all NA or integers")
  }

  # Where index_start is NA, index_end must also be NA

  if (!identical(which(is.na(index_start)),
                 which(is.na(index_end)))) {
    stop("Mismatches of NA between index_start and index_end")
  }

  # Check whenever index_start is !NA, files contains :index
  # (and the converse also holds)

  if (length(files) == 1) {
    all_files <- rep(files, length(scenario))
  } else {
    all_files <- files
  }

  if (!identical(grepl(":index", all_files), !is.na(index_start))) {
    stop("Mismatch between NA in index_start, and :index placeholder in files")
  }

  for (scenario_no in seq_along(scenarios)) {
    scenario <- scenarios[scenario_no]
    if (length(index_start) != 1) {
      index_from <- index_start[scenario_no]
      index_to < index_end[scenario_no]
    } else {
      index_from <- index_start
      index_to <- index_end
    }

    if (is.na(index_from)) {
      index_from <- 1
      index_to <- 1
    }

    for (i in index_from:index_to) {

      one_file <- all_files[scenario_no]
      one_file <- gsub(":index", i, one_file)
      one_file <- gsub(":group", modelling_group, one_file)
      one_file <- gsub(":touchstone", touchstone, one_file)
      one_file <- gsub(":disease", disease, one_file)
      one_file <- gsub(":scenario", scenario, one_file)
      the_file <- file.path(path, one_file)
      if (!file.exists(the_file)) {
        stop(sprintf("File not found: %s", the_file))
      }
    }
  }

  # We are now as tested as we can get at this point.
  ####################################################

}
