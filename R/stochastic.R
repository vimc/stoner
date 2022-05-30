stochastic_process_validate <- function(con, touchpoint, scenarios, in_path,
                                        files, index_start, index_end,
                                        outcomes,
                                        runid_from_file,
                                        upload_to_annex,
                                        annex,
                                        cert, bypass_cert_check) {
  assert_connection(con)
  if (upload_to_annex) {
    assert_connection(annex)
  }
  assert_scalar_character(touchpoint$modelling_group)
  assert_db_value_exists(con, "modelling_group", "id", touchpoint$modelling_group)
  assert_scalar_character(touchpoint$disease)
  assert_db_value_exists(con, "disease", "id", touchpoint$disease)
  assert_scalar_character(touchpoint$touchstone)
  assert_db_value_exists(con, "touchstone", "id", touchpoint$touchstone)

  if (is.data.frame(dalys)) {
    stopifnot(all.equal(sort(names(dalys)),
                        c("average_duration", "disability_weight", "outcome", "proportion")))
  }

  assert_scalar_character(in_path)
  if (!file.exists(in_path)) {
    stop(sprintf("Input path not found: %s", in_path))
  }
  stopifnot(file.exists(in_path))

  # Certificate check (if enabled).
  if (!is.na(cert)) {
    cert <- file.path(in_path, cert)
  }

  if (!bypass_cert_check) {
    if (!file.exists(cert)) {
      stop(sprintf("Certificate not found: %s", cert))
    }
    stone_stochastic_cert_verify(con, cert, touchpoint$modelling_group,
                                 touchpoint$touchstone, touchpoint$disease)
  }

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
    files <- rep(files, length(scenarios))
  }

  if (length(index_start) == 1) {
    index_start <- rep(index_start, length(files))
  }

  if (length(index_end) == 1) {
    index_end <- rep(index_end, length(files))
  }

  if (!identical(grepl(":index", files), !is.na(index_start))) {
    stop("Mismatch between NA in index_start, and :index placeholder in files")
  }

  if (runid_from_file) {
    if (any(index_start != 1) || any(index_end != 200)) {
      stop("Must have index_start and index_end as 1..200 to imply run_id")
    }
  }

  for (scenario in scenarios) {
    stochastic_validate_scenario(con, touchpoint$touchstone, scenario,
                                 touchpoint$disease,
                                 touchpoint$modelling_group)
  }

  check_outcomes(con, "cases", outcomes$cases)
  check_outcomes(con, "deaths", outcomes$deaths)
  if (is.data.frame(outcomes$dalys)) {
    check_outcomes(con, "dalys", unique(outcomes$dalys$outcome))
  } else {
    check_outcomes(con, "dalys", outcomes$dalys)
  }

  list(
    files = file.path(in_path, files),
    index_start = index_start,
    index_end = index_end
  )
}


check_outcomes <- function(con, type, options) {
  assert_character(options)
  if (any(duplicated(options))) {
    stop(sprintf("Duplicated outcome in %s", type))
  }
  res <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM burden_outcome
        WHERE code IN %s", sql_in(options)))

  missing <- options[!options %in% res$code]

  if (length(missing) > 0) {
    stop(sprintf("Outcomes not found, %s %s",
                 type, sql_in(missing)))
  }
  invisible(TRUE)
}

stochastic_validate_scenario <- function(con, touchstone, scenario, disease,
                                         modelling_group) {
  # Scenario-specific tests:
  # 1. (touchstone, scenario_description) exists in scenario table
  # 2. (scenario_description, disease) exists in scenario_description table
  # 3. (touchstone, modelling_group) exists in responsibility_set table
  # 4. (responsibility_set, scenario) exist in responsibility, where
  #    scenario id comes from #1, and responsibility_set id from #3
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
  # Possibly, we could check that all scenarios are included, but
  # the exceptions are the groups that have to do a VIS report, as they
  # have several additional scenarios for which they provide central but
  # not stochastic estimates. So not sure if this is so easy to insist on.
  invisible(TRUE)
}
