##' Convert a modelling group's stochastic files into the summary format,
##' ready for later uploading to the Montagu data annex. Four files are
##' produced which reduce age to all-age-total, and under-5-total, by
##' calendar year, or birth-cohort year.
##'
##' @export
##' @title Process stochastic data
##' @import data.table
##' @import readr
##' @importFrom utils write.csv
##' @param con DBI connection to production. Used for verifying certificate
##' against expected properties
##' @param modelling_group The modelling group id
##' @param disease The disease
##' @param touchstone The touchstone (including version) for these estimates
##' @param scenarios A vector of scenario_descriptions. If the files
##' parameter is of length more than 1, then it must be the same length as
##' the number of scenarios, and a one-to-one mapping between the two is
##' assumed.
##' @param in_path The folder containing the stochastic files
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
##' @param out_path Path to writing output files into
##' @param pre_aggregation_path Path to dir to write out pre age-disaggregated
##' data into. If NULL then this is skipped.
##' @param outcomes A list of names vectors, where the name is the burden
##' outcome, and the elements of the list are the column names in the
##' stochastic files that should be summed to compute that outcome. The
##' default is to expect outcomes `deaths`, `cases`, `dalys`, and `yll`,
##' with single columns with the same names in the stochastic files.
##' @param dalys_recipe If DALYs must be calculated, you can supply a data
##' frame here, and stoner will calculate DALYs using that recipe. The
##' data frame must have names `outcome`, `proportion`, `average_duration`
##' and `disability_weight`. See [stoner_calculate_dalys].
##' @param runid_from_file Occasionally groups have omitted the run_id
##' from the stochastic file, and provided 200 files, one per run_id. Set
##' runid_from_file to TRUE if this is the case, to deduce the run_id from
##' the filenames. The index_start and index_end must be 1 and 200 in
##' this case.
##' @param allow_missing_disease Occasionally groups have omitted the
##' disease column from their stochastic data. Set this to TRUE to expect
##' that circumstance, and avoid generating warnings.
##' @param upload_to_annex Set to TRUE if you want to upload the results
##' straight into annex. (Files will still be created, as the upload is
##' relatively fast; creating the csvs is slower and worth caching)
##' @param annex DBI connection to annex, used if upload_to_annex is TRUE.
##' @param allow_new_database If uploading, then set this to TRUE to enable
##' creating the stochastic_file table if it is not found.
##' @param bypass_cert_check If TRUE, then no checks are carried out on the
##' parameter certificate (if provided).
##' @param testing For internal use only.
##' @param lines Number of lines to read from each file, Inf by default to
##' read all lines. Set a lower number for testing subset of process before
##' doing the full run.
##' @param log_file Path to file to save logs to, NULL to not log to file.
##' If file exists it will be appended to, otherwise file will be created.
##' @param silent TRUE to silence console logs.
##' @param lines Number of lines to read from each file, Inf by default to
##' read all lines. Set a lower number for testing subset of process before
##' doing the full run.
stone_stochastic_process <- function(con, modelling_group, disease,
                                     touchstone, scenarios, in_path, files,
                                     cert, index_start, index_end, out_path,
                                     pre_aggregation_path = NULL,
                                     outcomes = list(deaths = "deaths",
                                                     cases = "cases",
                                                     dalys = "dalys",
                                                     yll = "yll"),
                                     dalys_recipe = NULL,
                                     runid_from_file = FALSE,
                                     allow_missing_disease = FALSE,
                                     upload_to_annex = FALSE,
                                     annex = NULL,
                                     allow_new_database = FALSE,
                                     bypass_cert_check = FALSE,
                                     testing = FALSE,
                                     lines = Inf,
                                     log_file = NULL,
                                     silent = FALSE) {

  start <- Sys.time()

  ## Initialise logger
  if (!is.null(log_file)) {
    assert_scalar_character(log_file)
    if (dir.exists(log_file)) {
      stop(sprintf("Log file '%s' is a directory, must be a path to a file",
                   log_file))
    }
    if (!file.exists(log_file)) {
      file.create(log_file, showWarnings = FALSE, overwrite = FALSE)
    }
  }
  assert_scalar_logical(silent)

  lg <- get_logger()
  if (silent) {
    threshold <- lg$inherited_appenders$console$threshold
    lg$inherited_appenders$console$set_threshold("error")
    on.exit(lg$inherited_appenders$console$set_threshold(threshold), add = TRUE)
  }
  if (!is.null(log_file)) {
    lg$add_appender(lgr::AppenderFile$new(log_file), name = "file")
    on.exit(lg$remove_appender("file"), add = TRUE)
  }

  ## Setup life table cache
  cache$life_table <- NULL

  #######################################################################
  # Do all the parameter and file testing upfront, as this can be a very
  # time-consuming process...
  touchpoint <- list(
    modelling_group = modelling_group,
    disease = disease,
    touchstone = touchstone
  )

  withCallingHandlers(
    files <- stochastic_process_validate(con,
      touchpoint = touchpoint,
      scenarios = scenarios,
      in_path = in_path,
      files = files,
      index_start = index_start,
      index_end = index_end,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      outcomes = outcomes,
      dalys_recipe = dalys_recipe,
      runid_from_file = runid_from_file,
      upload_to_annex = upload_to_annex,
      annex = annex,
      cert = cert,
      bypass_cert_check = bypass_cert_check,
      lines = lines),

    error = function(e) {
      lg$fatal(paste0("Processing for modelling_group: %s, disease: %s ",
                      "failed with error \n    %s"),
               modelling_group, disease, e$message)
    }
  )

  read_params <- list(
    in_path = in_path,
    files = files,
    runid_from_file = runid_from_file,
    allow_missing_disease = allow_missing_disease,
    lines = lines
  )

  lg$info(paste0("Validated inputs, processing scenario data for ",
                 "modelling_group: %s, disease: %s"), modelling_group, disease)

  scenario_data <- timed(
    all_scenarios(con,
                  touchpoint = touchpoint,
                  scenarios = scenarios,
                  read_params = read_params,
                  outcomes = outcomes,
                  dalys_recipe = dalys_recipe),
    "Processed %s scenarios for modelling group: %s, disease: %s",
    length(scenarios), touchpoint$modelling_group, touchpoint$disease)

  if (!is.null(pre_aggregation_path)) {
    timed(write_pre_aggregated_to_disk(scenario_data, touchpoint,
                                       pre_aggregation_path),
          "Wrote all pre-aggregated output to dir %s", pre_aggregation_path)
  }

  all_aggregated <- timed(aggregate_data(scenario_data),
                          "Finished aggregating all scenario data")

  paths <- timed(write_output_to_disk(all_aggregated, out_path,
                                      modelling_group, disease),
                 "Wrote all output to dir %s", out_path)

  # Upload to Annex. Only allow possibility of creating new stochastic_file
  # table on the first one; it will either fail there, or exist for the later
  # three uploads.
  if (upload_to_annex) {
    timed(write_output_to_annex(paths, con, annex, modelling_group,
                          disease, touchstone, allow_new_database,
                          testing),
          "Wrote outputs to annex")
  }

  elapsed <- Sys.time() - start
  lg$info("Processing for modelling_group: %s, disease: %s completed in %s",
          modelling_group, disease, human_readable_time(elapsed))
  paths
}


all_scenarios <- function(con,
                          touchpoint,
                          scenarios,
                          read_params,
                          outcomes,
                          dalys_recipe) {

  all_scenarios <- NULL
  all_countries <- DBI::dbGetQuery(con, "SELECT id, nid FROM country")
  lg <- get_logger()
  lg$info("Processing %s scenarios for modelling group: %s, disease: %s",
          length(scenarios), touchpoint$modelling_group, touchpoint$disease)
  for (scenario_no in seq_along(scenarios)) {
    scenario_name <- scenarios[scenario_no]
    files <- read_params$files[[scenario_name]]
    lg$info("Processing modelling group: %s, disease: %s, scenario (%s/%s): %s",
            touchpoint$modelling_group,
            touchpoint$disease,
            scenario_no,
            length(scenarios),
            scenario_name)
    scenario_data <- process_scenario(con, scenario_name, files,
                                      touchpoint, read_params, outcomes,
                                      dalys_recipe, all_countries)

    ##############################################################
    # If this is the first scenario, then it's easy...
    if (is.null(all_scenarios)) {
      all_scenarios <- scenario_data

      # Otherwise, we need to add new columns with the new scenario
      # HOWEVER: there could be different countries in different
      # scenarios, so we may need to add countries with NA data to
      # make the rows line up.

    } else {
      all_scenarios <- dplyr::full_join(
        all_scenarios, scenario_data,
        by = c("country", "year", "run_id", "age"))
    }
  }
  all_scenarios
}

process_scenario <- function(con, scenario, files, touchpoint,
                             read_params, outcomes, dalys_recipe,
                             countries) {
  scenario_data <- list()
  lines <- read_params$lines

  ################################################################

  lg <- get_logger()
  for (i in seq_along(files)) {
    the_file <- files[i]
    lg$info("Reading %s", the_file)
    scenario_data[[i]] <-
      read_xz_csv(con, the_file, outcomes, dalys_recipe,
                  read_params$allow_missing_disease,
                  read_params$runid_from_file, i,
                  touchpoint$touchstone, countries,
                  lines = lines)
  }

  # We now have a full scenario. Eliminate age, splitting into
  # the four files (calendar/cohort, and u5/all age).
  # For now, in the cohort files, I'm going to call cohort
  # 'year' - just to keep code tidier, as the code is common...

  if (length(scenario_data) == 1) {
    scenario_data <- scenario_data[[1]]
  } else {
    scenario_data <- rbindlist(scenario_data)
  }

  for (i in seq_along(outcomes)) {
    outcome <- names(outcomes[i])
    names(scenario_data)[names(scenario_data) == outcome] <-
      paste0(outcome, "_", scenario)
  }

  if (!is.null(dalys_recipe)) {
    names(scenario_data)[names(scenario_data) == 'dalys'] <-
      paste0("dalys_", scenario)
  }

  scenario_data
}

aggregate_data <- function(scenario_data) {
  agg_and_sort <- function(data) {
    ## Define run_id, year and country as NULL to avoid
    ## R CMD note about no visible binding for global variable
    run_id <- year <- country <- age <- NULL
    data %>%
      dplyr::group_by(run_id, year, country) %>%
      dplyr::summarise_all(sum) %>%
      dplyr::select(-age) %>% ## Drop age as we have aggregated over it
      dplyr::arrange(run_id, country, year)
  }

  scen_u5 <- scenario_data[scenario_data$age <= 4 , ]
  scen_u5_cal <- agg_and_sort(scen_u5)

  scen_u5$year <- scen_u5$year - scen_u5$age
  scen_u5_coh <- agg_and_sort(scen_u5)

  scen_cal <- agg_and_sort(scenario_data)

  scenario_data$year <- scenario_data$year - scenario_data$age
  scen_coh <- agg_and_sort(scenario_data)

  scen_u5_coh <- scen_u5_coh %>%
    dplyr::rename(cohort = year)
  scen_coh <- scen_coh %>%
    dplyr::rename(cohort = year)

  list(
    u5_calendar_year = scen_u5_cal,
    u5_cohort = scen_u5_coh,
    all_calendar_year = scen_cal,
    all_cohort = scen_coh
  )
}


################################################################
# Read a single csv.xz file, summing the outcomes into the three
# we want, ignoring the columns we don't want, and

read_large_file <- function(...) {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({
    gc()
    unlink(tmp, recursive = TRUE)
  })
  withr::with_envvar(
    c(VROOM_TEMP_PATH = tmp),
    readr::read_csv(..., lazy = FALSE))
}

calc_outcomes <- function(csv, outcomes, single_outcome) {
  # If the outcome we want is the sum of other outcomes...
  if (!identical(outcomes, single_outcome)) {
    total <- csv[[outcomes[1]]]
    if (length(outcomes) > 1) {
      for (i in 2:length(outcomes)) {
        total <- total + csv[[outcomes[i]]]
      }
    }
    csv[[single_outcome]] <- total
  }
  csv
}

read_xz_csv <- function(con, the_file, outcomes, dalys_recipe, allow_missing_disease,
                        runid_from_file, run_id, touchstone, countries,
                        lines) {
  if (is.data.frame(dalys_recipe)) {
    dalys_cols <- unique(dalys_recipe$outcome)
  }
  meta_cols <- unique(unlist(outcomes))

  col_list <- list(
    year = readr::col_integer(),
    age = readr::col_integer(),
    country = readr::col_character(),
    country_name = readr::col_skip(),
    cohort_size = readr::col_skip()
  )

  if (!runid_from_file) {
    col_list[['run_id']] = readr::col_integer()
  }

  if (!allow_missing_disease) {
    col_list[['disease']] <- col_skip()
  }

  for (outcome in meta_cols) {
    if (!is.na(outcome)) {
      col_list[[outcome]] <- readr::col_double()
    }
  }

  columns <- do.call(readr::cols_only, col_list)

  csv <- suppressMessages(as.data.table(
    read_large_file(the_file,
                    col_types = columns,
                    progress = FALSE, na = "NA",
                    n_max = lines)
  ))

  for (n in names(csv)) {
    csv[[n]][is.na(csv[[n]])] <- 0
  }

  if (runid_from_file) {
    csv[['run_id']] <- run_id
  }

  csv$country <- countries$nid[match(csv$country, countries$id)]

  if (is.data.frame(dalys_recipe)) {
    res <- stoner_calculate_dalys(con, touchstone, csv,
                                  dalys_recipe, cache$life_table)
    csv <- res$data
    if (is.null(cache$life_table)) {
      cache$life_table <- res$life_table
    }
  }

  for (i in seq_along(outcomes)) {
    csv <- calc_outcomes(csv, outcomes[[i]], names(outcomes)[i])
  }

  csv <- as.data.frame(csv)
  cols <- unique(c("run_id", "year", "age", "country", names(outcomes), if (!is.null(dalys_recipe)) "dalys"))
  csv[, cols]
}


write_pre_aggregated_to_disk <- function(data, touchpoint,
                                         pre_aggregation_path) {
  countries <- unique(data$country)
  for (country in countries) {
    timed({
      path <- file.path(pre_aggregation_path,
                        sprintf("%s_%s_%s_pre_aggregation.qs",
                                touchpoint$modelling_group,
                                touchpoint$disease,
                                country))
      data <- as.data.frame(data)
      qs::qsave(data[data$country == country, ], path)
    }, "Saved %s size %s", path, prettyunits::pretty_bytes(file.size(path)))
  }
  invisible(TRUE)
}


write_output_to_disk <- function(output, out_path, modelling_group, disease) {
  all_u5_cal_file <- file.path(out_path, sprintf("%s_%s_calendar_u5.qs",
                                                 modelling_group, disease))
  timed(qs::qsave(x = as.data.frame(output$u5_calendar_year),
            file = all_u5_cal_file),
        "Saved %s size %s", all_u5_cal_file,
        prettyunits::pretty_bytes(file.size(all_u5_cal_file)))


  all_cal_file <- file.path(out_path, sprintf("%s_%s_calendar.qs",
                                              modelling_group, disease))
  timed(qs::qsave(x = as.data.frame(output$all_calendar_year),
            file = all_cal_file),
        "Saved %s size %s", all_u5_cal_file,
        prettyunits::pretty_bytes(file.size(all_u5_cal_file)))

  all_u5_coh_file <- file.path(out_path, sprintf("%s_%s_cohort_u5.qs",
                                                 modelling_group, disease))
  timed(qs::qsave(x = as.data.frame(output$u5_cohort),
            file = all_u5_coh_file),
        "Saved %s size %s", all_u5_cal_file,
        prettyunits::pretty_bytes(file.size(all_u5_cal_file)))

  all_coh_file <- file.path(out_path, sprintf("%s_%s_cohort.qs",
                                              modelling_group, disease))
  timed(qs::qsave(x = as.data.frame(output$all_cohort),
            file = all_coh_file),
        "Saved %s size %s", all_u5_cal_file,
        prettyunits::pretty_bytes(file.size(all_u5_cal_file)))
  list(
    all_u5_cal_file = all_u5_cal_file,
    all_u5_coh_file = all_u5_coh_file,
    all_cal_file = all_cal_file,
    all_coh_file = all_coh_file
  )
}

write_output_to_annex <- function(paths, con, annex, modelling_group,
                                  disease, touchstone, allow_new_database,
                                  testing) {
  stone_stochastic_upload(
    paths$all_u5_cal_file, con, annex, modelling_group, disease,
    touchstone, is_cohort = FALSE, is_under5 = TRUE,
    allow_new_database = allow_new_database, testing = testing)

  stone_stochastic_upload(
    paths$all_u5_coh_file, con, annex, modelling_group, disease,
    touchstone, is_cohort = TRUE, is_under5 = TRUE, testing = testing)

  stone_stochastic_upload(
    paths$all_cal_file, con, annex, modelling_group, disease,
    touchstone, is_cohort = FALSE, is_under5 = FALSE, testing = testing)

  stone_stochastic_upload(
    paths$all_coh_file, con, annex, modelling_group, disease,
    touchstone, is_cohort = TRUE, is_under5 = FALSE, testing = testing)
}

stochastic_process_validate <- function(con, touchpoint, scenarios, in_path,
                                        files, index_start, index_end,
                                        out_path,
                                        pre_aggregation_path,
                                        outcomes,
                                        dalys_recipe,
                                        runid_from_file,
                                        upload_to_annex,
                                        annex,
                                        cert, bypass_cert_check,
                                        lines,
                                        logger) {
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

  if (!is.null(dalys_recipe)) {
    stopifnot(all.equal(sort(names(dalys_recipe)),
                        c("average_duration", "disability_weight", "outcome", "proportion")))

    # Can't specify both a DALYs sum and a recipe.

    stopifnot(!"dalys" %in% names(outcomes))
  }

  assert_scalar_character(in_path)
  if (!file.exists(in_path)) {
    stop(sprintf("Input path not found: %s", in_path))
  }

  assert_scalar_character(out_path)
  if (!file.exists(out_path)) {
    stop(sprintf("Output path not found: %s", out_path))
  }

  if (!is.null(pre_aggregation_path)) {
    assert_scalar_character(pre_aggregation_path)
    if (!file.exists(pre_aggregation_path)) {
      stop(sprintf("Pre aggregation output path not found: %s",
                   pre_aggregation_path))
    }
  }

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

  assert_scalar_numeric(lines)

  for (scenario in scenarios) {
    stochastic_validate_scenario(con, touchpoint, scenario)
  }

  for (i in seq_along(outcomes)) {
    check_outcome(con, names(outcomes)[i], outcomes[[i]])
  }

  validate_paths(file.path(in_path, files), scenarios,
                 touchpoint, index_start, index_end)
}


validate_paths <- function(files, scenarios, touchpoint,
                                          index_start, index_end) {

  scenario_substitute_path <- function(scenario_no) {
    if (length(index_start) != 1) {
      index_from <- index_start[scenario_no]
      index_to <- index_end[scenario_no]
    } else {
      index_from <- index_start
      index_to <- index_end
    }

    if (is.na(index_from)) {
      index_from <- 1
      index_to <- 1
    }

    scenario <- scenarios[scenario_no]
    files <- lapply(index_from:index_to, function(i) {
      the_file <- files[scenario_no]
      the_file <- gsub(":index", i, the_file)
      the_file <- gsub(":group", touchpoint$modelling_group, the_file)
      the_file <- gsub(":touchstone", touchpoint$touchstone, the_file)
      the_file <- gsub(":disease", touchpoint$disease, the_file)
      the_file <- gsub(":scenario", scenario, the_file)

      if (!file.exists(the_file)) {
        stop(sprintf("File not found: %s", the_file))
      }
      the_file
    })
    unlist(files)
  }
  paths <- lapply(seq_along(scenarios), scenario_substitute_path)
  names(paths) <- scenarios
  paths
}


check_outcome <- function(con, type, options) {
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

stochastic_validate_scenario <- function(con, touchpoint, scenario) {
  # Scenario-specific tests:
  # 1. (touchstone, scenario_description) exists in scenario table
  # 2. (scenario_description, disease) exists in scenario_description table
  # 3. (touchstone, modelling_group) exists in responsibility_set table
  # 4. (responsibility_set, scenario) exist in responsibility, where
  #    scenario id comes from #1, and responsibility_set id from #3
  scenario_id <- DBI::dbGetQuery(con, "
      SELECT id FROM scenario WHERE touchstone = $1
         AND scenario_description = $2",
                                 list(touchpoint$touchstone, scenario))$id
  if (length(scenario_id) != 1) {
    stop(sprintf("scenario %s not found in touchstone %s",
                 scenario, touchpoint$touchstone))
  }

  scenario_descs <- DBI::dbGetQuery(con, "
      SELECT count(*) FROM scenario_description WHERE id = $1
         AND disease = $2", list(scenario, touchpoint$disease))$count
  if (scenario_descs != 1) {
    stop(sprintf("scenario_description %s not valid for disease %s",
                 scenario, touchpoint$disease))
  }

  respset_id <- DBI::dbGetQuery(con, "
      SELECT id FROM responsibility_set WHERE touchstone = $1
         AND modelling_group = $2", list(touchpoint$touchstone,
                                         touchpoint$modelling_group))$id
  if (length(respset_id) != 1) {
    stop(sprintf("No responsibility_set for group %s in touchstone %s",
                 touchpoint$modelling_group, touchpoint$touchstone))
  }

  resp_id <- DBI::dbGetQuery(con, "
      SELECT id FROM responsibility WHERE responsibility_set = $1
         AND scenario = $2", list(respset_id, scenario_id))$id
  if (length(resp_id) != 1) {
    stop(sprintf("No responsibility for group %s, scenario %s, touchstone %s",
                 touchpoint$modelling_group, scenario, touchpoint$touchstone))
  }
  # Possibly, we could check that all scenarios are included, but
  # the exceptions are the groups that have to do a VIS report, as they
  # have several additional scenarios for which they provide central but
  # not stochastic estimates. So not sure if this is so easy to insist on.
  invisible(TRUE)
}
