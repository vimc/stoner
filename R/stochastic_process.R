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
##' @param deaths If deaths must be calculated as a sum of other burden
##' outcomes, then provide a vector of the outcome names here. The default
##' is the existing deaths burden_outcome.
##' @param cases If cases must be calculated as a sum of other burden
##' outcomes, then provide a vector of the outcome names here. The default
##' is the existing cases burden_outcome.
##' @param dalys If DALYs must be calculated as a sum of other burden
##' outcomes, then provide a vector of the outcome names here. The default
##' is the existing DALYs burden_outcome. Alternatively, for the one
##' remaining group that does not provide DALYs, you can supply a data
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
##' relative fast; creating the csvs is slower and worth caching)
##' @param annex DBI connection to annex, used if upload_to_annex is TRUE.
##' @param allow_new_database If uploading, then set this to TRUE to enable
##' creating the stochastic_file table if it is not found.
##' @param bypass_cert_check If TRUE, then no checks are carried out on the
##' parameter certificate (if provided).
##' @param testing For internal use only.
stone_stochastic_process <- function(con, modelling_group, disease,
                                     touchstone, scenarios, in_path, files,
                                     cert, index_start, index_end, out_path,
                                     deaths = "deaths", cases = "cases",
                                     dalys = "dalys",
                                     runid_from_file = FALSE,
                                     allow_missing_disease = FALSE,
                                     upload_to_annex = FALSE,
                                     annex = NULL,
                                     allow_new_database = FALSE,
                                     bypass_cert_check = FALSE,
                                     testing = FALSE) {

  #######################################################################
  # Do all the parameter and file testing upfront, as this can be a very
  # time-consuming process...
  touchpoint <- list(
    modelling_group = modelling_group,
    disease = disease,
    touchstone = touchstone,
  )
  outcomes <- list(
    deaths = deaths,
    cases = cases,
    dalys = dalys
  )
  inputs <- stochastic_process_validate(con,
                                        touchpoint = touchpoint,
                                        scenarios = scenario,
                                        in_path = in_path,
                                        files = files,
                                        index_start = index_start,
                                        index_end = index_end,
                                        outcomes = outcomes,
                                        runid_from_file = runid_from_file,
                                        upload_to_annex = upload_to_annex,
                                        annex = annex,
                                        cert = cert,
                                        bypass_cert_check = bypass_cert_check)

  read_params <- list(
    in_path = in_path,
    files = inputs$files,
    index_start = inputs$index_start,
    index_end = inputs$index_end,
    run_id_from_file = run_id_from_file,
    allow_missing_disease = allow_missing_disease
  )
  all_aggregated <- all_scenarios(con,
                                  touchpoint = touchpoint,
                                  scenarios = scenarios,
                                  read_params = read_params,
                                  out_path = out_path,
                                  outcomes = outcomes,
                                  dry_run = TRUE)

  # We are now as tested as we can get at this point.
  ####################################################

  all_aggregated <- all_scenarios(con,
                                  touchpoint = touchpoint,
                                  scenarios = scenarios,
                                  read_params = read_params,
                                  outcomes = outcomes,
                                  dry_run = FALSE)

  if (dry_run) {
    # If it's a dry_run. then nothing left to do now; all params
    # for all scenarios / files have been tested.
    return()
  }

  write_output_to_disk(all_aggregated, out_path, modelling_group, disease)

  # Upload to Annex. Only allow possibility of creating new stochastic_file
  # table on the first one; it will either fail there, or exist for the later
  # three uploads.
  if (upload_to_annex) {
    write_output_to_annex(paths, con, annex, modelling_group,
                          disease, touchstone, allow_new_database,
                          testing)
  }

  invisible()
}


all_scenarios <- function(con,
                          touchpoint,
                          scenarios,
                          read_params,
                          outcomes,
                          dry_run) {
  all_aggregated <- NULL

  all_countries <- DBI::dbGetQuery(con, "SELECT id, nid FROM country")
  for (scenario_no in seq_along(scenarios)) {
    scenario_name <- scenarios[scenario_no]
    aggregated_scenario <- process_scenario(scenario_name, touchpoint,
                                            read_params, outcomes,
                                            all_countries, dry_run)


    ##############################################################
    # If this is the first scenario, then it's easy...
    if (is.null(all_aggregated)) {
      all_aggregated <- aggregated_scenario

      # Otherwise, we need to add new columns with the new scenario
      # HOWEVER: there could be different countries in different
      # scenarios, so we may need to add countries with NA data to
      # make the rows line up.

    } else {

      # And are there any countries in the new scenario that we
      # didn't encounter in previous scenarios (or vice versa)? If so, we need to
      # add NA rows to the accumulated data.
      known_countries <- unique(all_aggregated$u5_calendar_year$country)
      next_countries <- unique(aggregated_scenario$u5_calendar_year$country)

      missing_countries_left <- next_countries[
        !next_countries %in% known_countries]
      if (length(missing_countries_left) > 0) {
        all_aggregated <- add_na_countries(all_aggregated, missing_countries_left)
      }

      missing_countries_right <- known_countries[
        !known_countries %in% next_countries]
      if (length(missing_countries_right) > 0) {
        aggregated_scenario <- add_na_countries(aggregated_scenario,
                                                missing_countries_right)
      }

      all_aggregated <- bind_scenarios(all_aggregated, aggregated_scenario)
    }
  }

  names(all_aggregated$u5_cohort)[names(all_aggregated$u5_cohort) == "year"] <- "cohort"
  names(all_aggregated$all_cohort)[names(all_aggregated$all_cohort) == "year"] <- "cohort"

  all_aggregated
}

rename_cols <- function(aggregated_data, scenario_name) {
  for (df in aggregated_data) {
    names(df)[names(df) == 'deaths'] <- paste0("deaths_", scenario_name)
    names(df)[names(df) == 'cases'] <- paste0("cases_", scenario_name)
    names(df)[names(df) == 'dalys'] <- paste0("dalys_", scenario_name)
  }
  aggregated_data
}

bind_scenarios <- function(all_aggregated, scen_aggregated, scenario_name) {
  # Now all... and scen... should have same number of rows,
  # and be sorted by run_id, country, year (or cohort)
  bind_one <- function(aggregated_name) {
    all <- all_aggregated[[aggregated_name]]
    scen <- scen_aggregated[[aggregated_name]]
    scen$year <- NULL
    scen$country <- NULL
    scen$run_id <- NULL
    cbind(all, scen)
  }
  lapply(names(all_aggregated), bind_one)
}

process_scenario <- function(scenario, touchpoint, read_params, outcomes,
                             countries, dry_run) {
  scenario_data <- list()

  if (length(read_params$index_start) != 1) {
    index_from <- read_params$index_start[scenario_no]
    index_to <- read_params$index_end[scenario_no]
  } else {
    index_from <- read_params$index_start
    index_to <- read_params$index_end
  }

  if (is.na(index_from)) {
    index_from <- 1
    index_to <- 1
  }

  ################################################################

  for (i in index_from:index_to) {

    the_file <- read_params$files[scenario_no]
    the_file <- gsub(":index", i, the_file)
    the_file <- gsub(":group", touchpoint$modelling_group, the_file)
    the_file <- gsub(":touchstone", touchpoint$touchstone, the_file)
    the_file <- gsub(":disease", touchpoint$disease, the_file)
    the_file <- gsub(":scenario", scenario, the_file)

    if (!file.exists(the_file)) {
      stop(sprintf("File not found: %s", the_file))
    }
    if (!dry_run) {
      message(the_file)
      if (is.data.frame(outcomes$dalys)) {
        dalys_cols <- unique(outcomes$dalys$outcome)
      } else {
        dalys_cols <- outcomes$dalys
      }

      scenario_data[[i]] <-
        read_xz_csv(the_file,
                    unique(c(outcomes$deaths, outcomes$cases, dalys_cols)),
                    read_params$allow_missing_disease,
                    read_params$runid_from_file, i, countries)
    }
  }

  if (dry_run) {
    return(invisible(TRUE))
  }

  # We now have a full scenario. Eliminate age, splitting into
  # the four files (calendar/cohort, and u5/all age).
  # For now, in the cohort files, I'm going to call cohort
  # 'year' - just to keep code tidier, as the code is common...

  if (index_from == index_to) {
    scenario_data <- scenario_data[[1]]
  } else {
    scenario_data <- rbindlist(scenario_data)
  }

  #######################################################

  agg_and_sort <- function(data) {
    # Next lines are just to avoid travis NOTEs on
    # the by = list line.
    run_id <- NULL
    year <- NULL
    country <- NULL
    data <- data[ , lapply(.SD, sum),
                  by = list(run_id, year, country),
                  .SDcols = c("cases", "dalys", "deaths")]
    data[order(data$run_id, data$country, data$year), ]

  }

  scen_u5 <- scenario_data[scenario_data$age <= 4 , ]
  scen_u5_cal <- agg_and_sort(scen_u5)

  scen_u5$year <- scen_u5$year - scen_u5$age
  scen_u5_coh <- agg_and_sort(scen_u5)

  scen_cal <- agg_and_sort(scenario_data)

  scenario_data$year <- scenario_data$year - scenario_data$age
  scen_coh <- agg_and_sort(scenario_data)

  aggregated_data <- list(
    u5_calendar_year = scen_u5_cal,
    u5_cohort = scen_u5_coh,
    all_calendar_year = scen_cal,
    all_cohort = scen_coh
  )

  scenario_data <- NULL
  gc()

  rename_cols(aggregated_data, scenario)
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

read_xz_csv <- function(the_file, meta_cols, allow_missing_disease,
                        runid_from_file, run_id, countries) {
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
                    progress = FALSE, na = "NA")
  ))

  for (n in names(csv)) {
    csv[[n]][is.na(csv[[n]])] <- 0
  }

  if (runid_from_file) {
    csv[['run_id']] <- run_id
  }

  csv$country <- countries$nid[match(csv$country, countries$id)]

  if (is.data.frame(dalys)) {
    res <- stoner_calculate_dalys(con, touchstone, csv, dalys, cache_life_table)
    csv <- res$data
    if (is.null(cache_life_table)) {
      cache_life_table <- res$life_table
    }

  } else {
    csv <- calc_outcomes(csv, dalys, "dalys")
  }

  csv <- calc_outcomes(csv, deaths, "deaths")
  csv <- calc_outcomes(csv, cases, "cases")

  csv[, c("run_id", "year", "age", "country", "deaths", "cases" ,"dalys")]
}


add_na_countries <- function(aggregated_stochastic, missing) {
  lapply(aggregated_stochastic, function(aggregation) {
    na_data <- aggregation[aggregation$country == aggregation$country[1], ]
    na_cols <- names(aggregation)
    na_cols <- na_cols[!na_cols %in% c("run_id", "country", "year")]
    na_data[, na_cols] <- NA
    new_data <- list()
    for (m in seq_along(missing)) {
      na_data$country <- missing[m]
      new_data[[m]] <- na_data
    }

    aggregation <- rbind(aggregation, rbindlist(new_data))
    aggregation[order(
      aggregation$run_id, aggregation$country, aggregation$year), ]
  })
}


write_output_to_disk <- function(output, out_path, modelling_group, disease) {
  all_u5_cal_file <- file.path(out_path, sprintf("%s_%s_calendar_u5.csv",
                                                 modelling_group, disease))
  write.csv(x = output$u5_calendar_year, file = all_u5_cal_file,
            row.names = FALSE)


  all_cal_file <- file.path(out_path, sprintf("%s_%s_calendar.csv",
                                              modelling_group, disease))
  write.csv(x = output$all_calendar_year, file = all_cal_file,
            row.names = FALSE)

  all_u5_coh_file <- file.path(out_path, sprintf("%s_%s_cohort_u5.csv",
                                                 modelling_group, disease))
  write.csv(x = output$u5_calendar_year, file = all_u5_coh_file,
            row.names = FALSE)

  all_coh_file <- file.path(out_path, sprintf("%s_%s_cohort.csv",
                                              modelling_group, disease))
  write.csv(x = output$u5_cohort, file = all_coh_file, row.names = FALSE)
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
