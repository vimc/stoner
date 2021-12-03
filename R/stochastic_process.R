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

  cache_life_table <- NULL

  assert_connection(con)
  if (upload_to_annex) {
    assert_connection(annex)
  }

  countries <- DBI::dbGetQuery(con, "SELECT id, nid FROM country")

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

  if (is.data.frame(dalys)) {
    stopifnot(all.equal(sort(names(dalys)),
      c("average_duration", "disability_weight", "outcome", "proportion")))
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
    stone_stochastic_cert_verify(con, cert, modelling_group,
                                 touchstone, disease)
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

  check_outcomes <- function(type, options) {
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

  check_outcomes("cases", cases)
  check_outcomes("deaths", deaths)

  if (is.data.frame(dalys)) {
    check_outcomes("dalys", unique(dalys$outcome))

  } else {
    check_outcomes("dalys", dalys)
  }

  # dry_run below is so that we can test all the files exists
  # before we bother reading any of them.

  all_scenarios <- function(dry_run = FALSE) {

    all_u5_cal <- NULL
    all_u5_coh <- NULL
    all_cal <- NULL
    all_coh <- NULL

    for (scenario_no in seq_along(scenarios)) {

      scenario_data <- list()
      scenario <- scenarios[scenario_no]

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

      ################################################################
      # Read a single csv.xz file, summing the outcomes into the three
      # we want, ignoring the columns we don't want, and

      read_xz_csv <- function(the_file, meta_cols, allow_missing_disease,
                              runid_from_file, run_id) {

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

      ################################################################

      for (i in index_from:index_to) {

        one_file <- files[scenario_no]
        one_file <- gsub(":index", i, one_file)
        one_file <- gsub(":group", modelling_group, one_file)
        one_file <- gsub(":touchstone", touchstone, one_file)
        one_file <- gsub(":disease", disease, one_file)
        one_file <- gsub(":scenario", scenario, one_file)
        the_file <- file.path(in_path, one_file)

        if (!file.exists(the_file)) {
          stop(sprintf("File not found: %s", the_file))
        }
        if (!dry_run) {
          message(the_file)
          if (is.data.frame(dalys)) {
            dalys_cols <- unique(dalys$outcome)
          } else {
            dalys_cols <- dalys
          }

          scenario_data[[i]] <-
            read_xz_csv(the_file,
                        unique(c(deaths, cases, dalys_cols)),
                        allow_missing_disease,
                        runid_from_file, i)
        }
      }

      if (!dry_run) {

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

        scenario_data <- NULL
        gc()


        ##############################################################
        # If this is the first scenario, then it's easy...

        rename_cols <- function(scen, scname) {
          names(scen)[names(scen) == 'deaths'] <- paste0("deaths_", scname)
          names(scen)[names(scen) == 'cases'] <- paste0("cases_", scname)
          names(scen)[names(scen) == 'dalys'] <- paste0("dalys_", scname)
          scen
        }


        if (is.null(all_u5_cal)) {

          all_u5_cal <- rename_cols(scen_u5_cal, scenario)
          all_u5_coh <- rename_cols(scen_u5_coh, scenario)
          all_cal <- rename_cols(scen_cal, scenario)
          all_coh <- rename_cols(scen_coh, scenario)

        # Otherwise, we need to add new columns with the new scenario
        # HOWEVER: there could be different countries in different
        # scenarios, so we may need to add countries with NA data to
        # make the rows line up.

        } else {

          # Are there any countries in the accumulated data that aren't
          # in this new scenario? If, so need to add some NA rows to scenario.

          add_na_countries <- function(df, missing) {
            na_data <- df[df$country == df$country[1], ]
            na_cols <- names(df)
            na_cols <- na_cols[!na_cols %in% c("run_id", "country", "year")]
            na_data[, na_cols] <- NA
            new_data <- list()
            for (m in seq_along(missing)) {
              na_data$country <- missing[m]
              new_data[[m]] <- na_data
            }

            df <- rbind(df, rbindlist(new_data))
            df <- df[order(df$run_id, df$country, df$year), ]
          }

          known_countries <- unique(all_u5_cal$country)
          next_countries <- unique(scen_u5_cal$country)

          missing_countries <- known_countries[
            !known_countries %in% next_countries]

          if (length(missing_countries) > 0) {
            scen_u5_cal <- add_na_countries(scen_u5_cal, missing_countries)
            scen_u5_coh <- add_na_countries(scen_u5_coh, missing_countries)
            scen_cal <- add_na_countries(scen_cal, missing_countries)
            scen_coh <- add_na_countries(scen_coh, missing_countries)
          }

          # And are there any countries in the new scenario that we
          # didn't encounter in previous scenarios? If so, we need to
          # add NA rows to the accumulated data.

          missing_countries <- next_countries[
            !next_countries %in% known_countries]

          if (length(missing_countries) > 0) {
            all_u5_cal <- add_na_countries(all_u5_cal, missing_countries)
            all_u5_coh <- add_na_countries(all_u5_coh, missing_countries)
            all_cal <- add_na_countries(all_cal, missing_countries)
            all_coh <- add_na_countries(all_coh, missing_countries)
          }

          # Now all... and scen... should have same number of rows,
          # and be sorted by run_id, country, year (or cohort)

          col_bind <- function(all, scen, scname) {
            scen <- rename_cols(scen, scname)
            scen$year <- NULL
            scen$country <- NULL
            scen$run_id <- NULL
            cbind(all, scen)
          }

          all_u5_cal <- col_bind(all_u5_cal, scen_u5_cal, scenario)
          all_u5_coh <- col_bind(all_u5_coh, scen_u5_coh, scenario)
          all_cal <- col_bind(all_cal, scen_cal, scenario)
          all_coh <- col_bind(all_coh, scen_coh, scenario)

        } # End of "not the first scenario"
      } # End of "dry_run must be FALSE"
    } # End of scenario loop

    # If it's a dry_run. then nothing left to do now; all params
    # for all scenarios / files have been tested.

    if (dry_run) {
      return()
    }

    names(all_u5_coh)[names(all_u5_coh) == "year"] <- "cohort"
    names(all_coh)[names(all_coh) == "year"] <- "cohort"

    # We're done. Save the files.

    all_u5_cal_file <- file.path(out_path, sprintf("%s_%s_calendar_u5.csv",
                                                   modelling_group, disease))

    write.csv(x = all_u5_cal, file = all_u5_cal_file, row.names = FALSE)


    all_cal_file <- file.path(out_path, sprintf("%s_%s_calendar.csv",
                                                modelling_group, disease))

    write.csv(x = all_cal, file = all_cal_file, row.names = FALSE)

    all_u5_coh_file <- file.path(out_path, sprintf("%s_%s_cohort_u5.csv",
                                                   modelling_group, disease))

    write.csv(x = all_u5_coh, file = all_u5_coh_file, row.names = FALSE)

    all_coh_file <- file.path(out_path, sprintf("%s_%s_cohort.csv",
                                                modelling_group, disease))

    write.csv(x = all_coh, file = all_coh_file, row.names = FALSE)

    # Upload to Annex. Only allow possibility of creating new stochastic_file
    # table on the first one; it will either fail there, or exist for the later
    # three uploads.

    if (upload_to_annex) {
      stoner::stone_stochastic_upload(
        all_u5_cal_file, con, annex, modelling_group, disease,
        touchstone, is_cohort = FALSE, is_under5 = TRUE,
        allow_new_database = allow_new_database, testing = testing)

      stoner::stone_stochastic_upload(
        all_u5_coh_file, con, annex, modelling_group, disease,
        touchstone, is_cohort = TRUE, is_under5 = TRUE, testing = testing)

      stoner::stone_stochastic_upload(
        all_cal_file, con, annex, modelling_group, disease,
        touchstone, is_cohort = FALSE, is_under5 = FALSE, testing = testing)

      stoner::stone_stochastic_upload(
        all_coh_file, con, annex, modelling_group, disease,
        touchstone, is_cohort = TRUE, is_under5 = FALSE, testing = testing)
    }
  }

  all_scenarios(dry_run = TRUE)

  # We are now as tested as we can get at this point.
  ####################################################

  all_scenarios(dry_run = FALSE)

  invisible()
}
