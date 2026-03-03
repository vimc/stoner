##' Convert a modelling group's stochastic files into an intermediate
##' format including all the original data, separated by country
##' and scenario, and in PQ format for later processing. As much as
##' possible, we'll try and detect the input formats.
##'
##' @export
##' @title Standardise stochastic data files
##' @importFrom data.table rbindlist
##' @import arrow
##' @param group The modelling group.
##' @param in_path The folder or network path where the original files are found.
##' @param out_path The folder or network path to write output files to.
##' @param scenarios A vector of strings giving each scenario name.
##' @param files This can either be a vector of strings of the same length to
##' the vector of scenarios, in which case each entry should match the scenario,
##' providing the filename for the original uploads for that scenario. Most
##' groups provide files numbered between 1 and 200 for their stochastic runs;
##' replace that number with `:index` in the filename to match. Alternatively,
##' files can be a single entry containing the string `:scenario`; in this case,
##' files must exist that match each entry in the `scenarios` parameter, and
##' the same file string can be used to match all of them (perhaps additionaly
##' wiht `:index`).
##' @param index This is usually a vector of ints, `1:200` to match the
##' range of stochastic files uploaded per scenario. A few groups upload one
##' large file containing everything, in which case `:index` shouldn't occur
##' in the `files` parameter, and should be omitted.
##' @param rubella_fix Historically Rubella uploads used the burden outcome
##' `rubella_deaths_congenital` and `rubella_cases_congenital` instead of
##' the simpler `deaths` and `cases`, and additionally provided a
##' `rubella_infections` field. `rubella_fix` needs to be TRUE to standardise
##' these to the simpler names. Processing Rubella stochastic files without
##' this set to TRUE will fail - so while we should always do this, keeping
##' the parameter makes it more clear in the code what we're doing and why.
##' @param missing_run_id_fix Some groups in the past have omitted run_id
##' from the files, but included them in the filenames. This fix inserts
##' that into the files if the index parameter indicates we have 200 runs to
##' process.

stone_stochastic_standardise <- function(
    group, in_path, out_path, scenarios, files, index = 1,
    rubella_fix = TRUE, missing_run_id_fix = TRUE) {

  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
  if ((length(files) == 1) && (grepl(":scenario", files))) {
    files <- rep(files, length(scenarios))
    for (j in seq_along(scenarios)) {
      files[j] <- gsub(":scenario", scenarios[j], files[j])
    }
  }

  for (i in seq_along(scenarios)) {
    message(scenarios[i])
    all_data <- as.data.frame(data.table::rbindlist(lapply(index, function(j) {
      cat("\r", j)
      file <- gsub(":index", j, files[i])
      d <- read.csv(file.path(in_path, file))
      d$country_name <- NULL

      # Fixes needed to standardise Rubella
      if (rubella_fix) {
        names(d)[names(d) == "rubella_deaths_congenital"] <- "deaths"
        names(d)[names(d) == "rubella_cases_congenital"] <- "cases"
        d$rubella_infections <- NULL
      }

      # Detect where run_id is missing, but in filenames

      if (missing_run_id_fix) {
        if ((!"run_id" %in% names(d)) && (length(index) == 200)) d$run_id <- j
      }

      # Round to integer, as per guidance. (Not using as.integer, as that
      # has limits on how large numbers can be, so we are just truncating
      # digits here)

      d$dalys <- round(d$dalys)
      d$deaths <- round(d$deaths)
      d$cases <- round(d$cases)
      d$yll <- round(d$yll)
      d$cohort_size <- round(d$cohort_size)

      d
    }), use.names = TRUE))

    countries <- sort(unique(all_data$country))
    for (country in countries) {
      cat("\r", country)
      d <- all_data[all_data$country == country, ]
      d <- d[order(d$run_id, d$year, d$age), ]
      file <- sprintf("%s_%s_%s.pq", group, scenarios[i], country)
      arrow::write_parquet(d, file.path(out_path, file))
    }
    cat("\r Completed\n\n")
  }
}

##' Create a central stochastic file from a set of standardised
##' files created by stone_stochastic_standardise. This is for
##' a single scenario, and we'll expect to find a file for each
##' country containing all the runs. We'll produce one file with
##' the mean (or median) of the stochastics, and all countries
##' in one file.
##'
##' @export
##' @title Produce central average from stochastic files.
##' @importFrom data.table rbindlist
##' @import arrow
##' @import dplyr
##' @param base The folder in which the standardised stochastic files are found.
##' @param touchstone The touchstone name (for the graph title)
##' @param disease The disease, used for building the filename and graph title.
##' @param group The modelling group, used in the filename and graph title.
##' @param scenario The scenario to plot.
##' @param avg_method The function to use for averaging the outcomes, probably
##' mean (the default) or median.

stone_stochastic_central <- function(base, touchstone, disease, group,
                                     scenario, avg_method = mean) {

  # Get vector of standard files to work with

  path <- file.path(base, touchstone, sprintf("%s_%s", disease, group))
  if (!dir.exists(path)) {
    cli::cli_abort("Path not found: {path}")
  }
  files <- list.files(path, pattern = "*.pq")

  if (length(files) == 0) {
    cli::cli_abort("No .pq files found in {path}")
  }

  files <- files[grepl(sprintf("%s_%s_[A-Z][A-Z][A-Z].pq", group, scenario), files)]
  if (length(files) == 0) {
    cli::cli_abort("No files matching scenario: {scenario}")
  }

  pb <- cli::cli_progress_bar("Loading countries", total = length(files))
  central <- list()
  for (fn in seq_along(files)) {
    f <- files[fn]
    cli::cli_progress_update(pb, fn)

    d <- arrow::read_parquet(file.path(path, f))
    orig_country <- unique(d$country)

    d <- d %>% group_by(.data$year, .data$age) %>%
               summarise(
                   deaths = avg_method(.data$deaths),
                   cases = avg_method(.data$cases),
                   dalys = avg_method(.data$dalys),
                   yll = avg_method(.data$yll),
                   cohort_size = avg_method(.data$cohort_size),
                   .groups = "drop"
                   )
    d$disease <- disease
    d$country <- orig_country
    central[[f]] <- d
  }
  cli::cli_progress_done()
  central <- as.data.frame(data.table::rbindlist(central))
  outfile <- sprintf("%s_%s_central.pq", group, scenario)
  arrow::write_parquet(central, file.path(path, outfile))
}
