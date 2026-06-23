filter_string <- function(s, units) {
  if (is.null(s)) return(sprintf("all %s", units))
  s <- as.numeric(s)
  if (identical(unique(sort(s)), as.numeric(min(s):max(s)))) {
    sprintf("%s %d..%d", units, min(s), max(s))
  } else sprintf("selected %s", units)
}

check_arg_counts <- function(touchstones, disease, groups, scenarios) {

  if (!length(touchstones) %in% 1:2) {
    cli::cli_abort("Only specify one or two touchstones.")
  }

  if (!length(groups) %in% 1:2) {
    cli::cli_abort("Only specify one or two modelling groups.")
  }

  if ((length(touchstones) == 2) && (length(groups) ==2)) {
    cli::cli_abort("Only one of `touchstones` or `groups` can be plural.")
  }

  if (!length(scenarios) %in% 1:2) {
    cli::cli_abort("Only specify one or two scenarios.")
  }

  if (length(disease) != 1) {
    cli::cli_abort("Only specify one disease")
  }
}


check_args <- function(base, touchstones, disease, groups, country,
                       scenarios, outcome, xaxis) {

  # Check meta.csv exists.

  if (!file.exists(file.path(base, "meta.csv"))) {
    cli::cli_abort("Please call stone_stochastic_meta(path) first")
  }
  meta <- read.csv(file.path(base, "meta.csv"))

  # Check all touchstones exist

  missing_touchstones <- touchstones[!touchstones %in% meta$touchstone]
  if (length(missing_touchstones) > 0) {
    cli::cli_abort("Touchstone not found: {missing_touchstones}")
  }
  meta <- meta[meta$touchstone %in% touchstones, ]

  # Check all diseases exist in all touchstones

  for (touchstone in touchstones) {
    if (!disease %in% meta$disease[meta$touchstone == touchstone]) {
      cli::cli_abort("Disease {disease} not found in touchstone {touchstone}")
    }
  }

  meta <- meta[meta$disease %in% disease, ]

  # Modelling groups exist in all touchstones

  for (touchstone in touchstones) {
    missing_groups <- groups[
      !groups %in% meta$group[meta$touchstone == touchstone]]
    if (length(missing_groups) > 0) {
      cli::cli_abort("Groups not found in {touchstone}: {missing_groups}")
    }
  }

  meta <- meta[meta$group %in% groups, ]

  for (scenario in scenarios) {
    for (group in groups) {
      for (touchstone in touchstones) {
        if (nrow(meta[meta$touchstone == touchstone &
                      meta$group == group &
                      meta$scenario == scenario, ]) == 0) {
          cli::cli_abort("Scenario {scenario} not found in {touchstone}, {group}")
        }
      }
    }
  }

  meta <- meta[meta$scenario %in% scenarios, ]
  countries <- strsplit(meta$countries, ";")
  present <- unlist(lapply(countries, function(x) country %in% x))
  if (any(!present)) {
    present <- which(!present)[1]
    m <- meta[present, ]
    cli::cli_abort(
      "Country {country} not found in {m$touchstone}, {m$group}, {m$scenario}")
  }

  outcomes <- strsplit(meta$outcomes, ";")
  present <- unlist(lapply(outcomes, function(x) outcome %in% x))
  if (any(!present)) {
    present <- which(!present)[1]
    m <- meta[present, ]
    cli::cli_abort(
      "Outcome {outcome} not found in {m$touchstone}, {m$group}, {m$scenario}")
  }

  if (!isTRUE(tolower(xaxis) %in% c("time", "age"))) {
    cli::cli_abort("`xaxis` must be either `time` or `age`.")
  }
}

##' A feature-rich-ish graph plotting function, which can show the stochastic
##' line for an outcome for different runs, the mean, median and quantiles.
##' If two scenarios are specified, then the burden difference between
##' scenarios is plotted. Additionally, if multiple groups or multiple
##' touchstones are specified, then separate graphs with the same scaling are
##' plotted to allow comparison.
##'
##' Graphs can have calendar year or birth cohort on the y-axis for
##' time-series plots, in which case age is aggregated and the ages to be
##' included can be specified. Alternatively, the x-axis can be age aggregated
##' over time, where the calendar years to be aggregated can be specified.
##'
##' Finally, it can also include a central estimate provided as a file within
##' a packit, from the montagu reporting portal.
##'
##' @export
##' @title Stochastic plot
##' @import dplyr
##' @importFrom rlang :=
##' @import arrow
##' @importFrom graphics lines par
##' @importFrom stats quantile median
##' @importFrom grDevices recordPlot
##' @param base The root folder in which the standardised stochastic files are
##' found. Within it should be folders with touchstone names.
##' @param touchstones A touchstone to plot, or a pair  of touchstones to
##' compare. If a pair, then only one modelling group can be specified below.
##' @param disease The disease to display.
##' @param groups The modelling group to plot, or a pair of modelling
##' groups to be compared, in which case, only one touchstone can be specified.
##' @param country The country to plot.
##' @param scenarios A scenario to plot data for, or a pair of scenarios in
##' which case we plot the outcome for the first scenario, subtract the second.
##' @param outcome The outcome to plot, for example `deaths`, `cases`, `dalys`
##' or since 2023, `yll`.
##' @param xaxis The default is "time", meaning the x-axis is either calendar
##' year, or birth cohort, depending on the `by_cohort` parameter. This means
##' and age gets filtered by the `filter` parameter, and then aggregated.
##' Alternatively, if set to "age", then age will be on the x-axis, and the
##' `filter` parameter specifies either the years, or the birth cohorts to
##' select (depending on the `by_cohort` parameter), before aggregation.
##' @param filter Filter either ages, years, or birth cohort years to a range -
##' see the comments above on the `xaxis` parametern. The filter is a vector
##' of ages, or years, or can be NULL to do no filtering.
##' @param by_cohort If TRUE, then age is subtracted from year to convert it to
##' year of birth before aggregating. See the `xaxis` and `filter` parameters.
##' @param log If TRUE, then use a logged y-axis.
##' @param packit_id If set, then read central burden estimates from a file
##' within a packit on the Montagu packit server.
##' @param packit_file Used with packit_id to specify the filename of an RDS
##' file providing burden estimates. We expect to find scenario, year, age,
##' country, burden_outcome and value fields in the table.
##' @param include_stochastics Default TRUE, select whether to draw the
##' individual stochastic lines.
##' @param include_quantiles Default TRUE, select whether to plot the
##' 5% and 95% quantile lines.
##' @param include_mean Default TRUE, select whether to plot the mean.
##' @param include_median Default TRUE, select whether to plot the median.
##' @returns A list of either one or two graphs, depending on the parameters
##' selected. The last (or only) graph will be the one displayed
##' automatically in RStudio, so if you do use this function to plot
##' multiple graphs, call `plot` on each one to see it.

stone_stochastic_graph <- function(base,
                                   touchstones, disease, groups, country,
                                   scenarios, outcome,
                                   xaxis = "time",
                                   filter = NULL,
                                   by_cohort = FALSE, log = FALSE,
                                   packit_id = NULL, packit_file = NULL,
                                   include_stochastics = TRUE,
                                   include_quantiles = TRUE,
                                   include_mean = TRUE,
                                   include_median = TRUE) {

  check_arg_counts(touchstones, disease, groups, scenarios)
  check_args(base, touchstones, disease, groups, country, scenarios, outcome,
             xaxis)

  xaxis <- tolower(xaxis)
  outcome <- tolower(outcome)

  miny <- Inf
  maxy <- -Inf
  minx <- Inf
  maxx <- -Inf

  # Fetch the packit data if wanted

  if (!is.null(packit_id)) {
    central <- get_packit_data(packit_id, packit_file,
                               country, scenarios, outcome, by_cohort)
    if (xaxis == "age") {
      central <- aggregate_by_age(central, outcome, filter)
      minx <- 0
      maxx <- 100
    } else {
      central <- aggregate_by_year(central, outcome, filter)
      miny <- min(central[[outcome]])
      maxy <- max(central[[outcome]])
      minx <- min(central$year)
      maxx <- max(central$year)
    }
  }

  # Fetch the data from the files. We may end up with 1 or 2 graphs here.

  data <- list()
  n_graphs <- 0
  for (touchstone in touchstones) {
    for (group in groups) {
      n_graphs <- n_graphs + 1
      data[[n_graphs]] <- get_graph_data(base, touchstone, disease, group,
                                         country, scenarios, outcome, by_cohort)
    }
  }

  # Filter and aggregate the data, finding bounds

  for (i in 1:n_graphs) {
    if (xaxis == "age") {
      data[[i]] <- aggregate_by_age(data[[i]], outcome, filter)
      minx <- 0
      maxx <- 100
    } else {
      data[[i]] <- aggregate_by_year(data[[i]], outcome, filter)
      minx <- min(minx, data[[i]]$year)
      maxx <- max(maxx, data[[i]]$year)
    }
    miny <- min(miny, data[[i]][[outcome]])
    maxy <- max(maxy, data[[i]][[outcome]])
  }

  # Sort out titles and labels common to both graphs
  units <- if (xaxis == "time") "ages" else if (by_cohort) "cohorts" else "years"
  filter_title <- filter_string(filter, units)
  outcome_ylab <- outcome
  scenario_title <- scenarios[1]

  if (log) {
    log <- "y"
    miny <- max(1, miny)
  } else {
    log <- ""
  }

  if (length(scenarios) == 2) {
    outcome_ylab <- paste(outcome_ylab, "averted")
    scenario_title <- sprintf("Difference of %s ->\n%s", scenarios[1],
                              scenarios[2])
  }


  # Plot the one or two graphs

  lapply(1:n_graphs, function(i) {
    touchstone_title <- touchstones[min(i, length(touchstones))]
    group_title <- groups[min(i, length(groups))]
    title <- sprintf("%s, %s, %s, %s\n%s, %s\n",
                     touchstone_title, disease, group_title,
                     filter_title,
                     scenario_title, country)


    if (xaxis == "age") {
      xlabel <- "Age"
      xfield <- "age"
    } else {
      xlabel <- if (by_cohort) "Birth Cohort" else "Year"
      xfield <- "year"
    }


    runs <- max(data[[i]]$run_id)
    par(mar = c(5, 4, 5, 2))
    plot(ylab = outcome_ylab, xlab = xlabel,
         x = NULL, y = NULL,
         col = "#b0b0b0", xlim = c(minx, maxx), ylim = c(miny, maxy),
         main = title, log = log)

    if (include_stochastics) {
      for (j in seq_len(runs)) {
        lines(x = data[[i]][[xfield]][data[[i]]$run_id == j],
              y = data[[i]][[outcome]][data[[i]]$run_id == j],
              col = "#b0b0b0")
      }
    }

    if (include_mean | include_median | include_quantiles) {
      avgs <- data[[i]] %>% group_by(.data[[xfield]]) %>%
        summarise(
          mean   = mean(.data[[outcome]]),
          median = median(.data[[outcome]]),
          q05 = quantile(.data[[outcome]], 0.05),
          q95 = quantile(.data[[outcome]], 0.95),
          .groups = "drop"
        )
      if (include_mean) {
        lines(x = avgs[[xfield]], y = avgs$mean, col = "#ff4040", lwd = 2)
      }
      if (include_median) {
        lines(x = avgs[[xfield]], y = avgs$median, col = "#00ff00", lwd = 2)
      }
      if (include_quantiles) {
        lines(x = avgs[[xfield]], y = avgs$q05, col = "#202020", lwd = 2)
        lines(x = avgs[[xfield]], y = avgs$q95, col = "#202020", lwd = 2)
      }
    }

    if (!is.null(packit_id)) {
      lines(x = central[[xfield]], y = central[[outcome]],
            col = "#2020ff", lwd = 2)
    }

    recordPlot()
  })
}

get_burden_difference <- function(data, outcome) {
  if (length(data) == 2) {
    data[[1]][[outcome]] <- data[[1]][[outcome]] - data[[2]][[outcome]]
  }
  data[[1]]
}

# Here we'll fetch the data for one graph. `scenarios` might be length 1
# (which is easy) or 2 - in which case we'll subtract the outcome values of
# the second from the first. We can also do the by_cohort calculation in here

get_graph_data <- function(base, touchstone, disease, group, country,
                           scenarios, outcome, by_cohort) {

  data <- lapply(scenarios, function(scenario) {
    pq <- sprintf("%s/%s/%s_%s/%s_%s_%s.pq", base, touchstone, disease,
                  group, group, scenario, country)

    if (!file.exists(pq)) {
      cli::cli_abort("Couldn't find file {pq} - check files.")
    }
    d <- arrow::read_parquet(pq)
    d$year <- if (by_cohort) d$year - d$age else d$year
    d <- d[order(d$year ,d$age, d$run_id), ]
    d[, c("run_id", "year", "age", outcome)]
  })
  get_burden_difference(data, outcome)
}

aggregate_by_year <- function(d, outcome, ages = NULL) {
  if (!is.null(ages)) {
    d <- d[d$age %in% ages, ]
  }
  d %>% group_by(.data$run_id, .data$year) %>%
    summarise(
      !!outcome := sum(.data[[outcome]], na.rm = TRUE),
      .groups = "drop")
}

aggregate_by_age <- function(d, outcome, years = NULL) {
  if (!is.null(years)) {
    d <- d[d$year %in% years, ]
  }
  d %>% group_by(.data$run_id, .data$age) %>%
    summarise(
      !!outcome := sum(.data[[outcome]], na.rm = TRUE),
      .groups = "drop")
}

get_packit_data <- function(packit_id, packit_file,
  country, scenarios, outcome, by_cohort) {

  central <- readRDS(fetch_packit(packit_id, packit_file))
  central <- central[central$country == country, ]
  central <- central[central$burden_outcome == outcome, ]

  data <- lapply(scenarios, function(scenario) {
    d <- central[central$scenario == scenario, ]
    d$year <- if (by_cohort) d$year - d$age else d$year
    names(d)[names(d) == "value"] <- outcome
    d[order(d$year, d$age), ]
  })

  d <- get_burden_difference(data, outcome)
  d <- as.data.frame(d[, c("year", "age", outcome)])
  d$run_id <- 1
  d
}

##' Launch a Shiny app to allow interactive plotting of
##' standardised stochastic data, burden estimates,
##' impacts, comparisons between touchstones, and
##' comparisons between modelling groups.
##'
##' @export
##' @importFrom shiny runApp
##' @title Stochastic plot
##' @param data_dir The location of the standardised stochastic folder
##' hierarchy; this can be a local path, a fully-qualified network path on
##' windows, or a mount point on linux or Mac.
stochastic_explorer <- function(
  data_dir = "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics") {
  if (!dir.exists(data_dir)) {
    cli::cli_abort(c(
      "x" = "Cannot access the path/mount: {.path {data_dir}}",
      "i" = "Please check you can see this path normally. If not:",
      "*" = "You need ZScaler on and connected if you're not within DIDE",
      "*" = "(But strongly advise remote desktop into DIDE - large files)",
      "*" = "On linux, ensure the mount is correctly set up.",
      "*" = "Check with DIDE IT that you have access to VIMC files",
      "*" = "Check your general internet access."
    ))
  }

  assign("data_dir", data_dir, envir = .GlobalEnv)
  runApp(system.file("app", package = "stoner"))
}
