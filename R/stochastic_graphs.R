##' Draw a stochastic plot showing all the different runs, with the mean,
##' median, 5% and 95% quantiles shown.
##'
##' @export
##' @title Stochastic plot
##' @import dplyr
##' @importFrom rlang :=
##' @import arrow
##' @importFrom graphics lines
##' @importFrom stats quantile median
##' @param base The folder in which the standardised stochastic files are found.
##' @param touchstone The touchstone name (for the graph title)
##' @param disease The disease, used for building the filename and graph title.
##' @param group The modelling group, used in the filename and graph title.
##' @param country The country to plot.
##' @param scenario The scenario to plot.
##' @param outcome The outcome to plot, for example `deaths`, `cases`, `dalys` or
##' since 2023, `yll`.
##' @param ages A vector of one or more ages to be selected and aggregated, or
##' if left as NULL, then all ages are used and aggregated.
##' @param by_cohort If TRUE, then age is subtracted from year to convert it to
##' year of birth before aggregating.
##' @param log If TRUE, then use a logged y-axis.
##' @param packit_id If set, then read central burden estimates from a file
##' within a packit on the Montagu packit server.
##' @param packit_file Used with packit_id to specify the filename of an RDS
##' file providing burden estimates. We expect to find scenario, year, age,
##' country, burden_outcome and value fields in the table.
##' @param include_quantiles Default TRUE, select whether to plot the
##' 5% and 95% quantile lines.
##' @param include_mean Default TRUE, select whether to plot the mean.
##' @param include_median Default TRUE, select whether to plot the median.
##' @param scenario2 Default NULL; if set, then the burdens from this
##' scenario will be subtracted from those in `scenario` - ie, this plots
##' an impact graph of applying the second scenario. For many graphs that
##' use this, the result will be positive numbers, representing cases
##' or deaths averted.

stone_stochastic_graph <- function(base, touchstone, disease, group, country,
                                   scenario, outcome, ages = NULL,
                                   by_cohort = FALSE, log = FALSE,
                                   packit_id = NULL, packit_file = NULL,
                                   include_quantiles = TRUE,
                                   include_mean = TRUE,
                                   include_median = TRUE,
                                   scenario2 = NULL) {

  d <- prepare_graph_data(base, touchstone, disease, group, country,
                         scenario, outcome, ages, by_cohort)
  if (is.null(ages)) {
    age <- "all ages"
  } else if (all(sort(ages) == min(ages):max(ages))) {
    age <- sprintf("age %d..%d", min(ages), max(ages))
  } else age <- "selected ages"

  title <- sprintf("%s, %s, %s, %s\n%s, %s\n", touchstone, disease, group, age,
                   scenario, country)
  outcome_ylab <- outcome

  if (!is.null(scenario2)) {
    d2 <- prepare_graph_data(base, touchstone, disease, group, country,
                            scenario2, outcome, ages, by_cohort)
    title <- sprintf("%s, %s, %s, %s\n%s, %s\n", touchstone, disease, group, age,
                     sprintf("Impact of %s ->\n%s", scenario, scenario2), country)
    d <- d[order(d$year, d$run_id), ]
    d2 <- d2[order(d2$year, d2$run_id), ]
    d[[outcome]] <- d[[outcome]] - d2[[outcome]]
    outcome_ylab <- paste(outcome_ylab, "averted")
  }

  runs <- max(d$run_id)
  miny <- max(1, min(d[[outcome]]))
  maxy <- max(d[[outcome]])
  log <- if (log) "y" else ""

  if (!is.null(packit_id)) {
    central <- prepare_central_data(packit_id, packit_file)


  }
  par(mar = c(5, 4, 5, 2))
  plot(ylab = outcome_ylab, xlab = if (by_cohort) "Birth Cohort" else "year",
       x = d$year[d$run_id == 1], y = d[[outcome]][d$run_id == 1], type="l",
       col = "#b0b0b0", ylim = c(miny, maxy), main = title, log = log)

  for (i in 2:runs) {
    lines(x = d$year[d$run_id == i], y = d[[outcome]][d$run_id == i],
          col = "#b0b0b0")
  }

  avgs <- d %>% group_by(.data$year) %>%
    summarise(
      mean   = mean(.data[[outcome]]),
      median = median(.data[[outcome]]),
      q05 = quantile(.data[[outcome]], 0.05),
      q95 = quantile(.data[[outcome]], 0.95),
      .groups = "drop"
    )
  if (include_mean) {
    lines(x = avgs$year, y = avgs$mean, col = "#ff4040", lwd = 2)
  }
  if (include_median) {
    lines(x = avgs$year, y = avgs$median, col = "#00ff00", lwd = 2)
  }
  if (include_quantiles) {
    lines(x = avgs$year, y = avgs$q05, col = "#202020", lwd = 2)
    lines(x = avgs$year, y = avgs$q95, col = "#202020", lwd = 2)
  }
  recordPlot()
}


prepare_graph_data <- function(base, touchstone, disease, group, country,
                               scenario, outcome, ages, by_cohort) {

  pq <- sprintf("%s/%s/%s_%s/%s_%s_%s.pq", base, touchstone, disease,
                group, group, scenario, country)

  d <- arrow::read_parquet(pq)
  if (!is.null(ages)) {
    d <- d[d$age %in% ages, ]
  }
  if (by_cohort) {
    d$year <- d$year - d$age
  }
  d <- d[, c("run_id", "year", "age", outcome)]
  d <- d %>% group_by(.data$run_id, .data$year) %>%
    summarise(
      !!outcome := sum(.data[[outcome]], na.rm = TRUE),
      .groups = "drop")
  d
}

prepare_central_data <- function(packit_id, packit_file,
  country, scenario, outcome, ages, by_cohort) {

  central <- readRDS(fetch_packit(packit_id, packit_file))
  central <- central[central$country == country, ]
  central <- central[central$scenario == scenario, ]
  central <- central[central$burden_outcome == outcome, ]
  if (!is.null(ages)) {
    central <- central[d$age %in% ages, ]
  }
  if (by_cohort) {
    central$year <- central$year - d$age
  }
  names(central)[names(central) == "value"] <- outcome
  central <- central %>% group_by(.data$year) %>%
    summarise(
      !!outcome := sum(.data[[outcome]], na.rm = TRUE),
      .groups = "drop")
  central$run_id <- 0
  central
}

##' Launch a Shiny app to allow interactive plotting of
##' standardised stochastic data, burden estimates,
##' impacts, comparisons between touchstones, and
##' comparisons between modelling groups.
##'
##' @export
##' @title Stochastic plot
##'
stochastic_explorer <- function(
  data_dir = "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics") {
  assign("data_dir", data_dir, envir = .GlobalEnv)
  shiny::runApp(system.file("app", package = "stoner"))
}
