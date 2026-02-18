##' Draw a stochastic plot showing all the different runs, with the mean,
##' median, 5% and 95% quantiles shown.
##'
##' @export
##' @title Stochastic plot
##' @import dplyr
##' @import arrow
##' @importFrom graphics lines
##' @importFrom stats quantile median
##' @param base The folder in which the standardised stochastic files are found.
##' @param touchstone The touchstone name (for the graph title)
##' @param disease The disease, used for building the filename and graph title.
##' @param group The modelling group, used in the filename and graph title.
##' @param country The country to plot.
##' @param scenario The scenario to plot.
##' @param ages A vector of one or more ages to be selected and aggregated, or
##' if left as NULL, then all ages are used and aggregated.
##' @param by_cohort If TRUE, then age is subtracted from year to convert it to
##' year of birth before aggregating.
##' @param log If TRUE, then use a logged y-axis.

stone_stochastic_graph <- function(base, touchstone, disease, group, country,
                                   scenario, outcome, ages = NULL,
                                   by_cohort = FALSE, log = FALSE) {

  pq <- sprintf("%s/%s/%s_%s/%s_%s_%s.pq", base, touchstone, disease,
                group, group, scenario, country)

  title <- sprintf("%s, %s, %s\n%s, %s\n %s", touchstone, disease, group, scenario, country, age_string)
  log <- if (log) "y" else "n"
  d <- arrow::read_parquet(pq)
  if (!is.null(ages)) {
    d <- d[d$age %in% ages, ]
  }
  if (by_cohort) {
    d$year <- d$year - d$age
  }
  d <- d[, c("run_id", "year", "age", outcome)]
  d <- d %>% group_by(run_id, year) %>%
             summarise(
               !!outcome := sum(.data[[outcome]], na.rm = TRUE),
             .groups = "drop")

  miny <- max(1, min(d[[outcome]]))
  maxy <- max(d[[outcome]])
  plot(ylab = outcome, xlab = if (by_cohort) "Birth Cohort" else "year",
       x = d$year[d$run_id == 1], y = d[[outcome]][d$run_id == 1], type="l",
       col = "#b0b0b0", ylim = c(miny, maxy), main = title, log = log)
  for (i in 2:200) {
    lines(x = d$year[d$run_id == i], y = d[[outcome]][d$run_id == i],
         col = "#b0b0b0")
  }

  avgs <- d %>% group_by(year) %>%
    summarise(
      mean   = mean(.data[[outcome]]),
      median = median(.data[[outcome]]),
      q05 = quantile(.data[[outcome]], 0.05),
      q95 = quantile(.data[[outcome]], 0.95),
      .groups = "drop"
    )
  lines(x = avgs$year, y = avgs$mean, col = "#ff4040", lwd = 2)
  lines(x = avgs$year, y = avgs$median, col = "#00ff00", lwd = 2)
  lines(x = avgs$year, y = avgs$q05, col = "#202020", lwd = 2)
  lines(x = avgs$year, y = avgs$q95, col = "#202020", lwd = 2)
}
