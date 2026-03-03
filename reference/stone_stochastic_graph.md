# Stochastic plot

Draw a stochastic plot showing all the different runs, with the mean,
median, 5% and 95% quantiles shown.

## Usage

``` r
stone_stochastic_graph(
  base,
  touchstone,
  disease,
  group,
  country,
  scenario,
  outcome,
  ages = NULL,
  by_cohort = FALSE,
  log = FALSE
)
```

## Arguments

- base:

  The folder in which the standardised stochastic files are found.

- touchstone:

  The touchstone name (for the graph title)

- disease:

  The disease, used for building the filename and graph title.

- group:

  The modelling group, used in the filename and graph title.

- country:

  The country to plot.

- scenario:

  The scenario to plot.

- outcome:

  The outcome to plot, for example `deaths`, `cases`, `dalys` or since
  2023, `yll`.

- ages:

  A vector of one or more ages to be selected and aggregated, or if left
  as NULL, then all ages are used and aggregated.

- by_cohort:

  If TRUE, then age is subtracted from year to convert it to year of
  birth before aggregating.

- log:

  If TRUE, then use a logged y-axis.
