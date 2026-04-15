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
  log = FALSE,
  packit_id = NULL,
  packit_file = NULL,
  include_quantiles = TRUE,
  include_mean = TRUE,
  include_median = TRUE,
  scenario2 = NULL
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

- packit_id:

  If set, then read central burden estimates from a file within a packit
  on the Montagu packit server.

- packit_file:

  Used with packit_id to specify the filename of an RDS file providing
  burden estimates. We expect to find scenario, year, age, country,
  burden_outcome and value fields in the table.

- include_quantiles:

  Default TRUE, select whether to plot the 5% and 95% quantile lines.

- include_mean:

  Default TRUE, select whether to plot the mean.

- include_median:

  Default TRUE, select whether to plot the median.

- scenario2:

  Default NULL; if set, then the burdens from this scenario will be
  subtracted from those in `scenario` - ie, this plots an impact graph
  of applying the second scenario. For many graphs that use this, the
  result will be positive numbers, representing cases or deaths averted.
