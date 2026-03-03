# Calculating DALYs for stochastic or central estimates

One modelling group does not provide DALYs, and we need to calculate
those, both for their central estimates (already uploaded to Montagu),
and their stochastic estimates (provided as CSV files). The DALYs
calculation is a simple function of the life expectancy of people (of a
given age and country in a given year), and various weighted
contributions of different burden outcomes.

## Usage

``` r
stoner_calculate_dalys(
  con,
  touchstone,
  data,
  dalys_params,
  life_table = NULL,
  year_min = 2000,
  year_max = 2100
)
```

## Arguments

- con:

  DBI connection to a Montagu database. Used for retrieving demographic
  data for life expectancy.

- touchstone:

  The touchstone (including version); the demographic data retrieved
  will be specific to this touchstone.

- data:

  A data.frame containing burden estimates - either central or
  stochastic. The columns for both kinds will include `country` (which
  may be a Montagu numerical id, or the 3-letter code), `year`, `age`,
  and a number of burden outcomes.Stochastic data will also have a
  `run_id`.

- dalys_params:

  A data.frame containing how to calculate dalys; each row represents a
  condition, and the four columns describe how to calculate the cost of
  that condition across the population in that year. For each condition,
  the `outcome` column must provide a burden outcome (which will be a
  column name in `data`). The `proportion` is the proportion of people
  reported as that outcome, who suffer this condition.
  `average_duration` is the number of years for which the condition is
  suffered, set to greater than 120 for a life-long period. And
  `disability_weight` is a measure of how severe the implications are of
  this particular condition. See
  http://ghdx.healthdata.org/record/ihme-data/gbd-2017-disability-weights
  for where these figures come from; the final figures are decided in
  discussion with the groups.

- life_table:

  If `NULL`, then the life table will be looked up and interpolated to
  yearly for age and time. This takes a bit of time, so the resulting
  data is part of what this function returns, and the life table part
  can be provided here to speed things up. If provided, it should be a
  data.frame with the columns `value` and `.code`; the latter is in the
  format country-year-age. The type of country in `.code` should match
  with the type of the country in `data`. The years and ages will be
  interpolated down to single-years.

- year_min:

  The first year of the range in which to calculate DALYs. (Default
  2000)

- year_max:

  The final year of the range in which to calculate DALYs. (Default
  2100)

## Value

A list with two named components; `life_table` is the result of calling
the life-table function, which can then be specified in subsequent calls
to `stoner_calculate_dalys`, and `data` is a version of the original
dataset, now with a `dalys` column. Note that if `data` already
contained a `dalys` column when provided to the function, that column
will be overwritten with the new results.
