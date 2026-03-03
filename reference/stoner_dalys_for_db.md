# Create CSV with DALYs for an uploaded burden estimate set

When a group has uploaded a burden estimate set without DALYs, this
function can retrieve that burden estimate set, extend it with DALYs,
and write a new CSV file ready for uploading through the Montagu portal.

## Usage

``` r
stoner_dalys_for_db(
  con,
  dalys_params,
  modelling_group = NULL,
  disease = NULL,
  touchstone = NULL,
  scenario = NULL,
  burden_estimate_set_id = NULL,
  output_file = NULL,
  life_table = NULL,
  year_min = 2000,
  year_max = 2100
)
```

## Arguments

- con:

  DBI connection to a Montagu database, for retrieving the
  burden_estimate set.

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

- modelling_group:

  If the modelling group, disease, touchstone and scenario are all
  provided, then we lookup the current (most recently uploaded) burden
  estimate set matching those details.

- disease:

  If specified, the name of the disease in question

- touchstone:

  If specified, the touchstone (including version) for the set.

- scenario:

  If specified, the name of the scenario for the set.

- burden_estimate_set_id:

  Alternatively, if modelling_group, disease, touchstone and scenario
  are NULL, the numerical id of a particular burden estimate set can be
  specified here, to extend that estimate set with DALYs. Otherwise,
  leave as NULL to look up by the other four fields.

- output_file:

  If provided, then write an output pq file with the additional DALYs
  field to this filename. The file will be ready to be uploaded to
  Montagu.

- life_table:

  If provided, then re-use the life table provided by a previous call to
  this function. Otherwise, it can be left at NULL to generate the
  life-table.

- year_min:

  The first year of the range in which to calculate DALYs. (Default
  2000)

- year_max:

  The final year of the range in which to calculate DALYs. (Default
  2100)

## Value

A list with two components, `data` is a data.frame of the complete DALYs
extended data, and `life_table` is the life table used for calculating
the DALYs, should that be required again.
