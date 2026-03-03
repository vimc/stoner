# Process stochastic data

Convert a modelling group's stochastic files into the summary format,
ready for later uploading to the Montagu data annex. Four files are
produced which reduce age to all-age-total, and under-5-total, by
calendar year, or birth-cohort year.

## Usage

``` r
stone_stochastic_process(
  con,
  modelling_group,
  disease,
  touchstone,
  scenarios,
  in_path,
  files,
  cert,
  index_start,
  index_end,
  out_path,
  pre_aggregation_path = NULL,
  outcomes = list(deaths = "deaths", cases = "cases", dalys = "dalys", yll = "yll"),
  dalys_recipe = NULL,
  runid_from_file = FALSE,
  allow_missing_disease = FALSE,
  upload_to_annex = FALSE,
  annex = NULL,
  allow_new_database = FALSE,
  bypass_cert_check = FALSE,
  testing = FALSE,
  lines = Inf,
  log_file = NULL,
  silent = FALSE
)
```

## Arguments

- con:

  DBI connection to production. Used for verifying certificate against
  expected properties

- modelling_group:

  The modelling group id

- disease:

  The disease

- touchstone:

  The touchstone (including version) for these estimates

- scenarios:

  A vector of scenario_descriptions. If the files parameter is of length
  more than 1, then it must be the same length as the number of
  scenarios, and a one-to-one mapping between the two is assumed.

- in_path:

  The folder containing the stochastic files

- files:

  Either a single string containing placeholders to indicate filenames,
  or a vector of files, one for each scenario. Placeholders can include
  :group :touchstone :scenario :disease and :index

- cert:

  Name of the certificate file accompanying the estimates

- index_start:

  A scalar or vector matching the length of scenarios. Each entry is
  either an integer or NA, indicating the first number in a sequence of
  files. NA implies there is a single file with no sequence. The
  placeholder :index in the filenames will be replaced with this.

- index_end:

  Similar to index_start, indicating the last number in a sequence of a
  files. Can be scalar, applying to all scenarios, or a vector with an
  entry for each scenario, with an integer value or NA in each case.

- out_path:

  Path to writing output files into

- pre_aggregation_path:

  Path to dir to write out pre age-disaggregated data into. If NULL then
  this is skipped.

- outcomes:

  A list of named vectors, where the name is the burden outcome, and the
  elements of the list are the column names in the stochastic files that
  should be summed to compute that outcome. The default is to expect
  outcomes `deaths`, `cases`, `dalys`, and `yll`, with single columns
  with the same names in the stochastic files.

- dalys_recipe:

  If DALYs must be calculated, you can supply a data frame here, and
  stoner will calculate DALYs using that recipe. The data frame must
  have names `outcome`, `proportion`, `average_duration` and
  `disability_weight`. See
  [stoner_calculate_dalys](https://vimc.github.io/stoner/reference/stoner_calculate_dalys.md).

- runid_from_file:

  Occasionally groups have omitted the run_id from the stochastic file,
  and provided 200 files, one per run_id. Set runid_from_file to TRUE if
  this is the case, to deduce the run_id from the filenames. The
  index_start and index_end must be 1 and 200 in this case.

- allow_missing_disease:

  Occasionally groups have omitted the disease column from their
  stochastic data. Set this to TRUE to expect that circumstance, and
  avoid generating warnings.

- upload_to_annex:

  Set to TRUE if you want to upload the results straight into annex.
  (Files will still be created, as the upload is relatively fast;
  creating the csvs is slower and worth caching)

- annex:

  DBI connection to annex, used if upload_to_annex is TRUE.

- allow_new_database:

  If uploading, then set this to TRUE to enable creating the
  stochastic_file table if it is not found.

- bypass_cert_check:

  If TRUE, then no checks are carried out on the parameter certificate
  (if provided).

- testing:

  For internal use only.

- lines:

  Number of lines to read from each file, Inf by default to read all
  lines. Set a lower number for testing subset of process before doing
  the full run.

- log_file:

  Path to file to save logs to, NULL to not log to file. If file exists
  it will be appended to, otherwise file will be created.

- silent:

  TRUE to silence console logs.
