# Standardise stochastic data files

Convert a modelling group's stochastic files into an intermediate format
including all the original data, separated by country and scenario, and
in PQ format for later processing. As much as possible, we'll try and
detect the input formats.

## Usage

``` r
stone_stochastic_standardise(
  group,
  in_path,
  out_path,
  scenarios,
  files,
  index = 1,
  rubella_fix = TRUE,
  missing_run_id_fix = TRUE
)
```

## Arguments

- group:

  The modelling group.

- in_path:

  The folder or network path where the original files are found.

- out_path:

  The folder or network path to write output files to.

- scenarios:

  A vector of strings giving each scenario name.

- files:

  This can either be a vector of strings of the same length to the
  vector of scenarios, in which case each entry should match the
  scenario, providing the filename for the original uploads for that
  scenario. Most groups provide files numbered between 1 and 200 for
  their stochastic runs; replace that number with `:index` in the
  filename to match. Alternatively, files can be a single entry
  containing the string `:scenario`; in this case, files must exist that
  match each entry in the `scenarios` parameter, and the same file
  string can be used to match all of them (perhaps additionaly wiht
  `:index`).

- index:

  This is usually a vector of ints, `1:200` to match the range of
  stochastic files uploaded per scenario. A few groups upload one large
  file containing everything, in which case `:index` shouldn't occur in
  the `files` parameter, and should be omitted.

- rubella_fix:

  Historically Rubella uploads used the burden outcome
  `rubella_deaths_congenital` and `rubella_cases_congenital` instead of
  the simpler `deaths` and `cases`, and additionally provided a
  `rubella_infections` field. `rubella_fix` needs to be TRUE to
  standardise these to the simpler names. Processing Rubella stochastic
  files without this set to TRUE will fail - so while we should always
  do this, keeping the parameter makes it more clear in the code what
  we're doing and why.

- missing_run_id_fix:

  Some groups in the past have omitted run_id from the files, but
  included them in the filenames. This fix inserts that into the files
  if the index parameter indicates we have 200 runs to process.
