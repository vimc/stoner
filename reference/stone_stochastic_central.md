# Produce central average from stochastic files.

Create a central stochastic file from a set of standardised files
created by stone_stochastic_standardise. This is for a single scenario,
and we'll expect to find a file for each country containing all the
runs. We'll produce one file with the mean (or median) of the
stochastics, and all countries in one file.

## Usage

``` r
stone_stochastic_central(
  base,
  touchstone,
  disease,
  group,
  scenario,
  avg_method = mean
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

- scenario:

  The scenario to plot.

- avg_method:

  The function to use for averaging the outcomes, probably mean (the
  default) or median.
