# Calculate life expectancy table

Normally this will be called internally from `stoner_calculate_dalys`,
but for testing purposes is also exposed.

## Usage

``` r
stoner_life_table(con, touchstone, year_min, year_max, char_countries = TRUE)
```

## Arguments

- con:

  DBI connection to a Montagu database. Used for retrieving demographic
  data for life expectancy.

- touchstone:

  The touchstone (including version); the demographic data retrieved
  will be specific to this touchstone.

- year_min:

  The first year of the range in which to calculate DALYs. (Default
  2000)

- year_max:

  The final year of the range in which to calculate DALYs. (Default
  2100)

- char_countries:

  A logical that if TRUE specifies that the countries in the resulting
  life table should be expressed in 3-character form, or otherwise
  numerical.

## Value

A data.frame with columns `value` and `.code`. The value is the expected
years of life left for a person in a particular country, a particular
year, and of a particular age. The `.code` is in the form
`country-year-age`, and both year and age are interpolated to single
years (whereas the UNWPP life table data is mostly in 5-year bands).
