# Produce `meta.csv` summary of the structure and content of a standardised stochastic data folder.

Create a `meta.csv` file in the root of the standardised stochastics.
The columns contain scalars of `touchstone`, `disease`, `group`,
`scenario` - and for each row, a semi-colon-separated lists for
`countries` and `outcomes`. This is useful for making the stochastic
explorer faster on startup (otherwise it has to sample all of the files
each time you run it) - and also it is a good general record of all the
stochastic data we have.

## Usage

``` r
stone_stochastic_make_meta(path)
```

## Arguments

- path:

  The root folder of the stochastic data.

## Details

This does mean that we should re-create the meta data each time we make
changes to the standardised stochastic data though.
