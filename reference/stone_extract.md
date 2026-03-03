# Extract touchstone-relevant data

Extract touchstone-relevant data from sources. This should be called
with `stoner::stone_extract(path, con`, from within the extract code of
a montagu-import, providing the same arguments provided to the
montagu-import extract function.

## Usage

``` r
stone_extract(path, con)
```

## Arguments

- path:

  Path to the import project root used for finding any local data.

- con:

  The active DBI connection for extracting any data.

## Value

A list of named data frames and/or named values representing the
extracted data.

## Details

See the vignette for information about the specifics of the extraction.
