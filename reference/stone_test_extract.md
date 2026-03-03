# Test the extracted data.

Run all generic tests on the provided extracted data. This should be
called from `test-extract.R` in a montagu-imports import. It tests that
the csv metadata is syntactically valid and as expected, and that
querying related database tables has also worked. After calling
`stoner::stone_text_extract(extracted_data)`, you should write tests
that are specific to the metadata for the particular import you are
writing.

## Usage

``` r
stone_test_extract(extracted_data)
```

## Arguments

- extracted_data:

  A list of `data.frame`s and/or values,

## Details

See the vignette for information on the stoner-specific tests.
