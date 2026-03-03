# Test the transformed data.

Run all generic tests on the provided transformed data. This should be
called from `test-extract.R` in a montagu-imports import. It tests that
the csv metadata is syntactically valid and as expected, and that
querying related database tables has also worked. After calling
`stoner::stone_text_extract(extracted_data)`, you should write tests
that are specific to the metadata for the particular import you are
writing.

## Usage

``` r
stone_test_transform(transformed_data)
```

## Arguments

- transformed_data:

  A list of `data.frame`s produced by the transform function. They will
  have names matching database tables in montagu, and columns matching
  the columns of those tables.

## Details

See the vignette for information on the stoner-specific tests.
