# stoner

[![Travis-CI Build Status](https://travis-ci.org/vimc/stoner.svg?branch=master)](https://travis-ci.org/vimc/stoner)
[![codecov.io](https://codecov.io/github/vimc/stoner/coverage.svg?branch=master)](https://codecov.io/github/vimc/stoner?branch=master)

Stoner is a helper package for writing montagu-imports concerning touchstones and associated responsibilities and 
expectations. It allows creation of, or changes to a touchstone to be specified by csv files.

# Using stoner

* `install.packages("stoner")`
* Create a [montagu-import](https://github.com/vimc/montagu-imports).
* Write csv files in a folder called `meta` inside that import folder.
* For your *extract* call `stoner::extract(path, con)`
* For your *test-extract* call `stoner::test_extract(extracted_data)`
* For your *transform* call `stoner::transform(extracted_data)`
* For your *test-transform* call `stoner::test_transform(transform_data)`
* For your *load* call `stoner::load(transformed_data, con)`

The above is the simplest case. You should also write some load tests as usual,
but these can now concentrate entirely on what rows the particularly import is
expected to add. See the vignette for more detail.


