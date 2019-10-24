##' Run all generic tests on the provided transformed data. This should be
##' called from \code{test-extract.R} in a montagu-imports import. It tests
##' that the csv metadata is syntactically valid and as expected, and
##' that querying related database tables has also worked. After
##' calling \code{stoner::text_extract(extracted_data)}, you should
##' write tests that are specific to the metadata for the particular
##' import you are writing.
##'
##' See the vignette for information on the stoner-specific tests.
##'
##' @export
##' @title Test the transformed data.
##' @param transformed_data A list of \code{data.frame}s produced by the transform
##'        function. They will have names matching database tables in montagu, and
##'        columns matching the columns of those tables.

test_transform <- function(transformed_data) {
  test_transform_touchstone(transformed_data)
  test_transform_scenario_description(transformed_data)
  test_transform_touchstone_demographic_dataset(transformed_data)
  test_transform_touchstone_country(transformed_data)
  test_transform_burden_estimate_expectation(transformed_data)
  test_transform_responsibility(transformed_data)
  test_transform_burden_estimate_country_expectation(transformed_data)
}
