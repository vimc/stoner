##' Run all generic tests on the provided extracted data. This should be
##' called from \code{test-extract.R} in a montagu-imports import. It tests
##' that the csv metadata is syntactically valid and as expected, and
##' that querying related database tables has also worked. After
##' calling \code{stoner::stone_text_extract(extracted_data)}, you should
##' write tests that are specific to the metadata for the particular
##' import you are writing.
##'
##' See the vignette for information on the stoner-specific tests.
##'
##' @export
##' @title Test the extracted data.
##' @param extracted_data A list of \code{data.frame}s and/or values,

stone_test_extract <- function(extracted_data) {
  test_extract_touchstone(extracted_data)
  test_extract_scenario_type(extracted_data)
  test_extract_scenario_description(extracted_data)
  test_extract_touchstone_demographic_dataset(extracted_data)
  test_extract_touchstone_country(extracted_data)
  test_extract_responsibilities(extracted_data)
  test_extract_fast_forward(extracted_data)
}
