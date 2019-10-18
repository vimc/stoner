##' Run all generic tests on the provided extracted data. This should be
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
##' @title Test the extracted data.
##' @param extracted_data A list of \code{data.frame}s and/or values,

test_extract <- function(extracted_data) {


  supported_tables <- c("touchstone", "touchstone_csv",
                        "touchstone_name", "touchstone_name_csv")

  expect_true(all(names(extracted_data) %in% supported_tables),
              label = "All extracted data is supported by stoner")

  test_extract_touchstone(extracted_data)

}
