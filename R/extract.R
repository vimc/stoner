##' Extract touchstone-relevant data from sources. This should be called with
##' \code{stoner::extract(path, con}, from within the extract code of a
##' montagu-import, providing the same arguments provided to the montagu-import
##' extract function.
##'
##' See the vignette for information about the specifics of the extraction.
##'
##' @export
##' @title Extract touchstone-relevant data
##' @param path Path to the import project root used for finding any local data.
##' @param con The active DBI connection for extracting any data.
##' @return A list of named data frames and/or named values representing the extracted data.
extract <- function(path, con) {

  # First get just the meta CSVs

  e <- list(
      touchstone_csv = read_meta(path, "touchstone.csv"),
      touchstone_name_csv = read_meta(path, "touchstone_name.csv")
  )

  # Remove any NULLs

  e <- e[!unlist(lapply(e, is.null))]

  # Now we know what we need, extract from db

  e <- c(
    extract_touchstone(e, path, con)
  )

  e
}
