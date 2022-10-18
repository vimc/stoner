##' Extract touchstone-relevant data from sources. This should be called with
##' \code{stoner::stone_extract(path, con}, from within the extract code of a
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
stone_extract <- function(path, con) {

  # First get just the meta CSVs

  e <- list(
    touchstone_csv = read_meta(path, "touchstone.csv"),
    touchstone_name_csv = read_meta(path, "touchstone_name.csv"),
    scenario_type_csv = read_meta(path, "scenario_type.csv"),
    scenario_description_csv = read_meta(path, "scenario_description.csv"),
    touchstone_demographic_dataset_csv = read_meta(path, "touchstone_demographic_dataset.csv"),
    touchstone_countries_csv = read_meta(path, "touchstone_country.csv"),
    responsibilities_csv = extract_responsibilities_csv(path),
    fast_forward_csv = read_meta(path, "fast_forward.csv"),
    prune_csv = read_meta(path, "prune.csv")
  )

  # Remove any NULLs

  e <- e[!vlapply(e, is.null)]

  # If fast-forwarding, or pruning, then that must be the only CSV

  if (!is.null(e$fast_forward_csv)) {
    if (length(e) > 1) {
      stop("fast_forward.csv, if specified, must be the only csv")
    }
  }

  if (!is.null(e$prune_csv)) {
    if (length(e) > 1) {
      stop("prune.csv, if specified, must be the only csv")
    }
  }

  # Now we know what we need, extract from db

  c(
    extract_touchstone(e, path, con),
    extract_scenario_type(e, path, con),
    extract_scenario_description(e, path, con),
    extract_touchstone_demographic_dataset(e, path, con),
    extract_touchstone_country(e, path, con),
    extract_responsibilities(e, path, con),
    extract_fast_forward(e, path, con),
    extract_prune(e, path, con)
  )
}
