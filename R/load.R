#' Load touchstone-relevant transformed data into the database. This should be
#' called from the load code of a montagu-import, which must therefore be a
#' custom load, not an automatic one. Call `stoner::stone_load(transformed_data, con)`
#' with the same arguments provided to the montagu-import load function.
#' @title Load touchstone data to database
#' @export
#' @param transformed_data The transformed data from the transform stage. In a
#' form which adheres to the DB schema.
#' @param con Active connection to the DB which the data will be loaded to.
#' @param allow_overwrite_scenario_description If TRUE, then stoner will overwrite
#' rows in the scenario_description table with rows from the CSV file, (matching id,
#' different description or disease), even if there are open or finished touchstones
#' that use this description. See the advanced section of the vignette for discussion
#' on why this may be necessary.
#'

stone_load <- function(transformed_data, con,
                       allow_overwrite_scenario_description = FALSE) {

  load_touchstone_name(transformed_data, con)
  load_touchstone(transformed_data, con)
  load_scenario_description(transformed_data, con,
                            allow_overwrite_scenario_description)
  load_touchstone_demographic_dataset(transformed_data, con)
  load_touchstone_countries(transformed_data, con)

  check_faulty_serials(con)

}
