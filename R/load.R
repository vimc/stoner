#' Load touchstone-relevant transformed data into the database. This should be
#' called from the load code of a montagu-import, which must therefore be a
#' custom load, not an automatic one. Call `stoner::load(transformed_data, con)`
#' with the same arguments provided to the montagu-import load function.
#' @title Load touchstone data to database
#' @export
#' @param transformed_data The transformed data from the transform stage. In a
#' form which adheres to the DB schema.
#' @param con Active connection to the DB which the data will be loaded to.
#'

load <- function(transformed_data, con) {
  load_touchstone_name      (transformed_data, con)
  load_touchstone           (transformed_data, con)
  load_scenario_description (transformed_data, con)
}
