##' Montagu generates a certificate when a group uploads their stochastic
##' parameter set. Stoner can check the certificate applies to the right
##' touchstone and modelling group.
##'
##' @export
##' @title Validate stochastic parameter certificate
##' @import jsonlite
##' @param con DBI connection to production. Used for verifying certificate
##' against expected properties
##' @param certfile Name of the certificate file accompanying the estimates
##' @param modelling_group The modelling group who provided the certificate
##' @param touchstone The touchstone the group reports for this certificate.

stone_stochastic_cert_verify <- function(con, certfile, modelling_group,
                                         touchstone) {

  read_file <- function(path) {
    rawToChar(readBin(path, raw(), file.size(path)))
  }

  assert_scalar_character(modelling_group)
  if (!db_exists(con, "modelling_group", "id", modelling_group)) {
    stop(sprintf("Unknown modelling group: %s", modelling_group))
  }

  assert_scalar_character(touchstone)
  if (!db_exists(con, "touchstone", "id", touchstone)) {
    stop(sprintf("Unknown touchstone: %s", touchstone))
  }

  if (is.null(certfile) || !file.exists(certfile)) {
    stop("Stochastic certificate not found")
  }

  cert <- jsonlite::fromJSON(read_file(certfile), simplifyVector = FALSE)
  params_id <- cert[[1]]$id
  mrps_info <- DBI::dbGetQuery(con,
    "SELECT modelling_group, touchstone
       FROM model_run_parameter_set
       JOIN responsibility_set
         ON responsibility_set.id = model_run_parameter_set.responsibility_set
        WHERE model_run_parameter_set.id = $1",params_id)

  if (mrps_info$modelling_group != modelling_group) {
    stop(sprintf("Modelling group mismatch - expected %s, ",
                 mrps_info$modelling_group))
  }

  if (mrps_info$touchstone != touchstone) {
    stop(sprintf("Touchstone mismatch - expected %s"
                 ,mrps_info$touchstone))
  }

  invisible()
}
