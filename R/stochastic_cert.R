##' Montagu generates a certificate when a group uploads their stochastic
##' parameter set. Stoner can check the certificate applies to the right
##' touchstone and modelling group.
##'
##' @export
##' @title Validate stochastic parameter certificate
##' @param con DBI connection to production. Used for verifying certificate
##' against expected properties
##' @param certfile Name of the certificate file to be verified.
##' @param modelling_group The expected modelling group. We expect that the
##' owner of this certificate logged in as this modelling_group, when
##' uploading their stochastic parameter set.
##' @param touchstone The expected touchstone. We expect that the owner of
##' the provided certificate uploaded a stochastic parameter set to this
##' touchstone in order to get it.
##' @param disease The expected disease. We expect that the certificate
##' provided was uploaded into Montagu with reference to this disease.

stone_stochastic_cert_verify <- function(con, certfile, modelling_group,
                                         touchstone, disease) {
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

  assert_scalar_character(disease)
  if (!db_exists(con, "disease", "id", disease)) {
    stop(sprintf("Unknown disease: %s", disease))
  }

  if (is.null(certfile) || !file.exists(certfile)) {
    stop("Stochastic certificate not found")
  }

  cert <- jsonlite::fromJSON(read_file(certfile), simplifyVector = FALSE)
  params_id <- cert[[1]]$id
  mrps_info <- DBI::dbGetQuery(con,
    "SELECT DISTINCT modelling_group, responsibility_set.touchstone, disease
       FROM model_run_parameter_set
       JOIN responsibility_set
         ON responsibility_set.id = model_run_parameter_set.responsibility_set
       JOIN responsibility
         ON responsibility.responsibility_set = responsibility_set.id
       JOIN scenario
         ON scenario.id = responsibility.scenario
       JOIN scenario_description
         ON scenario.scenario_description = scenario_description.id
        WHERE model_run_parameter_set.id = $1",params_id)

  # A model_run_parameter_set links to a responsibility_set, not responsibility
  # hence, for groups with multiple diseases (JHU-Tam, LSHTM-Clark, LSHTM-Jit),
  # we may get multiple rows here. modelling_group and touchstone will be the
  # same for all rows, but disease may not be. So the best we can do is verify
  # that the disease is among the expected diseases for the responsibility_set.

  if (all(mrps_info$modelling_group != modelling_group)) {
    stop(sprintf("Modelling group mismatch - expected %s",
                 mrps_info$modelling_group))
  }

  if (!disease %in% mrps_info$disease) {
    stop(sprintf("Disease %s not found in %s", disease,
                 paste(mrps_info$disease, collapse = ", ")))
  }

  if (all(mrps_info$touchstone != touchstone)) {
    stop(sprintf("Touchstone mismatch - expected %s"
                 ,mrps_info$touchstone))
  }

  invisible()
}
