##' To save space on production, we could delete burden estimate sets
##' which are not a modelling_group's current estimate set for a
##' particular touchstone and scenario


###############################################################################
# Quick helper to produce a set of WHERE x IN (a,b) <AND> clauses for
# whichever selection of results we might consider for pruning opportunities.

get_filters <- function(touchstone, modelling_group, disease, scenario) {

  filters <- c(
    if (!is.null(touchstone)) paste0(" scenario.touchstone IN ", sql_in(touchstone)) else NULL,
    if (!is.null(modelling_group)) paste0(" modelling_group IN ", sql_in(modelling_group)) else NULL,
    if (!is.null(disease)) paste0(" disease IN ", sql_in(disease)) else NULL,
    if (!is.null(scenario)) paste0(" scenario_description IN ", sql_in(scenario)) else NULL
  )

  if (length(filters) > 0) {
    paste0(" WHERE ", paste(filters, collapse = " AND "))
  } else {
    ""
  }

}


##' @export
##' @title Prune obselete datasets
##' @param con DBI connection to database containing burden estimate sets
##' @param modelling_group Only prune specific modelling group(s).
##' @param disease Only prune specific disease(s)
##' @param touchstone Only prune specific touchstone(s)
##' @param scenario Only prune specific scenario description(s).
##' @param dry_run If true, only report details on what rows would be dropped.
stone_prune <- function(con, modelling_group = NULL,  disease = NULL,
                        touchstone = NULL, scenario = NULL, dry_run = TRUE) {

  assert_connection(con)

  if (!is.null(modelling_group)) {
    assert_character(modelling_group)
  }

  if (!is.null(disease)) {
    assert_character(disease)
  }

  if (!is.null(touchstone)) {
    assert_character(touchstone)
  }

  if (!is.null(scenario)) {
    assert_character(scenario)
  }


  ############################################################
  # Get responsibility info for all the
  # Optionally filter to various things:-

  resp_info <- DBI::dbGetQuery(con, sprintf("
    SELECT modelling_group, disease,
           scenario.touchstone as touchstone,
           scenario.scenario_description as scenario,
           responsibility.id as responsibility,
           current_burden_estimate_set
      FROM responsibility
      JOIN responsibility_set
        ON responsibility_set.id = responsibility.responsibility_set
      JOIN scenario
        ON responsibility.scenario = scenario.id
      JOIN scenario_description
        ON scenario_description.id = scenario.scenario_description
        %s", get_filters(touchstone, modelling_group, disease, scenario)))


  #################################################################
  # Find all the burden estimate sets for the above
  # responsibilities, that aren't the current_burden_estimate_set

  bes_info <- DBI::dbGetQuery(con, sprintf("
    SELECT id AS burden_estimate_set,
           responsibility
      FROM burden_estimate_set
     WHERE responsibility IN %s
       AND NOT id IN %s",
    sql_in(resp_info$responsibility),
    sql_in(resp_info$current_burden_estimate_set)))

  bes_info <- dplyr::left_join(bes_info, resp_info, by = "responsibility")
  bes_info <- bes_info[order(bes_info$touchstone, bes_info$modelling_group,
                             bes_info$disease, bes_info$scenario,
                             bes_info$burden_estimate_set), ]

  bes_sql_list <- sql_in(bes_info$burden_estimate_set)

  #################################################################
  # If a dry-run, then just output what we we're planning to do
  # with some info.

  if (dry_run) {

    # Look up how many rows we'll delete per defunct estimate set
    # Note that there may be entries in bes_sql_list that have no
    # rows in burden_estimate, so we'll need to do these zeroes
    # manually.

    del_rows_info <- DBI::dbGetQuery(con, sprintf("
      SELECT burden_estimate_set, COUNT(*) AS rows
        FROM burden_estimate
       WHERE burden_estimate_set IN %s
    GROUP BY burden_estimate_set", bes_sql_list))

    bes_info <- dplyr::left_join(bes_info, del_rows_info,
                                 by = "burden_estimate_set")

    bes_info$rows[is.na(bes_info$rows)] <- 0

    bes_info <- dplyr::rename(bes_info,
      keep_bes = current_burden_estimate_set,
      del_bes = burden_estimate_set,
      group = modelling_group)

    bes_info <- bes_info[, c("touchstone", "group", "scenario", "keep_bes", "del_bes", "rows")]

    message(paste0(capture.output(bes_info), collapse = "\n"))

    cat("\nTotal rows to delete: ", sum(bes_info$rows))

    return()
  }

  #################################################################
  # Otherwise, delete possibly many rows from burden_estimate, and
  # a small number of rows from burden_estimate_set

  if (nrow(bes_info) > 0) {

    DBI::dbExecute(con, sprintf("
       DELETE FROM burden_estimate
        WHERE burden_estimate_set IN ", bes_sql_list))

    DBI::dbExecute(con, sprintf("
      DELETE FROM burden_estimate_set
       WHERE id IN ", bes_sql_list))

    DBI::dbExecute(con, "VACUUM burden_estimate")

  }
}
