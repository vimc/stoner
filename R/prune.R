##' @importFrom utils capture.output

extract_prune <- function(e, path, con) {

  # The prune.csv file indicates...
  # modelling_group, disease, scenario, and touchstones

  # these can be '*' for wildcard,
  # or a semi-colon separated list of options.

  # No CSV provided
  if (is.null(e$prune_csv)) {
    return(NULL)
  }

  # Check columns
  if (!identical(sort(names(e$prune_csv)),
                 c("disease", "modelling_group", "scenario", "touchstone"))) {
    stop("Columns should be disease, modelling_group, scenario, touchstone")
  }

  # Empty CSV provided
  if (nrow(e$prune_csv) == 0) {
    return(NULL)
  }

  # Check all necessary things exist, and
  # return vector of things.

  thing_exists <- function(entries, table) {
    entries <- unique(unlist(lapply(entries, split_semi)))
    if ("*" %in% entries) {
      return(NULL)
    }
    db_entries <- DBI::dbGetQuery(con, sprintf("
      SELECT * FROM %s WHERE id IN ('%s')", table,
        paste(entries, collapse="','")))$id
    if (any(!entries %in% db_entries)) {
      entries <- paste(entries[!entries %in% db_entries],
                       collapse = ", ")
      stop(sprintf("%s not found: %s\n", table, entries))
    }
    db_entries
  }

  modelling_group <- thing_exists(e$prune_csv$modelling_group, "modelling_group")
  disease <- thing_exists(e$prune_csv$disease, "disease")
  touchstone <- thing_exists(e$prune_csv$scenario, "scenario_description")
  scenario <- thing_exists(e$prune_csv$touchstone, "touchstone")


  # For each row in the CSV file:

  burden_estimate_set <- NULL

  for (i in seq_len(nrow(e$prune_csv))) {
    row <- e$prune_csv[i, ]
    touchstone <- split_semi(row$touchstone)
    modelling_group <- split_semi(row$modelling_group)
    disease < split_semi(row$disease)
    scenario <- split_semi(row$scenario)

    touchstone <- if ("*" %in% touchstone) NULL else touchstone
    modelling_group <- if ("*" %in% modelling_group) NULL else modelling_group
    disease <- if ("*" %in% disease) NULL else disease
    scenario <- if ("*" %in% scenario) NULL else scenario

    # Find all the responsibilities for any of these fields, including
    # the current_burden_estimate_set

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

    # Ignore where no burden_estimate_set has been uploaded.

    resp_info <- resp_info[!is.na(resp_info$current_burden_estimate_set), ]

    #################################################################
    # Find all the burden estimate sets for the above
    # responsibilities, that aren't the current_burden_estimate_set

    burden_estimate_set <- rbind(burden_estimate_set,
      DBI::dbGetQuery(con, sprintf("
        SELECT *
          FROM burden_estimate_set
         WHERE responsibility IN %s
           AND NOT id IN %s",
        sql_in(c(-1, resp_info$responsibility)),
        sql_in(c(-1, resp_info$current_burden_estimate_set)))))
  }

  # We now have a list of burden estimate sets to cull -
  # Remove any duplicates, and return

  burden_estimate_set <- burden_estimate_set[!duplicated(burden_estimate_set$id), ]
  list(burden_estimate_set = burden_estimate_set)
}

test_extract_prune <- function(e) {

}

transform_prune <- function(extracted_data) {
  if ("burden_estimate_set" %in% names(extracted_data)) {
    list(burden_estimate_set = extracted_data$burden_estimate_set)
  } else {
    NULL
  }
}

test_transform_prune <- function(t) {

}

load_prune <- function(transformed_data, con) {
  bes_to_remove <- unique(transformed_data$burden_estimate_set$id)

  if (length(bes_to_remove) > 0) {

    bes_sql_list <- sql_in(bes_to_remove)

    info <- DBI::dbGetQuery(con, sprintf("
      SELECT modelling_group, responsibility_set.touchstone as touchstone,
             scenario_description, burden_estimate_set.id as id FROM burden_estimate_set
        JOIN responsibility
          ON burden_estimate_set.responsibility = responsibility.id
        JOIN responsibility_set
          ON responsibility_set.id = responsibility.responsibility_set
        JOIN scenario
          ON scenario.id = responsibility.scenario
       WHERE burden_estimate_set.id IN %s", bes_sql_list))

    for (i in seq_len(nrow(info))) {
      cat(sprintf("Pruning burden estimate set %d : %s, %s, %s\n",
                  info$id[i], info$modelling_group[i],
                  info$touchstone[i], info$scenario_description[i]))
    }

    DBI::dbExecute(con, sprintf("
       DELETE FROM burden_estimate
        WHERE burden_estimate_set IN %s", bes_sql_list))

    DBI::dbExecute(con, sprintf("
      DELETE FROM burden_estimate_set
       WHERE id IN %s", bes_sql_list))

    DBI::dbExecute(con, "VACUUM burden_estimate")
  }
}

###############################################################################
# Quick helper to produce a set of WHERE x IN (a,b) <AND> clauses for
# whichever selection of results we might consider for pruning opportunities.

get_filters <- function(touchstone, modelling_group, disease, scenario) {

  filters <- c(
    if (!is.null(touchstone)) paste0("scenario.touchstone IN ", sql_in(touchstone)) else NULL,
    if (!is.null(modelling_group)) paste0("modelling_group IN ", sql_in(modelling_group)) else NULL,
    if (!is.null(disease)) paste0("disease IN ", sql_in(disease)) else NULL,
    if (!is.null(scenario)) paste0("scenario_description IN ", sql_in(scenario)) else NULL
  )

  if (length(filters) > 0) {
    paste0("WHERE ", paste(filters, collapse = " AND "))
  } else {
    ""
  }
}
