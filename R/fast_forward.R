###############################################################################
# FAST FORWARD
#
# Sort out the incoming CSV. Going to assume Fastforwarding is a standalone
# action - no other CSVs involved. (Otherwise it would get nasty with
# potentially adding responsibilities or repsonsibility_sets in
# different places within the same stoner import.
#
# Take a csv that may have * or ; in its
# modelling_group or scenario fields, and
# expand to singles per row.
#
# Pull this out so we can test it more easily.
#
# All the work really happens in expand_ff_csv to sort out what we want
# to do and what we need to know to do it...

expand_ff_csv <- function(csv, con) {

  if (!identical(sort(names(csv)),
                       sort(c("modelling_group", "scenario",
                              "touchstone_from", "touchstone_to")))) {
    stop("Incorrect columns in fast_forward.csv")
  }

  missing_things <- function(items, table, con, id_field = "id") {
    items <- unique(items)
    db_things <- DBI::dbGetQuery(con, sprintf(
      "SELECT %s FROM %s", id_field, table))[[id_field]]
    items[!items %in% db_things]
  }

  expand_semicolons <- function(csv, field) {
    new_csv <- NULL
    for (i in seq_len(nrow(csv))) {
      row <- csv[i, ]
      if (!grepl(";", row[[field]])) {
        new_csv <- rbind(new_csv, row)
      } else {
        items <- strsplit(row[[field]], ";")[[1]]
        for (item in items) {
          row[[field]] <- item
          new_csv <- rbind(new_csv, row)
        }
      }
    }
    new_csv
  }

  csv <- expand_semicolons(csv, "modelling_group")
  csv <- expand_semicolons(csv, "scenario")

  # Expand modelling group wildcard (*)

  while (any(csv$modelling_group == "*")) {
    i <- which(csv$modelling_group == "*")[1]
    row <- csv[i, ]
    mgs <- DBI::dbGetQuery(con, "
      SELECT DISTINCT modelling_group
        FROM responsibility_set
       WHERE touchstone = $1", csv$touchstone_from[i])$modelling_group
    csv <- csv[-i, ]
    for (mg in mgs) {
      row$modelling_group <- mg
      csv <- rbind(csv, row)
    }
  }

  # Test required modelling groups exist

  mgs <- missing_things(unique(csv$modelling_group), "modelling_group", con)
  if (length(mgs) > 0) {
    stop(sprintf("Modelling group(s) not found: %s",
                 paste(mgs, collapse = ", ")))
  }

  # Expand scenario wildcard (*)

  while (any(csv$scenario == "*")) {
    i <- which(csv$scenario == "*")[1]
    row <- csv[i, ]
    scenarios <- DBI::dbGetQuery(con, "
      SELECT DISTINCT scenario_description
        FROM scenario
        JOIN responsibility
          ON scenario.id = responsibility.scenario
        JOIN responsibility_set
          ON responsibility.responsibility_set = responsibility_set.id
       WHERE responsibility_set.modelling_group = $1
         AND scenario.touchstone = $2",
    list(csv$modelling_group[i], csv$touchstone_from[i]))$scenario_description
    csv <- csv[-i, ]
    for (scenario in scenarios) {
      row$scenario <- scenario
      csv <- rbind(csv, row)
    }
  }


  # Test required scenarios exist

  scs <- missing_things(unique(csv$scenario), "scenario", con,
                        "scenario_description")
  if (length(scs) > 0) {
    stop(sprintf("Scenario(s) not found: %s",
                    paste(scs, collapse = ", ")))
  }

  # We'll settle for that, without rejecting particular
  # {group,scenario}, to allow easier specification of
  # "these groups, these scenarios" on one line.

  # We'll just remove any invalid combos. Build a hash of
  # touchstone_from \r modelling_group \r scenario where a
  # burden_estimate_set exists for that combination.

  db_mash <- function(touchstones) {
    DBI::dbGetQuery(con, sprintf("
      SELECT CONCAT(modelling_group, '\r',
                    responsibility_set.touchstone, '\r',
                    scenario_description) as mash,
             responsibility.scenario as scid,
             responsibility_set.id as rset,
             responsibility.id as resp,
             current_burden_estimate_set as bes,
             current_stochastic_burden_estimate_set as sbes,
             is_open, expectations
        FROM responsibility_set
        JOIN responsibility
          ON responsibility_set.id = responsibility.responsibility_set
        JOIN scenario
          ON responsibility.scenario = scenario.id
       WHERE current_burden_estimate_set IS NOT NULL
         AND responsibility_set.touchstone IN %s", touchstones))
  }

  all_touchstone_from <-
    paste0("('", paste(unique(csv$touchstone_from), collapse = "','"), "')")

  db_mash1 <- db_mash(all_touchstone_from)

  csv$mash <- paste(csv$modelling_group, csv$touchstone_from, csv$scenario,
                    sep = "\r")
  csv <- csv[csv$mash %in% db_mash1$mash, ]
  matches <- match(csv$mash, db_mash1$mash)
  csv$rset <- db_mash1$rset[matches]
  csv$resp <- db_mash1$resp[matches]
  csv$bes <- db_mash1$bes[matches]
  csv$scid <- db_mash1$scid[matches]
  csv$sbes <- db_mash1$sbes[matches]
  csv$is_open <- db_mash1$is_open[matches]
  csv$expectations <- db_mash1$expectations[matches]

  # Like Columbo, just one more thing. Don't fast-forward if there is
  # already a burden estimate for that group/scenario in the destination
  # touchstone. So, look up and mash all
  # modelling_group \r touchstone_to \r scenario

  all_touchstone_to <-
    paste0("('", paste(unique(csv$touchstone_to), collapse = "','"), "')")

  db_mash2 <- db_mash(all_touchstone_to)

  # And remove any matches, reporting.

  csv$mash <- paste(csv$modelling_group, csv$touchstone_to, csv$scenario,
                    sep = "\r")
  ignoring <- csv[csv$mash %in% db_mash2$mash, ]
  csv <- csv[!csv$mash %in% db_mash2$mash, ]

  if (nrow(ignoring) > 0) {
    message("Estimates found in target touchstone for: ")
    for (i in seq_len(nrow(ignoring))) {
      message(paste(ignoring$touchstone_to, ignoring$modelling_group,
                    ignoring$scenario, sep = " - "))
    }
  }

  # Actually, there's two more things. We may need to create more
  # responsibility_sets if they don't exist in the new touchstone.
  # So we'll look up the ids if those responsibility_sets exist.
  # And the same for responsibilities afterwards.

  next_rsets <- DBI::dbGetQuery(con, sprintf("
    SELECT id, CONCAT(modelling_group, '\r', touchstone) AS mash
      FROM responsibility_set
     WHERE touchstone IN %s", all_touchstone_to))

  if (nrow(csv) > 0) {

    csv$mash <- paste(csv$modelling_group, csv$touchstone_to, sep = "\r")
    csv$rset_to <- NA
    csv$rset_to <- next_rsets$id[match(csv$mash, next_rsets$mash)]

    next_resps <- DBI::dbGetQuery(con, sprintf("
      SELECT responsibility.id as id,
             CONCAT(responsibility.id, '\r', scenario_description) AS mash
        FROM responsibility
        JOIN scenario
          ON responsibility.scenario = scenario.id
       WHERE scenario.touchstone IN %s", all_touchstone_to))

    csv$mash <- paste(csv$resp, csv$scenario, sep = "\r")
    csv$resp_to <- NA
    csv$rset_to <- next_resps$id[match(csv$mash, next_resps$mash)]

    csv$mash <- NULL
  }
  unique(csv)
}

###############################################################################
# EXTRACT FAST FORWARD

extract_fast_forward <- function(e, path, con) {

  # The fast_forward.csv file indicates...
  # modelling_groups, scenarios, touchstone_from, touchstone_to
  #
  # where: modelling_group and responsibilities can be wildcard (*),
  # or a semi-colon separated list of options.
  #
  # We want to extract info on responsibility_sets and responsibilities
  # for these. Also, we want to lookup all comments associated with
  # the responsibilities and responsibility_sets, as we may want to
  # replicate them (with perhaps an edit to say that we've fast-forwarded)
  #
  # It appears the interface only shows the most recent comment.

  # No CSV provided
  if (is.null(e$fast_forward_csv)) {
    return(NULL)
  }

  # Empty CSV provided
  if (nrow(e$fast_forward_csv) == 0) {
    return(NULL)
  }

  # Test all required touchstones exist before we continue

  ts <- unique(c(e$fast_forward_csv$touchstone_from,
                 e$fast_forward_csv$touchstone_to))
  db_ts <- DBI::dbGetQuery(con, "SELECT id FROM touchstone")
  ts <- ts[!ts %in% db_ts$id]
  if (length(ts) > 0) {
    stop(sprintf("Required touchstone(s) not found: %s",
                    paste(ts, collapse = ", ")))
  }

  csv <- expand_ff_csv(e$fast_forward_csv, con)

  # CSV provided, but turns out there is no work to do.
  if (nrow(csv) == 0) {
    return(NULL)
  }

  # Fetch the responsibility_set_comment and responsibility_comment

  rset_comm <- DBI::dbGetQuery(con, sprintf("
    SELECT *
      FROM responsibility_set_comment
     WHERE responsibility_set IN (%s)",
    paste(unique(csv$rset), collapse = ",")))

  # And the responsibility comments

  resp_comm <- rbind(DBI::dbGetQuery(con, sprintf("
      SELECT *
        FROM responsibility_comment
       WHERE responsibility IN (%s)",
      paste(unique(csv$resp), collapse = ","))))

  # We only want to keep the most recent comment for each
  # responsibility or responsibility_set. We'll do this in
  # the inner loop while the data frame is shorter

  keep_latest <- function(d, id_field) {
    ids <- unique(d[[id_field]])
    for (id in ids) {
      max_date <- max(d$added_on[d[[id_field]] == id])
      d <- d[d[[id_field]] != id | d$added_on == max_date, ]
    }
    d
  }
  resp_comm <- keep_latest(resp_comm, "responsibility")
  rset_comm <- keep_latest(rset_comm, "responsibility_set")

  list(ff_info = csv,
       resp_comments = resp_comm,
       rset_comments = rset_comm)
}

###############################################################################
# TEST EXTRACT FAST FORWARD

test_extract_fast_forward <- function(e) {

  if (!is.null(e$ff_info)) {
    testthat::expect_true(!is.null(e$ff_info))
    testthat::expect_true(!is.null(e$resp_comments))
    testthat::expect_true(!is.null(e$rset_comments))

    testthat::expect_equal(sort(names(e$ff_info)),
                           sort(c("modelling_group", "scenario",
                                  "touchstone_from", "touchstone_to",
                                  "resp", "rset", "bes", "rset_to",
                                  "resp_to", "sbes", "is_open",
                                  "expectations", "scid")),
                           label = "Correct columns in expanded ff csv")
  }

}

###############################################################################
# TRANSFORM FAST FORWARD

transform_fast_forward <- function(e) {
  t <- list()
  ff <- e$ff_info

  # No work to do:

  if (is.null(ff)) {
    return(NULL)
  }

  # responsibility_sets table.
  # Create new rows (with dummy ids for now) for each ff_info where
  # rset_to = NA

  # Do some work only on the ff_info that has rset_to = NA.

  ff_non_na_rset_to <- ff[!is.na(ff$rset_to), ]
  ff_na_rset_to <- ff[is.na(ff$rset_to), ]

  rsets <- unique(ff_na_rset_to[, c("rset", "modelling_group", "touchstone_to")])
  rsets$status <- "incomplete"
  names(rsets)[names(rsets) == "touchstone_to"] <- "touchstone"
  rsets$id <- seq(-1, by = -1, length.out = nrow(rsets))

  # Replace NAs in e$ff_info with the dummy ones

  rsets$mash <- paste(rsets$modelling_group, rsets$touchstone, sep = "\r")
  ff_na_rset_to$mash <- paste(ff_na_rset_to$modelling_group,
                              ff_na_rset_to$touchstone_to, sep = "\r")

  ff_na_rset_to$rset_to <- rsets$id[match(ff_na_rset_to$mash, rsets$mash)]

  rsets$mash <- NULL
  ff_na_rset_to$mash <- NULL

  # Update comments for responsibility_set

  update_comments <- function(d, ff, ff_field, comm_field) {
    d$id <- seq(-1, by = -1, length.out = nrow(d))
    d$comment <- unlist(lapply(seq_len(nrow(d)), function(x) {
      x <- d[x, ]
      touchstone <-
        unique(ff$touchstone_from[ff[[ff_field]] == x[[comm_field]]])
      paste0(x$comment, " - Fast-forwarded from ", touchstone)
    }))
    d
  }

  # Set responsibility_set_comment.responsibility_set to the new
  # (maybe negative) id for the responsibility_set.

  t[['responsibility_set_comment']] <-
    update_comments(e$rset_comments, ff, "rset", "responsibility_set")

  t[['responsibility_set_comment']]$responsibility_set <-
    rsets$id[match(t[['responsibility_set_comment']]$responsibility_set,
                   rsets$rset)]

  rsets$rset <- NULL

  t[['responsibility_set']] <- rsets

  ff <- rbind(ff_non_na_rset_to, ff_na_rset_to)

  # Create new responsibilites (will pick up the dummy ids created above)

  resps <- unique(ff[is.na(ff$resp_to),
                      c("resp", "rset_to", "scid", "bes", "sbes",
                        "is_open", "expectations")])

  rename_resps <- function(resps) {
    names(resps)[names(resps) == "rset_to"] <- "responsibility_set"
    names(resps)[names(resps) == "rset"] <- "responsibility_set"
    names(resps)[names(resps) == "resp_to"] <- "id"
    names(resps)[names(resps) == "resp"] <- "id"
    names(resps)[names(resps) == "scid"] <- "scenario"
    names(resps)[names(resps) == "bes"] <- "current_burden_estimate_set"
    names(resps)[names(resps) == "sbes"] <- "current_stochastic_burden_estimate_set"
    resps
  }

  resps <- rename_resps(resps)
  resps$newid <- seq(-1, by = -1, length.out = nrow(resps))

  # Update responsibility comments

  t[['responsibility_comment']] <-
    update_comments(e$resp_comments, ff, "resp", "responsibility")


  t[['responsibility_comment']]$responsibility  <-
    resps$newid[match(t[['responsibility_comment']]$responsibility, resps$id)]


  resps$id <- resps$newid
  resps$newid <- NULL

  # Add the update for responsibilities that already exist.

  resps_existing <- unique(ff[!is.na(ff$resp_to),
                              c("rset_to", "scid", "bes", "sbes",
                                "is_open", "expectations", "resp_to")])

  resps_existing <- rename_resps(resps_existing)

  # And remove the migrated estimate sets from the old touchstone

  resps_remove_bes <- ff[, c("rset", "scid",
                      "is_open", "expectations", "resp")]
  resps_remove_bes$sbes <- NA
  resps_remove_bes$bes <- NA
  resps_remove_bes <- rename_resps(resps_remove_bes)

  t[['responsibility']] <- rbind(resps, resps_existing, resps_remove_bes)


  t
}


test_transform_fast_forward <- function(transformed_data) {
  # Not much we can test here, as tables could
  # contain all or nothing.
}

###############################################################################

load_fast_forward <- function(transformed_data, con) {
  t <- transformed_data

  # So... 4 tables to work on. Shorten names to make this code easier.

  rs <- t[['responsibility_set']]
  r <- t[['responsibility']]

  # No work to do?

  if (is.null(r)) {
    return(NULL)
  }

  rsc <- t[['responsibility_set_comment']]
  rc <- t[['responsibility_comment']]

  # responsibility_set - add the new rows (which have negative ids)

  DBI::dbAppendTable(con, "responsibility_set",
                     rs[, c("modelling_group", "touchstone", "status")])

  #  - fetch mapping from old to new ids

  rs$mash <- paste(rs$modelling_group, rs$touchstone, sep = "\r")

  new_ids <- DBI::dbGetQuery(con, sprintf("
    SELECT id, CONCAT(modelling_group, '\r', touchstone) AS mash
      FROM responsibility_set
     WHERE CONCAT(modelling_group, '\r', touchstone) IN ('%s')",
       paste(rs$mash, collapse = "','")))

  rs$newid <- new_ids$id[match(rs$mash, new_ids$mash)]

  #  - Update negative responsibility_set in the responsibility table

  rneg <- r[r[['responsibility_set']] < 0, ]
  rpos <- r[r[['responsibility_set']] >= 0, ]
  rneg[['responsibility_set']] <-
    rs$newid[match(rneg[['responsibility_set']], rs$id)]

  # responsibility_set_comment with the new serial ids

  rsc[['responsibility_set']] <-
    rs$newid[match(rsc[['responsibility_set']], rs$id)]

  r <- rbind(rneg, rpos)

  # responsibility     - For the ones with negative ids,
  #                    - add, remembering the mapping again...

  rneg <- r[r$id < 0, ]
  rneg_no_id <- rneg
  rneg_no_id$id <- NULL
  DBI::dbAppendTable(con, "responsibility", rneg_no_id)
  rneg$mash <- paste(rneg[['responsibility_set']], rneg$scenario, sep = "\r")

  # Fetch the new ids mapping...

  new_ids <- DBI::dbGetQuery(con, sprintf("
    SELECT id, CONCAT(responsibility_set, '\r', scenario) AS mash
      FROM responsibility
     WHERE CONCAT(responsibility_set, '\r', scenario) IN ('%s')",
      paste(rneg$mash, collapse = "','")))

  rneg$newid <- new_ids$id[match(rneg$mash, new_ids$mash)]

  # And apply to responsibility_comment

  rc[['responsibility']] <-
    rneg$newid[match(rc[['responsibility']], rneg$id)]

  # - For responsibilities that already had ids,
  # - Update current_burden_estimate_set.

  for (i in seq_len(nrow(rpos))) {
    row <- rpos[i, ]
    DBI::dbExecute(con, "
      UPDATE responsibility
         SET current_burden_estimate_set = $1
       WHERE id = $2", list(row$current_burden_estimate_set, row$id))
  }

  # responsibility_set_comment and responsibility_comment
  # are now ready to go. These are just additions, so id can
  # be left as serial.

  rsc$id <- NULL
  rc$id <- NULL

  DBI::dbAppendTable(con, "responsibility_set_comment", rsc)
  DBI::dbAppendTable(con, "responsibility_comment", rc)
}
