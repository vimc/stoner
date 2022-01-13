###############################################################################
# FAST FORWARD
#
# Sort out the incoming CSV. Going to assume Fastforwarding is a standalone
# action - no other CSVs involved. (Otherwise it would get nasty with
# potentially adding responsibilities or responsibility_sets in
# different places within the same stoner import.
#
# Take a csv that may have * or ; in its
# modelling_group or scenario fields, and
# expand to singles per row.
#
# Also, we'll need to extract information about other related
# responsibility_sets and responsibilities, to work out if we need
# to create new ones, or (in the case of responsibilities) edit
# existing ones.
#
# All the work really happens in expand_ff_csv to sort out what we want
# to do and what we need to know to do it...

expand_ff_csv <- function(csv, con) {

  # Check columns in CSV are sensible

  assert_set_equal(names(csv),
    c("modelling_group", "scenario", "touchstone_from", "touchstone_to"),
    "Incorrect columns in fast_forward.csv")

  # Also, risky to have transitive changes - eg,
  # migrate from t1 -> t2, and something else from t2 -> t3 in the
  # same import. This also covers errors where touchstone_from = touchstone_to

  if (any(csv$touchstone_from %in% csv$touchstone_to)) {
    stop("Same touchstone appears in both touchstone_to and touchstone_from.")
  }


  # Generic function to check that all "values" exist in
  # the "table" in the database, in column "id_field".
  # Values missing from the db table are returned

  missing_things <- function(values, table, con, id_field = "id") {
    values <- unique(values)
    db_things <- DBI::dbGetQuery(con, sprintf(
      "SELECT %s FROM %s", id_field, table))[[id_field]]
    values[!values %in% db_things]
  }

  # The modelling_group and scenario fields in the incoming csv
  # may be multi-line, with semi-colon separated elements in
  # any/all lines. Here, for a single field, expand so that
  # semi-colon-separated items become multiple table lines
  # with a single item in each.

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

  # This is a little clumsy, but if there is a modelling_group '*',
  # expand this so we have a line with a named modelling_group
  # for each that already has a
  # responsibility_set in the origin touchstone.

  while (any(csv$modelling_group == "*")) {
    i <- which(csv$modelling_group == "*")[1]
    row <- csv[i, ]
    mgs <- DBI::dbGetQuery(con, "
      SELECT DISTINCT modelling_group
        FROM responsibility_set
       WHERE touchstone = $1",
          csv$touchstone_from[i])$modelling_group

    csv <- csv[-i, ]
    for (mg in mgs) {
      row$modelling_group <- mg
      csv <- rbind(csv, row)
    }
  }

  # Having done all the expansion of modelling groups,
  # all items in that column should now exist.

  mgs <- missing_things(unique(csv$modelling_group), "modelling_group", con)
  if (length(mgs) > 0) {
    stop(sprintf("Modelling group(s) not found: %s",
                 paste(mgs, collapse = ", ")))
  }

  # We also allow wildcard for scenarios...
  # Here expand that now ito separate rows for individual scenarios,
  # that the (already single) modelling group has in
  # the origin touchstone.

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

  # Having expanded scenarios to one per line, check they
  # all exist - noting we are keeping these human readable,
  # so are really scenario.scenario_description

  scs <- missing_things(unique(csv$scenario), "scenario", con,
                        "scenario_description")
  if (length(scs) > 0) {
    stop(sprintf("Scenario(s) not found: %s",
                    paste(scs, collapse = ", ")))
  }

  # So now we have a list of jobs to do:-
  #
  # modelling_group, touchstone_from, touchstone_to, scenario

  # We'll remove any invalid combos. Build a hash of
  # touchstone_from \r modelling_group \r scenario and
  # touchstone_to \r modelling_group \r scenario to see
  # what exists in the database.

  # We'll drop anything in the CSV that doesn't exist in the origin
  # touchstone, as there's nothing to fast-forward. But let's not
  # make that an error, otherwise we'll get lots of false errors
  # if we expand (*, *)

  # burden_estimate_set exists for that combination,
  # and also note all the interesting ids.

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
             is_open, expectations,
             responsibility_set.touchstone as touchstone,
             modelling_group,
             scenario_description
        FROM responsibility_set
        JOIN responsibility
          ON responsibility_set.id = responsibility.responsibility_set
        JOIN scenario
          ON responsibility.scenario = scenario.id
       WHERE responsibility_set.touchstone IN %s", touchstones))
  }

  # Look up all the responsibilities we *might* fast-forward...

  all_touchstone_from <-
    paste0("('", paste(unique(csv$touchstone_from), collapse = "','"), "')")

  db_mash1 <- db_mash(all_touchstone_from)

  # Bind some columns onto our expanded CSV file, by matching on
  # (modelling_group \r touchstone_from \r scenario)

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

  # Now look up all the responsibilities which *might* already exist
  # in the destination touchstone - which we'll need to decide what
  # to do about.

  all_touchstone_to <-
    paste0("('", paste(unique(csv$touchstone_to), collapse = "','"), "')")

  db_mash2 <- db_mash(all_touchstone_to)

  # And see if they coincide with the destinations we want to fastforward into
  # (modelling_group \r touchstone_to \r scenario)

  csv$mash <- paste(csv$modelling_group, csv$touchstone_to, csv$scenario,
                    sep = "\r")

  # If burden_estimates are already uploaded, then we can't fast-forward
  # so we'll leave a message about it, but continue doing the others.

  already_bes <- csv[csv$mash %in% db_mash2$mash[!is.na(db_mash2$bes)], ]
  csv <- csv[!csv$mash %in% db_mash2$mash[!is.na(db_mash2$bes)], ]

  if (nrow(already_bes) > 0) {
    s <- ("Estimates found in target touchstone for: ")
    for (i in seq_len(nrow(already_bes))) {
      s <- paste0(s, paste(already_bes$touchstone_to, already_bes$modelling_group,
                    already_bes$scenario, sep = " - "))
    }
    message(s)
  }

  # Possibly we already have no work to do.

  if (nrow(csv) == 0) {
    return(csv)
  }

  # Additionally, there may be NA for the burden_estimate_set, but
  # already a responsibility, in which case we want to set
  # resp_to to point to that responsibility, rather than NA, which
  # will mean "create a new one".

  csv$resp_to <- NA

  for (i in seq_len(nrow(csv))) {
    match_db <- db_mash2[db_mash2$touchstone == csv$touchstone_to[i] &
                         db_mash2$modelling_group == csv$modelling_group[i] &
                         db_mash2$scenario_description == csv$scenario[i] &
                         is.na(db_mash2$bes), ]
    if (nrow(match_db) == 1) {
      csv$resp_to[i] <- match_db$resp
    }
  }



  # We also might need to create new responsibility_sets...

  next_rsets <- DBI::dbGetQuery(con, sprintf("
    SELECT id, CONCAT(modelling_group, '\r', touchstone) AS mash
      FROM responsibility_set
     WHERE touchstone IN %s", all_touchstone_to))
  csv$rset_to <- NA
  if (nrow(next_rsets) > 0) {
    csv$mash <- paste(csv$modelling_group, csv$touchstone_to, sep = "\r")
    csv$rset_to <- next_rsets$id[match(csv$mash, next_rsets$mash)]
  }
  csv$mash <- NULL
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
#
# So - this is what we expect to be coming in from the extract...
#
# ff_info:   modelling_group      eg   Group-Name
#            scenario             eg   mena-booster-default
#            touchstone_from      eg   202110gavi-2
#            touchstone_to        eg   202110gavi-3
#            rset                 eg   464 - id of resp_set in touchstone_from
#            resp                 eg   3208 - id of resp in touchstone_from
#            bes                  eg   NA (id of existing burden estimate set)
#            scid                 eg   1993 id of scenario
#            sbes                 eg   NA - stochastic burden estimate set id
#            is_open              eg   FALSE
#            expectations         eg   391
#            resp_to              eg   3335 - responsibility in touchstone_to
#            rset_to              eg   484  - responsibility_set in touchstone_to
#
#
# resp_comments :    id, responsibility,     comment, added_by, added_on
# rset_comments :    id, responsnbility_set, comment, added_by, added_on


transform_fast_forward <- function(e) {
  t <- list()
  ff <- e$ff_info

  # If there's no work to do, exit early...

  if (is.null(ff)) {
    return(NULL)
  }

  # Helper for updating comments for responsibility_set / responsibility
  # d is the incoming table (either responsibility/responsibility_set)
  # ff is the fastforward info, containing "touchstone_from".
  # ff_field is the field in ff ("rset" or "resp")
  # comm_field is the field in the db table ("responsibility_set" or "responsibility")

  # We'll return a new copy of d, where each line has a negative id
  # (to be replaced later when adding the table), and each
  # comment has a reference to the pre-ff touchstone

  update_comments <- function(d, ff, ff_field, comm_field) {

    # Set ids to be negative - this will get updated later when
    # the rows are added.

    d$id <- seq(-1, by = -1, length.out = nrow(d))

    # Update each comment in the new table, appending with
    # "- Fast-forwarded from (old touchstone)"

    d$comment <- unlist(lapply(seq_len(nrow(d)), function(x) {
      x <- d[x, ]
      touchstone <-
        unique(ff$touchstone_from[ff[[ff_field]] == x[[comm_field]]])
      paste0(x$comment, " - Fast-forwarded from ", touchstone)
    }))
    d
  }


  # responsibility_sets table.
  # Create new rows (with dummy ids for now) for each ff_info where
  # rset_to = NA

  # Do some work only on the ff_info that has rset_to = NA.

  # Entries for which destination responsibility_set already exists:

  ff_non_na_rset_to <- ff[!is.na(ff$rset_to), ]

  # Entries for which destination responsibility_set does not already exist

  ff_na_rset_to <- ff[is.na(ff$rset_to), ]

  # Responsibility_sets that we'll need to create are:-

  new_rsets <- unique(ff_na_rset_to[, c("rset", "modelling_group", "touchstone_to")])
  if (nrow(new_rsets) > 0) {
    new_rsets$status <- "incomplete"
    names(new_rsets)[names(new_rsets) == "touchstone_to"] <- "touchstone"
    new_rsets$id <- seq(-1, by = -1, length.out = nrow(new_rsets))


    # Replace NAs in e$ff_info with the dummy ones

    new_rsets$mash <- paste(new_rsets$modelling_group, new_rsets$touchstone, sep = "\r")
    ff_na_rset_to$mash <- paste(ff_na_rset_to$modelling_group,
                                ff_na_rset_to$touchstone_to, sep = "\r")

    ff_na_rset_to$rset_to <- new_rsets$id[match(ff_na_rset_to$mash, new_rsets$mash)]

    new_rsets$mash <- NULL
    ff_na_rset_to$mash <- NULL


    # Set responsibility_set_comment.responsibility_set to the new
    # (maybe negative) id for the responsibility_set.

    new_responsibility_set_comments <-
      update_comments(e$rset_comments, ff, "rset", "responsibility_set")

    new_responsibility_set_comments$responsibility_set <-
      new_rsets$id[match(new_responsibility_set_comments$responsibility_set,
                     new_rsets$rset)]

    t[['responsibility_set_comment']] <- new_responsibility_set_comments

    new_rsets$rset <- NULL
    t[['responsibility_set']] <- new_rsets
  }



  # We've updated some details, so rebind ff...
  ff <- rbind(ff_non_na_rset_to, ff_na_rset_to)

  # For all entries were ff$resp_to is NA, we need
  # to create new responsibilities

  new_resps <- unique(ff[is.na(ff$resp_to),
                      c("resp", "rset_to", "scid", "bes", "sbes",
                        "is_open", "expectations")])

  rename_resps <- function(r) {
    names(r)[names(r) == "rset_to"] <- "responsibility_set"
    names(r)[names(r) == "rset"] <- "responsibility_set"
    names(r)[names(r) == "resp_to"] <- "id"
    names(r)[names(r) == "resp"] <- "id"
    names(r)[names(r) == "scid"] <- "scenario"
    names(r)[names(r) == "bes"] <- "current_burden_estimate_set"
    names(r)[names(r) == "sbes"] <- "current_stochastic_burden_estimate_set"
    r
  }

  new_resps <- rename_resps(new_resps)
  new_resps$newid <- seq(-1, by = -1, length.out = nrow(new_resps))

  # Create new responsibility_comments entries

  new_responsibility_comments <-
    update_comments(e$resp_comments, ff, "resp", "responsibility")

  # Handle where destination responsibility already existed.
  # For each row in that table, move any references to the previous
  # responsibility, to point to the new responsibility.
  # We are only dealing with most recent comment, so
  # nrow(ff_row) will be either 1 or 0.

  for (i in seq_len(nrow(new_responsibility_comments))) {
    ff_row <- ff[ff$resp == new_responsibility_comments$responsibility[i], ]
    if ((nrow(ff_row) == 1) && (!is.na(ff_row$resp_to))) {
      new_responsibility_comments$responsibility[i] <- ff_row$resp_to
    }
  }

  # And for any new responsibilities (which never existed before)
  # Again, we're only thinking of most recent comment, hence
  # nrow(resps_row) is either 1 or 0

  for (i in seq_len(nrow(new_resps))) {
    resps_row <- new_resps[new_resps$id[i] %in% new_responsibility_comments$responsibility, ]
    if (nrow(resps_row) == 1) {
      new_responsibility_comments$responsibility[i] <- resps_row$newid
    }
  }

  t[['responsibility_comment']] <- new_responsibility_comments

  new_resps$id <- new_resps$newid
  new_resps$newid <- NULL

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

  t[['responsibility']] <- rbind(new_resps, resps_existing, resps_remove_bes)
  t
}


test_transform_fast_forward <- function(transformed_data) {
  # Not much we can test here, as tables could
  # contain all or nothing.
}

###############################################################################

load_fast_forward <- function(transformed_data, con) {

  t <- transformed_data

  # So... Up to 4 tables to work on. Shorten names to make this code easier.

  # We only have work to do if there are responsibilities...

  r <- t[['responsibility']]
  if (is.null(r)) {
    return()
  }

  # Which responsibilities have responsibility_set
  # negative, or positive?

  rneg <- r[r[['responsibility_set']] < 0, ]
  rpos <- r[r[['responsibility_set']] >= 0, ]

  # There may not be responsibility_sets to add

  rs <- t[['responsibility_set']]
  num_rs <- if (is.null(rs)) 0 else nrow(rs)

  rsc <- t[['responsibility_set_comment']]
  rc <- t[['responsibility_comment']]

  # responsibility_set - if we have rows with negative ids,
  # then we need to create new db rows.

  if (num_rs > 0) {
    DBI::dbAppendTable(con, "responsibility_set",
                       rs[, c("modelling_group", "touchstone", "status")])


    # Now find the ids of the new rows that were added

    rs$mash <- paste(rs$modelling_group, rs$touchstone, sep = "\r")

    new_ids <- DBI::dbGetQuery(con, sprintf("
      SELECT id, CONCAT(modelling_group, '\r', touchstone) AS mash
        FROM responsibility_set
       WHERE CONCAT(modelling_group, '\r', touchstone) IN ('%s')",
         paste(rs$mash, collapse = "','")))

    # And bind to rs - which now has id (negative) and newid (positive)

    rs$newid <- new_ids$id[match(rs$mash, new_ids$mash)]

    # Now update any t.responsibility.responsibility_set
    # identifiers that were negative, replacing them with the
    # real keys.

    rneg[['responsibility_set']] <-
      rs$newid[match(rneg[['responsibility_set']], rs$id)]


    # responsibility_set_comment with the new serial ids

    rsc[['responsibility_set']] <-
      rs$newid[match(rsc[['responsibility_set']], rs$id)]

    r <- rbind(rneg, rpos)
  }

  # responsibility     - For the ones with negative ids,
  #                    - add, remembering the mapping again...

  rneg <- r[r$id < 0, ]
  rneg_no_id <- rneg
  rneg_no_id$id <- NULL

  # Assumption, following closing of old responsibilities:
  # Created responsibilities will be open. Close them
  # separately afterwards if necessary. (In part, this is
  # so we can see them in the portal and verify FF was ok)
  if (nrow(rneg_no_id) > 0) {    rneg_no_id$is_open <- TRUE

    DBI::dbAppendTable(con, "responsibility", rneg_no_id)
    rneg$mash <- paste(rneg[['responsibility_set']], rneg$scenario, sep = "\r")

    # Fetch the new ids mapping...

    new_ids <- DBI::dbGetQuery(con, sprintf("
      SELECT id, CONCAT(responsibility_set, '\r', scenario) AS mash
        FROM responsibility
       WHERE CONCAT(responsibility_set, '\r', scenario) IN ('%s')",
             paste(rneg$mash, collapse = "','")))

    rneg$newid <- new_ids$id[match(rneg$mash, new_ids$mash)]
    rpos$newid <- rpos$id

    # And apply to responsibility_comment, for those that needed it...

    negs <- which(rc[['responsibility']] < 0)

    rc[['responsibility']][negs] <-
      rneg$newid[match(rc[['responsibility']][negs], rneg$id)]
  }
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

  if (!is.null(rsc) > 0) {
    rsc$id <- NULL
    DBI::dbAppendTable(con, "responsibility_set_comment", rsc)
  }
  rc$id <- NULL
  DBI::dbAppendTable(con, "responsibility_comment", rc)
}
