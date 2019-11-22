# Get all touchstone demographic dataset rows for a set
# of touchstones. Also lookup both strings and numerical
# ids for demographic source and statistic type.

db_tdd_for_touchstones <- function(con, touchstones) {
  DBI::dbGetQuery(con, sprintf("
    SELECT touchstone_demographic_dataset.id, touchstone,
           demographic_dataset,
           demographic_source.code as dsource_code,
           demographic_statistic_type.code as dtype_code,
           demographic_source.id as dsource,
           demographic_statistic_type.id as dtype
      FROM touchstone_demographic_dataset
      JOIN demographic_dataset
        ON demographic_dataset.id =
             touchstone_demographic_dataset.demographic_dataset
      JOIN demographic_source
        ON demographic_source.id = demographic_dataset.demographic_source
      JOIN demographic_statistic_type
        ON demographic_statistic_type.id =
             demographic_dataset.demographic_statistic_type
     WHERE touchstone IN %s", sql_in(touchstones)))
}

# Get all demographic_dataset information to cover the given
# types and sources (which are character). The query is a bit
# generous - lookup all types and sources, not just the specific
# combinations of those that we want.

db_dd_info <- function(con, types, sources) {
  DBI::dbGetQuery(con, sprintf("
    SELECT demographic_dataset.demographic_source,
           demographic_dataset.demographic_statistic_type,
           demographic_source.code AS dsource_code,
           demographic_statistic_type.code AS dtype_code,
           demographic_dataset.id
      FROM demographic_dataset
      JOIN demographic_source
        ON demographic_dataset.demographic_source = demographic_source.id
      JOIN demographic_statistic_type
        ON demographic_dataset.demographic_statistic_type =
             demographic_statistic_type.id
     WHERE demographic_statistic_type.code IN %s
        OR demographic_source.code IN %s",
                             sql_in(unique(types)),
                             sql_in(unique(sources))))
}

extract_touchstone_demographic_dataset <- function(e, path, con) {

  # Do we have any touchstone_demographic_dataset work to do?
  if (is.null(e$touchstone_demographic_dataset_csv)) {
    return(list())
  }

  e_tdd_csv <- e$touchstone_demographic_dataset_csv

  # Test csv columns are correct.

  if (!setequal(names(e_tdd_csv), c("demographic_source",
                                    "demographic_statistic_type",
                                    "touchstone"))) {
    stop("Incorrect columns in touchstone_demographic_dataset.csv")
  }

  if (class(e_tdd_csv$demographic_source) != "character") {
    stop(paste0("demographic_source in touchstone_demographic_dataset.csv ",
                "must be character"))
  }

  if (class(e_tdd_csv$demographic_statistic_type) != "character") {
    stop(paste0("demographic_statistic_type in touchstone_demographic_",
                "dataset.csv must be character"))
  }

  # Lookup any existing touchstone_demographic_datasets, for all
  # touchstones in the csv. (also attaches info about the demographic
  # source, statistic type and dataset for our lookups)

  e$db_tdd <- db_tdd_for_touchstones(con, e_tdd_csv$touchstone)

  # Now lookup demographic_source, demographic_statistic_type and
  # touchstone info for all given CSV rows that we might want to add.

  e <- c(e, list(
    db_dsrc = db_get(con, "demographic_source", "code",
      unique(e_tdd_csv$demographic_source)),

    db_dstype = db_get(con, "demographic_statistic_type", "code",
      unique(e_tdd_csv$demographic_statistic_type)),
      tdd_touchstones = db_get(con, "touchstone", "id",
      unique(e_tdd_csv$touchstone))
  ))

  # Now the demographic_dataset (which is source, type). For the sake of
  # simplicity, look for matches of either source, or type, rather than
  # filtering to the exact combinations, which would be a bit complicated
  # at this point, and not worth much. There aren't very many of these.

  e <- c(e, list(db_dd = db_dd_info(con,
    e_tdd_csv$demographic_statistic_type,
    e_tdd_csv$demographic_source)))

  # In the extract tests and the transform, I need to work with the demographic
  # dataset, so will mash them together once here. Not exactly an extract, but
  # a very simple thing that saves some repetition later.

  # A mash to compare with existing touchstone_demographic_datasets

  e$touchstone_demographic_dataset_csv$mash <-
    mash(e_tdd_csv, c("touchstone", "demographic_source",
                      "demographic_statistic_type"))

  e$db_tdd$mash <- mash(e$db_tdd, c("touchstone", "dsource_code", "dtype_code"))

  # A mash to compare with existing demographic_datasets

  e$touchstone_demographic_dataset_csv$ds_mash <-
    mash(e_tdd_csv, c("demographic_source", "demographic_statistic_type"))

  e$db_dd$mash <- mash(e$db_dd, c("dsource_code", "dtype_code"))

  e
}

test_extract_touchstone_demographic_dataset <- function(e) {
  if (!is.null(e$touchstone_demographic_dataset_csv)) {
    e_tdd_csv <- e$touchstone_demographic_dataset_csv

    testthat::expect_true(all(e_tdd_csv$demographic_source %in%
                              e$db_dsrc$code),
      label = "demographic sources in touchstone_demographic_dataset exist")

    testthat::expect_true(all(e_tdd_csv$demographic_statistic_type %in%
                    e$db_dstype$code),
      label = "demog. statistic types in touchstone_demographic_dataset exist")

    all_touchstones <- unique(c(e$tdd_touchstones$id, e$touchstone_csv$id))

    testthat::expect_true(all(e_tdd_csv$touchstone %in% all_touchstones),
      label = "Touchstones in touchstone_demographic_dataset exist")

    testthat::expect_true(all(e_tdd_csv$ds_mash %in% e$db_dd$mash),
      label = "Demographic datasets already exist")

    testthat::expect_equal(0, sum(duplicated(e_tdd_csv$mash)),
      label = "No duplicated (Touchstone, demographic_dataset) pair in csv")
  }
}

###############################################################################

transform_touchstone_demographic_dataset <- function(e) {

  tdd <- e$touchstone_demographic_dataset_csv
  if (is.null(tdd)) {
    return(list())
  }

  tdd$demographic_dataset <- e$db_dd$id[match(tdd$ds_mash, e$db_dd$mash)]

  # If any (touchstone,dataset) already exist in db's
  # touchstone_demographic_dataset table, then they can be ignored.

  tdd <- tdd[!tdd$mash %in% e$db_tdd$mash, ]

  # If there is (touchstone,dataset) of same demographic_statistic_type
  # then this should be an update (ie, to a new demog. source)
  # and we need to look up demographic_dataset id for the updated
  # source/type combo. (We assume this exists - part of demography
  # update)

  if (nrow(tdd) == 0) {
    return(list())
  }

  # First find complete matches of touchstone, source, type.

  tdd <- assign_serial_ids(tdd, e$db_tdd, "touchstone_demographic_dataset",
    c("touchstone", "demographic_source", "demographic_statistic_type"),
    c("touchstone", "dsource_code", "dtype_code"))

  # Next find matches of touchstone and type, where we want to update
  # the source. We then need to overwrite "already_exists_db", because
  # we've done a partial mash - these are edits and aren't really
  # in the db already.

  tdd_extras <- tdd[!tdd$already_exists_db, ]

  tdd_extras <- assign_serial_ids(tdd_extras, e$db_tdd,
                                  "touchstone_demographic_dataset",
    c("touchstone", "demographic_statistic_type"),
    c("touchstone", "dtype_code"))

  tdd_extras$already_exists_db <- FALSE

  # Bind the existing rows, and the edits together

  tdd <- rbind(tdd[tdd$already_exists_db, ], tdd_extras)

  tdd <- tdd[, c("already_exists_db", "demographic_dataset",
                 "id", "touchstone")]
  list(touchstone_demographic_dataset = tdd)
}

test_transform_touchstone_demographic_dataset <- function(transformed_data) {
  # All meaningful tests done in extract phase
}

###############################################################################

load_touchstone_demographic_dataset <- function(transformed_data, con) {
  res <- add_serial_rows("touchstone_demographic_dataset",
                         transformed_data, con)

  # For each row in to_edit, do an SQL update.

  if (nrow(res$edits) > 0) {
    touchstone_status <- DBI::dbGetQuery(con, sprintf("
      SELECT id, status
        FROM touchstone
       WHERE id IN %s", sql_in(unique(res$edits$touchstone))))

    for (r in seq_len(nrow(res$edits))) {

      entry <- res$edits[r, ]
      DBI::dbExecute(con, "
        UPDATE touchstone_demographic_dataset
           SET demographic_dataset = $1
         WHERE id = $2
           AND touchstone = $3",
        list(entry$demographic_dataset, entry$id, entry$touchstone)
      )
    }
  }
  res
}
