# Get all touchstone demographic dataset rows for a set
# of touchstones, including id->code for demographic source
# and statistic type

db_tdd_for_touchstones <- function(con, touchstones) {
  DBI::dbGetQuery(con, sprintf("
      SELECT touchstone_demographic_dataset.id, touchstone, demographic_dataset,
           demographic_source.code as dsource_code,
           demographic_statistic_type.code as dtype_code,
           demographic_source.id as dsource,
           demographic_statistic_type.id as dtype
      FROM touchstone_demographic_dataset
      JOIN demographic_dataset
        ON demographic_dataset.id = touchstone_demographic_dataset.demographic_dataset
      JOIN demographic_source
        ON demographic_source.id = demographic_dataset.demographic_source
      JOIN demographic_statistic_type
        ON demographic_statistic_type.id = demographic_dataset.demographic_statistic_type
     WHERE touchstone IN %s", sql_in(touchstones)))
}

# Get all demographic_dataset information to cover the given
# types and sources (which are character)

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
        ON demographic_dataset.demographic_statistic_type = demographic_statistic_type.id
     WHERE demographic_statistic_type.code IN %s
        OR demographic_source.code IN %s",
                             sql_in(unique(types)),
                             sql_in(unique(sources))))
}

extract_touchstone_demographic_dataset <- function(e, path, con) {

  if (!is.null(e[['touchstone_demographic_dataset_csv']])) {

    # Test for columns, before it goes wrong.

    if (length(names(e$touchstone_demographic_dataset_csv)) !=5) {
      stop("Incorrect column count in touchstone_demographic_dataset.csv")
    }

    if (any(sort(names(e$touchstone_demographic_dataset_csv)) !=
            c("demographic_source", "demographic_statistic_type",
              "touchstone"))) {
      stop("Incorrect columns in touchstone_demographic_dataset.csv")
    }

    if (class(e$touchstone_demographic_dataset_csv$demographic_source)
        != "character") {
      stop("demographic_source in touchstone_demographic_dataset.csv must be character")
    }

    if (class(e$touchstone_demographic_dataset_csv$demographic_statistic_type)
        != "character") {
      stop("demographic_statistic_type in touchstone_demographic_dataset.csv must be character")
    }

    # Lookup touchstone_demographic_dataset for any given rows, and
    # cbind with info about the demographic source, statistic type and dataset

    e[['db_tdd']] <- db_tdd_for_touchstones(con,
      e$touchstone_demographic_dataset_csv$touchstone)

    # Next id for adding rows to touchstone_demographic_dataset...

    e[['tdd_next_id']] <-
      next_id(con, "touchstone_demographic_dataset", "id")

    # Look up demographic_source, demographic_statistic_type and
    # touchstone info for all given CSV rows.

    e <- c(e, list(
         db_dsrc = db_get(con, "demographic_source", "code",
           unique(e$touchstone_demographic_dataset_csv$demographic_source)),

         db_dstype = db_get(con, "demographic_statistic_type",
           "code", unique(e$touchstone_demographic_dataset_csv$demographic_statistic_type)),

         tdd_touchstones = db_get(con, "touchstone", "id",
              unique(e$touchstone_demographic_dataset_csv$touchstone))
          )
    )

  # For the sake of simplicity, do a generous lookup for
  # demographic_dataset - looking for matches of
  # either source or type, rather than filtering to the exact combinations,
  # which would be a bit complicated at this point, and not worth much.

    e <- c(e, list(db_dd = db_dd_info(con,
      e$touchstone_demographic_dataset_csv$demographic_statistic_type,
      e$touchstone_demographic_dataset_csv$demographic_source)))

  # In the extract tests and the transform, I need to work with the demographic
  # dataset, so will mash them together once here. Not exactly an extract, but
  # a very simple thing that saves some repetition later.

    # A mash to compare with existing touchstone_demographic_datasets

    e[['touchstone_demographic_dataset_csv']]$mash <- paste(
      e$touchstone_demographic_dataset_csv$touchstone,
      e$touchstone_demographic_dataset_csv$demographic_source,
      e$touchstone_demographic_dataset_csv$demographic_statistic_type,
        sep = '\r')

    e$db_tdd$mash <- paste(e$db_tdd$touchstone,
                           e$db_tdd$dsource_code,
                           e$db_tdd$dtype_code, sep = '\r')

    # A mash to compare with existing demographic_datasets

    e[['touchstone_demographic_dataset_csv']]$ds_mash <- paste(
      e$touchstone_demographic_dataset_csv$demographic_source,
      e$touchstone_demographic_dataset_csv$demographic_statistic_type,
      sep = '\r')

    e$db_dd$mash <- paste(e$db_dd$dsource_code,
                          e$db_dd$dtype_code, sep = '\r')
    e

  } else {
    list()
  }
}

test_extract_touchstone_demographic_dataset <- function(e) {
  if (!is.null(e$touchstone_demographic_dataset_csv)) {


    expect_true(all(e$touchstone_demographic_dataset_csv$demographic_source %in%
                    e$db_dsrc$code),
                label = "demographic sources in touchstone_demographic_dataset exist")

    expect_true(all(e$touchstone_demographic_dataset_csv$demographic_statistic_type %in%
                    e$db_dstype$code),
                label = "demographic statistic types in touchstone_demographic_dataset exist")

    all_touchstones <- unique(c(e$tdd_touchstones$id, e$touchstone_csv$id))

    expect_true(all(e$touchstone_demographic_dataset_csv$touchstone %in% all_touchstones),
                      label = "Touchstones in touchstone_demographic_dataset exist")

    expect_true(all(e$touchstone_demographic_dataset_csv$ds_mash %in%
                    e$db_dd$mash),
                      label = "Demographic datasets already exist")

    expect_equal(0, sum(duplicated(
                  e$touchstone_demographic_dataset_csv$mash)),
                  label = "No duplicated (Touchstone, demographic_dataset) pair in csv")
  }
}

###############################################################################

transform_touchstone_demographic_dataset <- function(e) {

  tdd <- e$touchstone_demographic_dataset_csv

  if (!is.null(tdd)) {
    tdd$demographic_dataset <- e$db_dd$id[match(tdd$ds_mash, e$db_dd$mash)]

    tdd$demographic_source_id <- e$db_dsrc$id[match(
      tdd$demographic_source, e$db_dsrc$code)]

    tdd$demographic_statistic_type_id <- e$db_dstype$id[match(
      tdd$demographic_statistic_type, e$db_dstype$code)]

  # If any (touchstone,dataset) already exist in db's
  # touchstone_demographic_dataset table, then they can be ignored.

    tdd <- tdd[!tdd$mash %in% e$db_tdd$mash, ]

  # If there is (touchstone,dataset) of same demographic_statistic_type
  #   then this should be an update (ie, to a new demog. source)
  #   and we need to look up demographic_dataset id for the updated
  #   source/type combo. (We assume this exists - part of demography
  #   update)

    tdd$mash <- paste(tdd$touchstone, tdd$demographic_statistic_type,
                      sep = '\r')

    e$db_tdd$mash <- paste(e$db_tdd$touchstone,
                           e$db_tdd$dtype_code, sep = '\r')

    tdd$id <- e$db_tdd$id[match(tdd$mash, e$db_tdd$mash)]

    which_nas <- which(is.na(tdd$id))
    tdd$id[which_nas] <- seq(from = e$tdd_next_id, by = 1,
                             length.out = length(which_nas))

    tdd[['already_exists_db']] <- FALSE
    tdd <- tdd[, c("already_exists_db", "demographic_dataset", "id", "touchstone")]
    list(touchstone_demographic_dataset = tdd)
  } else {
    list()
  }
}

test_transform_touchstone_demographic_dataset <- function(transformed_data) {
  # All meaningful tests done in extract phase
}

###############################################################################

load_touchstone_demographic_dataset <- function(transformed_data, con) {
  to_edit <- add_return_edits("touchstone_demographic_dataset",
                              transformed_data, con)

  # For each row in to_edit, do an SQL update, as long as the touchstone
  # being referred to is in the in-preparation state.

  if (nrow(to_edit)>0) {
    touchstone_status <- DBI::dbGetQuery(con, sprintf("
      SELECT id, status
        FROM touchstone
       WHERE id IN %s", sql_in(unique(to_edit$touchstone))))

    for (r in seq_len(nrow(to_edit))) {

      entry <- to_edit[r, ]
      if (touchstone_status$status[touchstone_status$id == entry$touchstone] !=
          'in_preparation') {
        stop(sprintf("Can't update touchstone_demographic_dataset - %s is not in-prep",
                   entry$touchstone))
      }

      DBI::dbExecute(con, "
        UPDATE touchstone_demographic_dataset
           SET demographic_dataset = $1
         WHERE id = $2
           AND touchstone = $3",
        list(entry$demographic_dataset, entry$id, entry$touchstone,)
      )
    }
  }
}
