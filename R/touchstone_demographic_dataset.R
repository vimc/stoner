extract_touchstone_demographic_dataset <- function(e, path, con) {

  e[['touchstone_demographic_dataset']] <- DBI::dbGetQuery(con, sprintf("
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
     WHERE touchstone IN %s", sql_in(e$touchstone_demographic_dataset_csv$touchstone)))

  e$touchstone_demographic_dataset_next_id <-
    next_id(con, "touchstone_demographic_dataset", "id")

  e <- c(e, list(
         demographic_source = db_get(con, "demographic_source", "code",
           unique(e$touchstone_demographic_dataset_csv$demographic_source)),
         demographic_statistic_type = db_get(con, "demographic_statistic_type",
           "code", unique(e$touchstone_demographic_dataset_csv$demographic_statistic_type)),
         tdd_touchstones = db_get(con, "touchstone", "id",
              unique(e$touchstone_demographic_dataset_csv$touchstone))
          )
  )

  # For the sake of simplicity, below is a Slightly generous lookup for
  # demographic_dataset - looking for matches of
  # either source or type, rather than filtering to the exact combinations,
  # which would be a bit complicated at this point, and not worth much.

  e <- c(e, list(demographic_dataset = DBI::dbGetQuery(con, sprintf("
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
      sql_in(unique(e$touchstone_demographic_dataset_csv$demographic_statistic_type)),
      sql_in(unique(e$touchstone_demographic_dataset_csv$demographic_source))))))

  # In the extract tests and the transform, I need to work with the demographic
  # dataset, so will mash them together once here. Not exactly an extract, but
  # a very simple thing that saves some repetition later.

  e$demographic_dataset$mash <- paste(
    e$demographic_dataset$dsource_code,
    e$demographic_dataset$dtype_code, sep = '#')

  e$touchstone_demographic_dataset_csv$mash <- paste(
    e$touchstone_demographic_dataset_csv$demographic_source,
    e$touchstone_demographic_dataset_csv$demographic_statistic_type, sep = '#')

  e$touchstone_demographic_dataset$mash <- paste(
    e$touchstone_demographic_dataset$dsource_code,
    e$touchstone_demographic_dataset$dtype_code, sep = '#')

  e
}

test_extract_touchstone_demographic_dataset <- function(e) {
  if (!is.null(e$touchstone_demographic_dataset_csv)) {

    expect_equal(sort(names(e$touchstone_demographic_dataset_csv)),
      c("demographic_source", "demographic_statistic_type", "mash", "touchstone"),
      label = "Correct columns in touchstone_demographic_dataset.csv")

    expect_type(e$touchstone_demographic_dataset_csv$touchstone, "character")
    expect_type(e$touchstone_demographic_dataset_csv$demographic_source, "character")
    expect_type(e$touchstone_demographic_dataset_csv$demographic_statistic_type, "character")

    expect_true(all(e$touchstone_demographic_dataset_csv$demographic_source %in%
                    e$demographic_source$code),
                label = "demographic sources in touchstone_demographic_dataset exist")

    expect_true(all(e$touchstone_demographic_dataset_csv$demographic_statistic_type %in%
                    e$demographic_statistic_type$code),
                label = "demographic statistic types in touchstone_demographic_dataset exist")

    all_touchstones <- unique(c(e$tdd_touchstones$id, e$touchstone_csv$id))

    expect_true(all(e$touchstone_demographic_dataset_csv$touchstone %in% all_touchstones),
                      label = "Touchstones in touchstone_demographic_dataset exist")

    expect_true(all(e$touchstone_demographic_dataset_csv$mash %in%
                    e$demographic_dataset$mash),
                      label = "Demographic datasets already exist")

    expect_equal(0, sum(duplicated(
                  e$touchstone_demographic_dataset_csv$mash)),
                  label = "No duplicated (Touchstone, demographic_dataset) pair in csv5")

  }
}

###############################################################################

transform_touchstone_demographic_dataset <- function(e) {

  tdd <- e$touchstone_demographic_dataset_csv

  tdd$demographic_dataset <- e$demographic_dataset$id[match(
      tdd$mash, e$demographic_dataset$mash)]

  tdd$demographic_source_id <- e$demographic_source$id[match(
      tdd$demographic_source, e$demographic_source$code)]

  tdd$demographic_statistic_type_id <- e$demographic_statistic_type$id[match(
      tdd$demographic_statistic_type, e$demographic_statistic_type$code)]

  # 1. Flag (touchstone,dataset) that already exist in touchstone_demographic_dataset db

  tdd$mash <- paste(tdd$demographic_source, tdd$demographic_statistic_type, sep = '#')
  tdd$already_exists_db <- tdd$mash %in% e$touchstone_demographic_dataset$mash

  # 2. If (touchstone,dataset) isn't already there, then...
  #    Is there (touchstone,dataset) of same demographic_statistic_type?
  #       a. If so, this should be an update of that row.
  #       b. If not, then this should be an extra row to be added.

  tdd$id <- e$touchstone_demographic_dataset$id[match(
    tdd$demographic_dataset, e$touchstone_demographic_dataset$demographic_dataset)]

  overwrites <- which(!tdd$already_exists_db)

  next_id <- e$touchstone_demographic_dataset_next_id

  for (overwrite in overwrites) {
    overwrite_id <- e$touchstone_demographic_dataset$id[
      e$touchstone_demographic_dataset$dtype ==
        tdd$demographic_statistic_type_id[overwrite] &
      e$touchstone_demographic_dataset$touchstone ==
        tdd$touchstone[overwrite]]

    if (length(overwrite_id) == 0) {
      overwrite_id <- next_id
      next_id <- next_id + 1L
    }
    tdd$id[overwrite] <- overwrite_id
    tdd$demographic_dataset[overwrite] <-
      e$touchstone_demographic_dataset$demographic_dataset[
        e$touchstone_demographic_dataset$id == overwrite_id]
  }

  tdd <- tdd[, c("already_exists_db", "demographic_dataset", "id", "touchstone")]

  list(touchstone_demographic_dataset = tdd)

}

test_transform_touchstone_demographic_dataset <- function(transformed_data) {
  tdd <- transformed_data$touchstone_demographic_dataset
  expect_equal(0, sum(duplicated(tdd$demographic_dataset)))
  expect_equal(0, sum(duplicated(tdd$id)))
  expect_false(any(is.na(transformed_data$tocuhstone_demographic_dataset$id)))
  expect_false(any(is.na(transformed_data$tocuhstone_demographic_dataset$demographic_dataset)))
}

###############################################################################

load_touchstone_demographic_dataset <- function(transformed_data, con) {
  to_edit <- add_return_edits("touchstone_demographic_dataset",
                              transformed_data, con)

  # For each row in to_edit, do an SQL update, as long as the touchstone
  # being referred to is in the in-preparation state.

  touchstone_status <- DBI::dbGetQuery(con, sprintf("
    SELECT id, status
      FROM touchstone
     WHERE id IN %s", sql_in(unique(tdd$touchstone))))

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
