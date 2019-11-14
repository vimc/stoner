###############################################################################

extract_touchstone_country <- function(e, path, con) {
  csv <- e$touchstone_countries_csv
  if (is.null(csv)) {
    return(e)
  }

  if (!setequal(names(csv), c("touchstone", "diseases", "countries"))) {
    stop("Invalid columns in touchstone_country.csv")
  }

  c(e, list(
    tc_disease = db_get(con, "disease", "id",
                        unique(unlist(split_semi(csv$diseases)))),
    tc_country = db_get(con, "country", "id",
                        unique(unlist(split_semi(csv$countries)))),
    tc_touchstone = db_get(con, "touchstone", "id",
                        unique(csv$touchstone)),
    db_touchstone_country = db_get(con, "touchstone_country", "touchstone",
                                   unique(csv$touchstone))
  ))
}

test_extract_touchstone_country <- function(e) {
  csv <- e$touchstone_countries_csv
  if (is.null(csv)) {
    return()
  }

  diseases <- lapply(csv$diseases, split_semi)
  countries <- lapply(csv$countries, split_semi)

  if (any(lengths(diseases) < 1)) {
    stop("Empty disease entry in touchstone_country")
  }

  if (any(lengths(countries) < 1)) {
    stop("Empty country entry in touchstone_country")
  }

  expect_true(all(unique(unlist(countries)) %in% e$tc_country$id),
    label = "All countries in touchstone_country are recognised")

  expect_true(all(unique(unlist(diseases)) %in% e$tc_disease$id),
    label = "All diseases in touchstone_country are recognised")

  # Touchstones either already exist, or are in the touchstone_csv
  # added in this import.

  all_touchstones <- unique(c(e$tc_touchstone$id,
                              e$touchstone_csv$id))

  expect_true(all(unique(csv$touchstone) %in% all_touchstones),
    label = "All touchstones in touchstone_country are recognised")
}

###############################################################################

transform_touchstone_country <- function(e) {

  # CSV Format: touchstone,disease1;disease2,country1;country2;country3

  csv <- e$touchstone_countries_csv
  if (is.null(csv)) {
    return(list())
  }

  if (nrow(csv) == 0) {
    return(list())
  }

  diseases <- lapply(csv$diseases, split_semi)
  countries <- lapply(csv$countries, split_semi)

  # Build a table that multiplies out countries and diseases...
  # for each touchstone.

  expand_countries <- unlist(lapply(1:nrow(csv), function(x) {
    rep(countries[[x]], length(diseases[[x]]))
  }))

  expand_diseases <- unlist(lapply(1:nrow(csv), function(x) {
    rep(diseases[[x]], each = length(countries[[x]]))
  }))

  expand_touchstones <- unlist(lapply(1:nrow(csv), function(x) {
    rep(csv$touchstone[[x]], each = (length(diseases[[x]]) * length(countries[[x]])))
  }))

  touchstone_country <- data_frame(
    touchstone = expand_touchstones,
    disease = expand_diseases,
    country = expand_countries)

  # Check for any duplicates in incoming CSV and error..

  touchstone_country$mash <- mash(touchstone_country)
  expect_false(any(duplicated(touchstone_country$mash)),
   label = "No duplicated entries in expanded touchstone_country csv")

  # Look up ids in db - record ids for rows that exist, or
  # assign negative id for new rows.

  tc_db <- e$db_touchstone_country
  tc_db$mash <- mash(tc_db, c("touchstone", "disease", "country"))

  touchstone_country$id <- tc_db$id[match(touchstone_country$mash, tc_db$mash)]
  touchstone_country$mash <- NULL

  # The id==NAs are the new rows

  which_nas <- which(is.na(touchstone_country$id))
  touchstone_country$already_exists_db <- !is.na(touchstone_country$id)

  touchstone_country$id[which_nas] <- seq(from = -1, by = -1,
                                          length.out = length(which_nas))

  list(touchstone_country = touchstone_country)
}

test_transform_touchstone_country <- function(transformed_data) {
  # All good tests done in extract, and in stoner unit tests.
}

###############################################################################

load_touchstone_country <- function(transformed_data, con) {
  add_serial_rows("touchstone_country", transformed_data, con)
}
