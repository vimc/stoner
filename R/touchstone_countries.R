###############################################################################

extract_touchstone_country <- function(e, path, con) {
  csv <- e$touchstone_countries_csv

  if (is.null(csv)) {
    return(e)
  }

  all_diseases <- unique(unlist(split_semi(csv$diseases)))
  all_countries <- unique(unlist(split_semi(csv$countries)))
  all_touchstones <- unique(csv$touchstone)

  c(e, list(
    disease = db_get(con, "disease", "id", all_disease),
    country = db_get(con, "country", "id", all_countries),

    touchstone_country =
      db_get(con, "touchstone_country", "touchstone", all_countries),

    touchstone_country_touchstones =
      db_get(con, "touchstone", "id", all_touchstones),

    touchstone_country_db =
      db_get(con, "touchstone_country", "touchstone", "all_touchstones")
  ))
}

test_extract_touchstone_country <- function(e) {
  csv <- e$touchstone_countries_csv
  if (is.null(csv)) {
    return()
  }

  expect_true(all(unique(unlist(
    split_semi(csv$countries))) %in% e$country$id),
    label = "All countries in touchstone_country are recognised")

  expect_true(all(unique(unlist(
    split_semi(csv$diseases))) %in% e$disease$id),
    label = "All diseases in touchstone_country are recognised")

  all_touchstones <- unique(c(e$touchstone_country_touchstones$id,
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

  disease <- split_semi(csv$diseases)

  if (any(lengths(disease) < 1)) {
    stop("Empty disease column in touchstone_country")
  }

  countries <- rep(split_semi(csv$countries), lengths(disease))

  touchstone <- lapply(1:length(disease), function(x)
                  rep(csv$touchstone[x], length(disease[[x]])))

  touchstone_country <- data_frame(
    touchstone = rep(unlist(touchstone), lengths(countries)),
    disease = rep(unlist(disease), lengths(countries)),
    country = unlist(countries))

  tc_db <- e$touchstone_country_db

  tc_db$mash <- mash(tc_db, c("touchstone", "disease", "country"))

  touchstone_country$mash <-
    mash(touchstone_country, c("touchstone", "disease", "country"))

  if (any(duplicated(touchstone_country$mash))) {
    stop("Duplicated entries in touchstone_country.csv")
  }

  touchstone_country$id <- tc_db$id[match(touchstone_country$mash, tc_db$mash)]
  touchstone_country$mash <- NULL
  touchstone_country$already_exists_db <- !is.na(touchstone_country$id)

  touchstone_country <- fill_in_keys(touchstone_country,
                                     e$touchstone_country_next_id)

  list(touchstone_country = touchstone_country)
}

test_transform_touchstone_country <- function(transformed_data) {
  # All good tests done in extract, and in stoner unit tests.
}

###############################################################################

load_touchstone_country <- function(transformed_data, con) {
  add_serial_rows("touchstone_country", transformed_data, con)
}
