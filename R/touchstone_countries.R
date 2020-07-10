###############################################################################

extract_touchstone_country <- function(e, path, con) {
  csv <- e$touchstone_countries_csv
  if (is.null(csv)) {
    return(list())
  }

  if (!setequal(names(csv), c("touchstone", "disease", "country"))) {
    stop("Invalid columns in touchstone_country.csv")
  }

  disease <- unlist(lapply(csv$disease, split_semi))
  country <- unlist(lapply(csv$country, split_semi))

  list(
    touchstone_countries_csv = csv,
    tc_disease = db_get(con, "disease", "id", unique(disease)),
    tc_country = db_get(con, "country", "id", unique(country)),
    db_touchstone_country = db_get(con, "touchstone_country", "touchstone",
                                   unique(csv$touchstone))
  )
}

test_extract_touchstone_country <- function(e) {
  csv <- e$touchstone_countries_csv
  if (is.null(csv)) {
    return()
  }

  diseases <- lapply(csv$disease, split_semi)
  countries <- lapply(csv$country, split_semi)

  if (any(lengths(diseases) < 1)) {
    stop("Empty disease entry in touchstone_country")
  }

  if (any(lengths(countries) < 1)) {
    stop("Empty country entry in touchstone_country")
  }

  testthat::expect_true(all(unique(unlist(countries)) %in% e$tc_country$id),
    label = "All countries in touchstone_country are recognised")

  testthat::expect_true(all(unique(unlist(diseases)) %in% e$tc_disease$id),
    label = "All diseases in touchstone_country are recognised")

}

###############################################################################

transform_touchstone_country <- function(e) {

  # CSV Format: touchstone,disease1;disease2,country1;country2;country3

  csv <- e$touchstone_countries_csv
  if (is.null(csv)) {
    return(list())
  }

  diseases <- lapply(csv$disease, split_semi)
  countries <- lapply(csv$country, split_semi)

  # Build a table that multiplies out countries and diseases...
  # for each touchstone.

  expand_countries <- unlist(lapply(seq_len(nrow(csv)), function(x) {
    rep(countries[[x]], length(diseases[[x]]))
  }))

  expand_diseases <- unlist(lapply(seq_len(nrow(csv)), function(x) {
    rep(diseases[[x]], each = length(countries[[x]]))
  }))

  expand_touchstones <- unlist(lapply(seq_len(nrow(csv)), function(x) {
    rep(csv$touchstone[[x]], each = (length(diseases[[x]]) *
                                     length(countries[[x]])))
  }))

  touchstone_country <- data_frame(
    touchstone = expand_touchstones,
    disease = expand_diseases,
    country = expand_countries)

  touchstone_country <- touchstone_country[!duplicated(
    paste(touchstone_country$touchstone, touchstone_country$disease,
          touchstone_country$country, sep = "\r")), ]

  touchstone_country <- assign_serial_ids(touchstone_country,
                                          e$db_touchstone_country,
                                          "touchstone_country")

  list(touchstone_country = touchstone_country)
}

test_transform_touchstone_country <- function(transformed_data) {
  # All good tests done in extract, and in stoner unit tests.
}

###############################################################################

load_touchstone_country <- function(transformed_data, con) {
  add_serial_rows("touchstone_country", transformed_data, con)
}
