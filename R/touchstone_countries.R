###############################################################################

extract_touchstone_countries <- function(path, con) {
  e <- list()

  e$touchstone_countries_csv <- read_meta(path, "touchstone_countries.csv")

  all_diseases <- unique(unlist(strsplit(e$touchstone_countries_csv$diseases, ";")))
  all_countries <- unique(unlist(strsplit(e$touchstone_countries_csv$countries, ";")))
  all_touchstones <- unique(e$touchstone_countries_csv$touchstone)

  e$disease <- DBI::dbGetQuery(con, sprintf("
    SELECT * FROM disease WHERE id IN %s", sql_in(all_diseases)))

  e$country <- DBI::dbGetQuery(con, sprintf("
    SELECT * FROM country WHERE id IN %s", sql_in(all_countries)))

  e$touchstone_country <- DBI::dbGetQuery(con, sprintf("
    SELECT * FROM touchstone_country WHERE touchstone IN %s",
    sql_in(all_touchstones)))

  e$touchstone_country_touchstones <- DBI::dbGetQuery(con, sprintf("
    SELECT id FROM touchstone WHERE id IN %s", sql_in(all_touchstones)))

  e
}

test_extract_touchstone_countries <- function(e) {
  expect_true(all(unique(unlist(strsplit(e$touchstone_countries_csv$countries, ";")))
                  %in% e$country$id),
    label = "All countries in touchstone_country are recognised")

  expect_true(all(unique(unlist(strsplit(e$touchstone_countries_csv$diseases, ";")))
                  %in% e$disease$id),
    label = "All diseases in touchstone_country are recognised")

  all_touchstones <- unique(c(e$touchstone_country_touchstones$id,
                              e$touchstone_csv$id))

  expect_true(all(unique(e$touchstone_countries_csv$touchstone) %in%
                all_touchstones),
    label = "All touchstones in touchstone_country are recognised")
}

###############################################################################

transform_touchstone_country <- function(e) {

  # CSV Format: touchstone,disease1;disease2,country1;country2;country3

  disease <- strsplit(e$touchstone_countries_csv$diseases, ";")

  if (any(lengths(disease) < 1)) {
    stop("Empty disease column in touchstone_country")
  }

  countries <- rep(strsplit(e$touchstone_countries_csv$countries, ";"),
                   lengths(disease))

  touchstone <- lapply(1:length(disease), function(x)
                  rep(e$touchstone_countries_csv$touchstone[x], length(disease[[x]])))

  touchstone_country <- data_frame(
    touchstone = rep(unlist(touchstone), lengths(countries)),
    disease = rep(unlist(disease), lengths(countries)),
    country = unlist(countries))

}

test_transform_touchstone_country <- function(transformed_data) {
  expect_false(any(is.null(transformed_data$touchstone_country$touchstone)))
  expect_false(any(is.null(transformed_data$touchstone_country$disease)))
  expect_false(any(is.null(transformed_data$touchstone_country$country)))
}

###############################################################################

load_touchstone_country <- function(transformed_data, con) {
  add_return_edits("touchstone_country", transformed_data, con)
}
