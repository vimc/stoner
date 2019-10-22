###############################################################################

extract_burden_estimate_expectation <- function(path, con) {
  e <- list()

  e$expectations_csv <- read_meta(path, "burden_estimate_expectation.csv")

  base_touchstones <- DBI::dbGetQuery(con, sprintf("
    SELECT touchstone_name
      FROM touchstone
     WHERE id IN %s", sql_in(e$expectations_csv$touchstone)))$touchstone_name

  e$burden_estimate_expectation <-
    db_get(con, "burden_estimate_expectation", "version", base_touchstones)

  e
}

test_extract_burden_estimate_expectation <- function(e) {
  expect_equal(sort(names(e$expectations_csv)),
    c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
      "cohort_min_inclusive", "description", "modelling_group", "scenario",
      "touchstone", "year_max_inclusive", "year_min_inclusive"),
    label = "Columns in burden_estimate_expectation.csv")
}

###############################################################################

transform_burden_estimate_expectation <- function(e, t_so_far) {

  new_touchstones <- t_so_far$touchstone[!t_so_far$touchstone$already_exists_db, ]
  new_touchstones$already_exists_db <- NULL
  touchstones <- rbind(e$touchstone, new_touchstones)

  bee <- e$expectations_csv
  bee$scenario <- NULL
  bee$modelling_group <- NULL
  bee$version <- touchstones$touchstone_name[match(
    bee$touchstone, touchstones$id)]
  bee$touchstone <- NULL

  bee$id <- mash_id(
    bee, e$burden_estimate_expectation,
    c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
      "cohort_min_inclusive", "description",
      "version", "year_max_inclusive", "year_min_inclusive"))

  bee$already_exists_db <- !is.na(bee$id)

  list(burden_estimate_expectation = bee)
}

test_transform_burden_estimate_expectation <- function(t) {

}

###############################################################################

load_burden_estimate_expectation <- function(transformed_data, con) {

}
