###############################################################################

extract_responsibilities <- function(e, path, con) {

  if (is.null(e$responsibilities_csv)) return(NULL)

  if (nrow(e$responsiblities_csv) == 0) return(NULL)

  list(resp_touchstones = DBI::dbGetQuery(con, sprintf("
    SELECT * FROM touchstone WHERE id IN %s",
      sql_in(unique(e$responsibility_csv$touchstone)))))

}

test_extract_burden_estimate_expectation <- function(e) {
  if (is.null(e$responsibilities_csv)) return()

  if (nrow(e$responsiblities_csv) == 0) return(NULL)

  expect_equal(sort(names(e$responsibilities_csv)),
    c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
      "cohort_min_inclusive", "countries", "description", "modelling_group",
      "outcomes", "scenario", "touchstone", "year_max_inclusive", "year_min_inclusive"),
    label = "Columns in burden_estimate_expectation.csv")

  if (any(e$expectations_csv$year_min_inclusive > e$expectations_csv$year_max_inclusive)) {
    stop("Burden estimate expectations year_min_inclusive must be before year_max_inclusive")
  }

  if (any(e$expectations_csv$age_min_inclusive > e$expectations_csv$age_max_inclusive)) {
    stop("Burden estimate expectations age_min_inclusive must be before age_max_inclusive")
  }

  if (any(e$expectations_csv$cohort_min_inclusive > e$expectations_csv$cohort_max_inclusive)) {
    stop("Burden estimate expectations cohort_min_inclusive must be before cohort_max_inclusive")
  }



}

###############################################################################

transform_responsibilities <- function(e, t_so_far) {
  if (is.null(e$responsibilities_csv)) return(list())
  if (nrow(e$responsibilities_csv) == 0) return(list())

  new_touchstones <- t_so_far$touchstone[!t_so_far$touchstone$already_exists_db, ]
  new_touchstones$already_exists_db <- NULL
  touchstones <- rbind(e$resp_touchstones, new_touchstones)

  bee <- e$expectations_csv
  bee$scenario <- NULL
  bee$modelling_group <- NULL
  bee$version <- touchstones$touchstone_name[match(
    bee$touchstone, touchstones$id)]
  bee$touchstone <- NULL

  bee$id <- mash_id(
    bee, e$burden_estimate_expectation,
    c("age_max_inclusive", "age_min_inclusive", "cohort_max_inclusive",
      "cohort_min_inclusive", "countries", "description", "modelling_group",
      "outcomes", "scenario", "touchstone", "year_max_inclusive", "year_min_inclusive"))

  bee$already_exists_db <- !is.na(bee$id)
  bee$id <- NULL

  list(burden_estimate_expectation = bee)
}

test_transform_burden_estimate_expectation <- function(t) {


}

###############################################################################

load_responsibilities <- function(transformed_data, con) {
  #add_serial_rows("burden_estimate_expectation", transformed_data, con)
}
