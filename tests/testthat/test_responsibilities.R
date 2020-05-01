context("responsibility")

# Here, we do tests on:

# burden_estimate_expectation.csv
# Cols: year_min_inclusive, year_max_inclusive
#       age_min_inclusive, age_max_inclusive
#       cohort_min_inclusive, cohort_max_inclusive
#       description, version
#

create_responsibilities <- function(test, modelling_group, touchstone, scenario,
                                    age_min_inclusive, age_max_inclusive,
                                    cohort_min_inclusive, cohort_max_inclusive,
                                    year_min_inclusive, year_max_inclusive,
                                    countries, outcomes, description, db = FALSE) {
  write.csv(
    data_frame(
      modelling_group = modelling_group,
      touchstone = touchstone,
      scenario = scenario,
      scenario_type = "standard",
      age_min_inclusive = age_min_inclusive,
      age_max_inclusive = age_max_inclusive,
      cohort_min_inclusive = cohort_min_inclusive,
      cohort_max_inclusive = cohort_max_inclusive,
      year_min_inclusive = year_min_inclusive,
      year_max_inclusive = year_max_inclusive,
      countries = countries,
      outcomes = outcomes,
      description = description),
    file.path(test$path, "meta",
      db_file(db, "responsibilities.csv")),
    row.names = FALSE
  )
}

test_responsibilities <- function(test, modelling_group, touchstone, scenario,
                                    age_min_inclusive, age_max_inclusive,
                                    cohort_min_inclusive, cohort_max_inclusive,
                                    year_min_inclusive, year_max_inclusive,
                                    countries, outcomes, description) {

  expect_present <- function(con, table, fields, values) {
    if (is.character(values)) {
      where_clause <- paste(paste(fields, values, sep = "='"),
                            collapse = "' AND ")
      where_clause <- sprintf("%s'", where_clause)
    } else {
      where_clause <- paste(paste(fields, values, sep = "="),
                            collapse = " AND ")
    }
    res <- DBI::dbGetQuery(test$con, sprintf(
      "SELECT * FROM %s WHERE %s", table, where_clause))
    expect_equal(nrow(res), 1)
    res
  }

  expect_multi <- function(con, table, fields, values, result_field, compare) {
    where_clause <- paste(paste(fields, values, sep = "="), collapse = " AND ")
    res <- DBI::dbGetQuery(test$con, sprintf(
      "SELECT %s FROM %s
        WHERE %s ORDER BY %s", result_field, table, where_clause, result_field))
    res <- paste(res[[result_field]], collapse = ";")
    expect_equal(res, compare)
  }

  test_single <- function(modelling_group, touchstone, scenario,
                          age_min_inclusive, age_max_inclusive,
                          cohort_min_inclusive, cohort_max_inclusive,
                          year_min_inclusive, year_max_inclusive,
                          countries, outcomes, description) {

    expect_present(test$con, "modelling_group", "id", modelling_group)
    res <- expect_present(test$con, "touchstone", "id", touchstone)
    expect_present(test$con, "touchstone_name", "id", res$touchstone_name)
    expect_present(test$con, "scenario_description", "id", scenario)
    scen <- expect_present(test$con, "scenario",
                           c("scenario_description", "touchstone"),
                           c(scenario, touchstone))
    res <- expect_present(test$con, "burden_estimate_expectation",
      c("age_min_inclusive", "age_max_inclusive", "cohort_min_inclusive",
        "cohort_max_inclusive", "year_min_inclusive", "year_max_inclusive",
        "description"),
      c(age_min_inclusive, age_max_inclusive, cohort_min_inclusive,
        cohort_max_inclusive, year_min_inclusive, year_max_inclusive,
        description))

    expect_multi(test$con, "burden_estimate_country_expectation",
      "burden_estimate_expectation", res$id, "country", countries)

    expect_multi(test$con, "burden_estimate_outcome_expectation",
                 "burden_estimate_expectation", res$id, "outcome", outcomes)

    res <- expect_present(test$con, "responsibility", "expectations", res$id)
    expect_equal(res$scenario, scen$id)

    expect_present(test$con, "responsibility_set",
                   c("id", "modelling_group", "touchstone"),
                   c(res$responsibility_set, modelling_group, touchstone))

  }

  for (i in seq_along(modelling_group)) {

    test_single(modelling_group[i], touchstone[i], scenario[i],
                age_min_inclusive[i], age_max_inclusive[i],
                cohort_min_inclusive[i], cohort_max_inclusive[i],
                year_min_inclusive[i], year_max_inclusive[i],
                countries[i], outcomes[i], description[i])
  }
}


test_that("New responsibility", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibilites(test)
  create_responsibilities(test, "LAP-elf", "nevis-1", "pies",
                          0, 100, 1900, 2100, 2000, 2100,
                          "AFG;ZWE", "cases;deaths", "FLU:LAP-elf:standard")
  do_test(test)
  test_responsibilities(test, "LAP-elf", "nevis-1", "pies",
                          0, 100, 1900, 2100, 2000, 2100,
                          "AFG;ZWE", "cases;deaths", "FLU:LAP-elf:standard")

})
