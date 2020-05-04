context("responsibility")

# Here, we do tests on:

# burden_estimate_expectation.csv
# Cols: year_min_inclusive, year_max_inclusive
#       age_min_inclusive, age_max_inclusive
#       cohort_min_inclusive, cohort_max_inclusive
#       description, version
#

create_responsibilities <- function(test, resp, db = FALSE) {
  write.csv(
    data_frame(
      modelling_group = resp$modelling_group,
      touchstone = resp$touchstone,
      scenario = resp$scenario,
      scenario_type = "standard",
      age_min_inclusive = resp$age_min_inclusive,
      age_max_inclusive = resp$age_max_inclusive,
      cohort_min_inclusive = resp$cohort_min_inclusive,
      cohort_max_inclusive = resp$cohort_max_inclusive,
      year_min_inclusive = resp$year_min_inclusive,
      year_max_inclusive = resp$year_max_inclusive,
      countries = resp$countries,
      outcomes = resp$outcomes,
      description = resp$description),
    file.path(test$path, "meta",
      db_file(db, "responsibilities.csv")),
    row.names = FALSE
  )
}

test_responsibilities <- function(test, resp) {

  expect_present <- function(con, table, fields, values, op = "OR") {

    if (is.character(values)) {
      where_clause <- paste(paste(fields, values, sep = "='"),
                            collapse = sprintf("' %s ", op))
      where_clause <- sprintf("%s'", where_clause)
    } else {
      where_clause <- paste(paste(fields, values, sep = "="),
                            collapse = sprintf(" %s ", op))
    }
    res <- DBI::dbGetQuery(test$con, sprintf(
      "SELECT * FROM %s WHERE %s", table, where_clause))
    expect_gte(nrow(res), 1)
    res
  }

  expect_multi <- function(con, table, fields, values, result_field, compare) {
    where_clause <- paste(paste(fields, values, sep = "="), collapse = " OR ")
    res <- DBI::dbGetQuery(test$con, sprintf(
      "SELECT %s FROM %s
        WHERE %s ORDER BY %s", result_field, table, where_clause, result_field))
    res <- paste(res[[result_field]], collapse = ";")
    expect_equal(res, compare)
  }

  test_single <- function(resp) {

    expect_present(test$con, "modelling_group", "id", resp$modelling_group)
    res <- expect_present(test$con, "touchstone", "id", resp$touchstone)
    expect_present(test$con, "touchstone_name", "id", res$touchstone_name)
    expect_present(test$con, "scenario_description", "id", resp$scenario)
    scen <- expect_present(test$con, "scenario",
                           c("scenario_description", "touchstone"),
                           c(resp$scenario, resp$touchstone), "AND")

    res <- expect_present(test$con, "burden_estimate_expectation",
      c("age_min_inclusive", "age_max_inclusive", "cohort_min_inclusive",
        "cohort_max_inclusive", "year_min_inclusive", "year_max_inclusive",
        "description"),
      c(resp$age_min_inclusive, resp$age_max_inclusive,
        resp$cohort_min_inclusive, resp$cohort_max_inclusive,
        resp$year_min_inclusive, resp$year_max_inclusive,
        resp$description), "AND")

    expect_multi(test$con, "burden_estimate_country_expectation",
            "burden_estimate_expectation", res$id, "country", resp$countries)

    expect_multi(test$con, "burden_estimate_outcome_expectation",
            "burden_estimate_expectation", res$id, "outcome", resp$outcomes)

    # May well be multiple responsibilities with same expectations, so
    # scenarios may differ, but we'll test all of them eventually.

    res <- expect_present(test$con, "responsibility", "expectations", res$id)
    expect_true(scen$id %in% res$scenario)

    # But all expectations should be in same responsibility set, so
    # this sohuld end up with 1 entry.

    resp_set <- unique(res$responsibility_set)
    expect_length(resp_set, 1)

    expect_present(test$con, "responsibility_set",
            c("id", "modelling_group", "touchstone"),
            c(resp_set, resp$modelling_group, resp$touchstone),
            "AND")

  }

  for (i in seq_len(nrow(resp))) {
    test_single(resp[i, ])
  }
}

default_responsibility <- function() {
  data_frame(
    modelling_group = "LAP-elf",
    touchstone = "nevis-1",
    scenario = "pies",
    age_min_inclusive = 0,
    age_max_inclusive = 100,
    cohort_min_inclusive = 1900,
    cohort_max_inclusive = 2100,
    year_min_inclusive = 2000,
    year_max_inclusive = 2100,
    countries = "AFG;ZWE",
    outcomes = "cases;deaths",
    description = "FLU:LAP-elf:standard"
  )
}

test_that("New responsibility - standard", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  create_responsibilities(test, resp)
  do_test(test)
  test_responsibilities(test, resp)

})

test_that("Add countries and outcomes to existing expectations", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)

  resp <- default_responsibility()
  resp$countries <- "AFG"
  resp$outcomes <- "cases"
  create_responsibilities(test, resp)
  do_test(test)

  # Now clear up all the csvs (including the db_ pre-install ones)
  # and add another country and outcome...

  clear_files(test)
  resp$countries <- "ZWE"
  resp$outcomes <- "deaths"
  create_responsibilities(test, resp)
  do_test(test)

  resp$countries <- "AFG;ZWE"
  resp$outcomes <- "cases;deaths"
  test_responsibilities(test, resp)

})

test_that("Add multiple responsibilities in one go, separate csv lines", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- rbind(default_responsibility(), default_responsibility())
  resp$scenario <- c("hot_chocolate", "pies")
  create_responsibilities(test, resp)
  do_test(test)
  test_responsibilities(test, resp)

  # Additionally check that they used the same expectation.

})

test_that("Add new responsibility to existing responsibility_set", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  create_responsibilities(test, resp)
  do_test(test)

  clear_files(test)
  resp$scenario <- "hot_chocolate"
  create_responsibilities(test, resp)
  do_test(test)

  resp <- rbind(resp, resp)
  resp$scenario <- c("hot_chocolate", "pies")
  test_responsibilities(test, resp)

  # Additionally check that they used the same expectation.

  expect_equal(1, length(DBI::dbGetQuery(test$con, "
    SELECT DISTINCT expectations FROM responsibility")$expectations))



})


