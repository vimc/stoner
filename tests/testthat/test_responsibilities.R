context("responsibility")

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

  # Check that there is an entry in "table", where "fields" are equal to
  # "values", such that the entries in "compare" exist in "result_field"

  expect_multi <- function(con, table, field, value, result_field, compare) {
    res <- DBI::dbGetQuery(con, sprintf("
      SELECT DISTINCT %s FROM %s
               WHERE %s = '%s'
                 AND %s IN %s", result_field, table,
                                field, value,
                                result_field,
                                sql_in(compare)))

    expect_true(all(compare == res$country))
    res
  }

  test_single <- function(line) {

    # Here, line is one line in the responsibilities.csv file, which
    # might actually represent multiple responsibilities, since we can
    # have semi-colonned scenarios on one line

    # Some basic existence tests first. Does the moelling group exist...

    expect_present(test$con, "modelling_group", "id", line$modelling_group)

    # Does the touchstone exist, and have a matching touchstone_name.

    tsinfo <- expect_present(test$con, "touchstone", "id", line$touchstone)
    expect_present(test$con, "touchstone_name", "id", tsinfo$touchstone_name)

    # There should be exactly one responsibility_set
    # for this (modelling_group, touchstone)

    resp_set <- expect_present(test$con, "responsibility_set",
                               c("modelling_group", "touchstone"),
                               c(line$modelling_group, line$touchstone), "AND")
    expect_equal(nrow(resp_set), 1)

    # We may have multiple semi-colon separate scenarios...
    # Expand and check all exist.

    line_scenarios <- unique(split_semi(line$scenario))
    scenario_descs <- expect_present(test$con, "scenario_description", "id",
                                    line_scenarios)
    expect_equal(nrow(scenario_descs), length(line_scenarios))

    # Retrieve numerical ids for those scenarios

    scenarios <- expect_present(test$con, "scenario", "scenario_description",
                                scenario_descs$id)

    expect_equal(nrow(scenarios), nrow(scenario_descs))

    # Should be one line for each responsibility, matching
    # responsibility_set and scenario

    for (scenario in scenarios$id) {
      resp <- expect_present(test$con, "responsibility",
                             c("responsibility_set", "scenario"),
                             c(resp_set$id, scenario), "AND")

      expect_equal(nrow(resp), 1)

      # Lookup the expectations (test there's 1, although that
      # is covered by db schema), and test all the properties
      # match what our csv says

      expec <- expect_present(test$con, "burden_estimate_expectation",
                              "id", resp$expectations)
      expect_equal(expec$age_max_inclusive, line$age_max_inclusive)
      expect_equal(expec$age_min_inclusive, line$age_min_inclusive)
      expect_equal(expec$year_max_inclusive, line$year_max_inclusive)
      expect_equal(expec$year_min_inclusive, line$year_min_inclusive)
      expect_equal(expec$cohort_max_inclusive, line$cohort_max_inclusive)
      expect_equal(expec$cohort_min_inclusive, line$cohort_min_inclusive)
      expect_equal(expec$description, line$description)
      expect_equal(expec$version, tsinfo$touchstone_name)

      # Test countries and outcomes also match

      countries <- unique(split_semi(line$countries))
      expect_multi(test$con, "burden_estimate_country_expectation",
                   "burden_estimate_expectation", resp$expectations,
                   "country", countries)

      outcomes <- unique(split_semi(line$outcomes))
      expect_multi(test$con, "burden_estimate_outcome_expectation",
                   "burden_estimate_expectation", resp$expectations,
                   "outcome", outcomes)

    }

    invisible()

  }

  # Build the description entry here, then test
  # that every line in the responsibilities_csv file is
  # represented...

  resp$description <- paste(
    resp$disease, resp$modelling_group, resp$scenario_type, sep = ':')

  for (i in seq_len(nrow(resp))) {
    test_single(resp[i, ])
  }
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

test_that("New responsibility - no countries or outcomes first", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$countries <- ""
  resp$outcomes <- ""
  create_responsibilities(test, resp)
  do_test(test)
  test_responsibilities(test, resp)
  clear_files(test)
  resp$countries <- "AFG;ZWE"
  resp$outcomes <- "cases;deaths"
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

  expect_equal(1, length(DBI::dbGetQuery(test$con, "
    SELECT DISTINCT expectations FROM responsibility")$expectations))
})

test_that("Add multiple responsibilities in one go, same csv line", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$scenario <- "hot_chocolate;pies"
  create_responsibilities(test, resp)
  do_test(test)
  test_responsibilities(test, resp)

  # Additionally check that they used the same expectation.

  expect_equal(1, length(DBI::dbGetQuery(test$con, "
    SELECT DISTINCT expectations FROM responsibility")$expectations))
})

#Test below is the only one that fails locally...

test_that("Add new responsibility to existing responsibility_set", {

  # Here, we're going to add some standard responsibilities,
  # and then do exactly the same again but with two scenarios
  # instead of one. We should end up with the same expectation
  # reused, but different responsibilities, within the same
  # responsibility_set

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
  # Because it should.

  expect_equal(1, length(DBI::dbGetQuery(test$con, "
    SELECT DISTINCT expectations FROM responsibility")$expectations))

})

test_that("OK with No rows in responsibility file", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp <- resp[resp$scenario == 'potato', ]
  create_responsibilities(test, resp)
  do_test(test)
  expect_equal(0, nrow(DBI::dbReadTable(test$con, "responsibility")))
  expect_equal(0, nrow(DBI::dbReadTable(test$con, "burden_estimate_expectation")))
})

test_that("Invalid country detected", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$countries[1] <- "Slough"
  create_responsibilities(test, resp)
  expect_error(do_test(test), "Unknown responsibility countries:")
})

test_that("Invalid outcomes", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$outcomes[1] <- "Incorrect Outcomes"
  create_responsibilities(test, resp)
  expect_error(do_test(test), "Unknown responsibility outcomes:")
})

test_that("Invalid modelling group detected", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$modelling_group[1] <- "Unknown Modelling Group"
  create_responsibilities(test, resp)
  expect_error(do_test(test), "Unknown responsibility modelling_groups:")
})

test_that("Incorrect age_min_inclusive/age_max_inclusive", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$age_min_inclusive <- 100
  resp$age_max_inclusive <- 0
  create_responsibilities(test, resp)
  expect_error(do_test(test), "Responsibility age_min_inclusive must be before age_max_inclusive")
})

test_that("Incorrect cohort_min_inclusive/cohort_max_inclusive", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$cohort_min_inclusive <- 100
  resp$cohort_max_inclusive <- 0
  create_responsibilities(test, resp)
  expect_error(do_test(test), "Responsibility cohort_min_inclusive must be before cohort_max_inclusive")
})

test_that("Incorrect year_min_inclusive/year_max_inclusive", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$year_min_inclusive <- 100
  resp$year_max_inclusive <- 0
  create_responsibilities(test, resp)
  expect_error(do_test(test), "Responsibility year_min_inclusive must be before year_max_inclusive")
})

test_that("Incorrect disease", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$disease <- "imposter_syndrome"
  create_responsibilities(test, resp)
  expect_error(do_test(test), "Unknown responsibility diseases: ")
})



test_that("Multiple modelling groups should have different expectations", {

  # Different responsibility_sets should have different expectations - even
  # if they look the same.

  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp2 <- default_responsibility()
  resp2$modelling_group <- "EBHQ-bunny"
  resp <- rbind(resp, resp2)
  create_responsibilities(test, resp)
  do_test(test)
  test_responsibilities(test, resp)
})

test_that("Import previous dump to give same state", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  resp$scenario <- "hot_chocolate;pies"
  create_responsibilities(test, resp)
  do_test(test)
  test_responsibilities(test, resp)
  dump
})

test_that("New responsibility_set - touchstone finished", {
  # Set up standard touchstones, make one finished,
  # then try to add brand new responsibility info to it.
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  do_test(test)
  DBI::dbExecute(test$con, "UPDATE touchstone SET status = 'finished'")
  clear_files(test)
  resp <- default_responsibility()
  create_responsibilities(test, resp)
  expect_error(do_test(test),
    "Error - attempt to add responsibility_set for non in-prep\ touchstones:")
})

test_that("New responsibility - touchstone finished", {
  # This time, set up touchstones including a responsibility set,
  # set the touchstone to finished, and try adding another responsibility
  # to the responsibility_set that already exists.

  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  create_responsibilities(test, resp)
  do_test(test)
  DBI::dbExecute(test$con, "UPDATE touchstone SET status = 'finished'")
  clear_files(test)
  resp <- default_responsibility()
  resp$scenario <- "pies"
  create_responsibilities(test, resp)
  expect_error(do_test(test),
    "Error - attempt to add responsibility for non in-prep touchstones:")
})

test_that("Test dump can be reimported", {

  # First create a standard import
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resp <- default_responsibility()
  create_responsibilities(test, resp)
  do_test(test)

  # Now delete the original files, and do a dump from the db.
  clear_files(test)
  stoner::stone_dump(test$con, "nevis-1", file.path(test$path, "meta"), FALSE)

  # Start a fresh db connection, (but don't delete the files we just dumped,
  # if the temporary path returned is the same as it was before...)

  test2 <- new_test(clear_files = FALSE)
  test2$path <- test$path

  # Only need disease and modelling group to already exist; the rest we'll
  # restore from the dump

  standard_disease(test2)
  standard_modelling_groups(test2)
  do_test(test2)

  # We expect the data in the db to be correct

  test_responsibilities(test2, resp)

})


