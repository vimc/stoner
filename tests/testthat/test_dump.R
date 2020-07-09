context("dump")

# Populate db with a touchstone and touchstone name.

initialise_touchstones <- function(test) {
  standard_disease_touchstones(test)
  do_test(test)
  test
}

# Population db with a touchstone, touchstone name, and
# a demographic dataset, source and type.

initialise_with_demography <- function(test) {
  standard_disease_touchstones(test)
  standard_demography(test)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  do_test(test)
  test
}

# Edit this when responsibilities are done properly
# Populate the db with touchstone, touchstone name,
# demographic dataset, source, type...
# and then the chain that leads to responsibility.

# For now, this is done with SQL as Stoner can't process
# incoming responsibilities.csv just yet.

initialise_with_fake_resps <- function(test, no_resps = FALSE,
                                       no_countries = FALSE,
                                       no_outcomes = FALSE) {
  standard_demography(test)
  create_touchstone_csv(test$path, "nevis", 1)
  create_touchstone_name_csv(test$path, "nevis")
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  create_disease_csv(test$path, c("flu", "piles"),
                                c("Elf flu", "Elf piles"), db = TRUE)
  create_scen_type_csv(test$path, "default", "Default Scenario Type")
  create_scen_desc_csv(test$path, "pies", "campaign", "flu", "default")
  create_ts_country_csv(test$path, "nevis-1", c("flu", "piles"), "AFG;ZWE")

  do_test(test)
  DBI::dbExecute(test$con, "
     INSERT INTO modelling_group
                 (id, institution, pi, description, comment, replaced_by)
          VALUES ('LAP-elf', 'Lapland Epi Centre', 'Santa', 'Description',
                  NULL, NULL)")

  rset <- DBI::dbGetQuery(test$con, "
     INSERT INTO responsibility_set
                 (modelling_group, touchstone, status)
          VALUES ('LAP-elf', 'nevis-1', 'incomplete') RETURNING id")$id

  if (no_resps) {
    return(test)
  }

  scen <- DBI::dbGetQuery(test$con, "
     INSERT INTO scenario
                 (touchstone, scenario_description, focal_coverage_set)
          VALUES ('nevis-1', 'pies', NULL) RETURNING id")$id

  exp_id <- DBI::dbGetQuery(test$con,
    "INSERT INTO burden_estimate_expectation
                 (year_min_inclusive, year_max_inclusive,
                 age_min_inclusive, age_max_inclusive,
                 cohort_min_inclusive, cohort_max_inclusive,
                 description, version)
          VALUES (2000, 2100, 0, 85, 1915, 2100, 'LAP-elf:pies:standard',
                 'nevis') RETURNING id")$id

  DBI::dbExecute(test$con,
    "INSERT INTO responsibility
                 (responsibility_set, scenario, current_burden_estimate_set,
                  current_stochastic_burden_estimate_set, is_open,
                  expectations)
          VALUES ($1, $2, NULL, NULL, TRUE, $3)",
    list(rset, scen, exp_id))

  if (!no_countries) {
    for (ctry in c("AFG", "ZWE")) {
      DBI::dbExecute(test$con,
        "INSERT INTO burden_estimate_country_expectation
                     (burden_estimate_expectation, country)
              VALUES ($1, $2)", list(exp_id, ctry))
    }
  }

  if (!no_outcomes) {
    for (out in c("deaths", "cases")) {
      DBI::dbExecute(test$con,
        "INSERT INTO burden_estimate_outcome_expectation
                     (burden_estimate_expectation, outcome)
              VALUES ($1, $2)", list(exp_id, out))
    }
  }

  test
}

# Test that a dumped touchstone.csv file matches the
# default csv written by helper-tests:standard_disease_touchstones

test_dump_touchstone <- function(path) {
  f <- file.path(path, "touchstone.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "nevis-1")
  expect_equal(data$touchstone_name, "nevis")
  expect_equal(data$version, 1)
  expect_equal(data$description, "nevis (version 1)")
  expect_equal(data$status, "in-preparation")
  expect_equal(data$comment, "Comment nevis-1")
}

# Test that a dumped touchstone_name.csv file matches the
# default csv written by helper-tests:standard_disease_touchstones

test_dump_touchstone_name <- function(path) {
  f <- file.path(path, "touchstone_name.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "nevis")
  expect_equal(data$description, "nevis description")
  expect_equal(data$comment, "nevis comment")
}

# Test that a dumped touchstone_demographic_dataset.csv file matches the
# default csv written by create_ts_dds, in intialise_with_demography (above)

test_dump_touchstone_dds <- function(path) {
  f <- file.path(path, "touchstone_demographic_dataset.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$touchstone, "nevis-1")
  expect_equal(data$demographic_source, "S1")
  expect_equal(data$demographic_statistic_type, "T1")
}

# Test that a dumped db_demographic_source.csv file matches the
# default csv written by helper-tests:standard_demography

test_dump_db_demographic_source <- function(path) {
  f <- file.path(path, "db_demographic_source.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$code, "S1")
  expect_equal(data$name, "Source 1")
}

# Test that a dumped db_demographic_statistic_type.csv file matches the
# default csv written by helper-tests:standard_demography

test_dump_db_demographic_statistic_type <- function(path) {
  f <- file.path(path, "db_demographic_statistic_type.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$code, "T1")
  expect_equal(data$name, "Type 1")
}

# Test that a dumped touchstone_country.csv file matches the
# fake data written using create_ts_country_csv above.

test_dump_touchstone_country <- function(path) {
  f <- file.path(path, "touchstone_country.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$disease, "flu;piles")
  expect_equal(data$country, "AFG;ZWE")
  expect_equal(data$touchstone, "nevis-1")
}

# Test that a dumped scenario_description.csv file matches the
# fake data written using create_scen_desc_csv in initialise_with_fake_resps
# above.

test_dump_scenario_type <- function(path) {
  f <- file.path(path, "scenario_type.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "default")
  expect_equal(data$name, "Default Scenario Type")
}

# Test that a dumped scenario_description.csv file matches the
# fake data written using create_scen_desc_csv in initialise_with_fake_resps
# above.

test_dump_scenario_description <- function(path) {
  f <- file.path(path, "scenario_description.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "pies")
  expect_equal(data$description, "campaign")
  expect_equal(data$disease, "flu")
}

# Test that a dumpedresponsibilities.csv file matches all the expectation
# details inserted in initialise_with_fake_resps

test_dump_responsibilities <- function(path, no_countries = FALSE,
                                       no_outcomes = FALSE) {
  f <- file.path(path, "responsibilities.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$modelling_group, "LAP-elf")
  expect_equal(data$disease, "flu")
  expect_equal(data$scenario, "pies")
  expect_equal(data$scenario_type, "standard")
  expect_equal(data$age_min_inclusive, 0)
  expect_equal(data$age_max_inclusive, 85)
  expect_equal(data$year_min_inclusive, 2000)
  expect_equal(data$year_max_inclusive, 2100)
  expect_equal(data$cohort_min_inclusive, 1915)
  expect_equal(data$cohort_max_inclusive, 2100)
  if (no_countries) {
    expect_true(is.na(data$countries))
  } else {
    expect_equal(data$countries, "AFG;ZWE")
  }
  if (no_outcomes) {
    expect_true(is.na(data$outcomes))
  } else {
    expect_equal(data$outcomes, "cases;deaths")
  }
  expect_equal(data$touchstone, "nevis-1")
}

# Test that a dumped db_modelling_group.csv file matches the fake
# modelling_group inserted in initialise_with_fake_resps

test_dump_db_modelling_group <- function(path) {
  f <- file.path(path, "db_modelling_group.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "LAP-elf")
  expect_equal(data$institution, "Lapland Epi Centre")
  expect_equal(data$pi, "Santa")
  expect_equal(data$description, "Description")
  expect_true(is.na(data$comment))
  expect_true(is.na(data$replaced_by))
}

# Test that a dumped db_disease.csv file matches the fake diseases
# inserted in initialise_with_fake_resps.

test_dump_db_disease <- function(path) {
  f <- file.path(path, "db_disease.csv")
  expect_true(file.exists(f))
  data <- read_csv(f)
  expect_equal(nrow(data), 2)
  expect_equal(data$id, c("flu", "piles"))
  expect_equal(data$name, c("Elf flu", "Elf piles"))
}

########################################################
# Proper tests now, which call the above...
#
# When the final argument of stone_dump is TRUE, then additional
# support files (prefixed db_) are written. This is not especially useful
# for users, except to indicate what diseases, modelling_groups and
# demographic_sources and types are depended up by the CSV files that are
# dumped.
#
# Typically they will already exist in a Montagu database, although in our
# stoner test database they might not, and the db_ csv files are used to
# pre-populate those fields.
#
# Below, where results should differ, I test with both TRUE and FALSE,
# ensuring I get the right number of files.

test_that("Dump non-existent Touchstone", {
  test <- new_test()
  tmp <- empty_dump()
  expect_error(stoner::stone_dump(test$con, "turnip", tmp, TRUE),
               "Touchstone not found - ")
})

# Suppose we have a touchstone which has no scenarios or responsibilites
# linked to it. Stoner should dump just touchstone and touchstone_name
# csvs, and nothing else.

test_that("Dump works (touchstone only)", {
  test <- initialise_touchstones(new_test())
  tmp <- empty_dump()
  stoner::stone_dump(test$con, "nevis-1", tmp, TRUE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  expect_equal(length(list.files(tmp)), 2)
})

# Now suppose we have a touchstone, and also a touchstone_demographic_dataset
# linked with that touchstone. Stoner should now write a CSV for
# touchstone, another for touchstone_name, and a third for
# touchstone_demographic_dataset. If we also ask for the "extras", it should
# give us the dependencies- db_demographic_source and
# db_demographic_statistic_type.

test_that("Dump works (touchstone + demography)", {
  test <- initialise_with_demography(new_test())
  tmp <- empty_dump()
  stoner::stone_dump(test$con, "nevis-1", tmp, FALSE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  test_dump_touchstone_dds(tmp)
  expect_equal(length(list.files(tmp)), 3)
  stoner::stone_dump(test$con, "nevis-1", tmp, TRUE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  test_dump_touchstone_dds(tmp)
  test_dump_db_demographic_source(tmp)
  test_dump_db_demographic_statistic_type(tmp)
  expect_equal(length(list.files(tmp)), 5)
})

############
# Re-implement below when expections support is done
############

# Suppose now that we have a touchstone, with demography, and a
# responsibility_set linking to that touchstone, but no
# responsibilities or expectations. This is a bit boring, but should
# still work, just without writing any responsibilities/expectations,
# or any scenarios.

test_that("Dump works (responsibility_set but no responsibilities)", {
  test <- initialise_with_fake_resps(new_test(), no_resps = TRUE)
  tmp <- empty_dump()
  stoner::stone_dump(test$con, "nevis-1", tmp, FALSE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  test_dump_touchstone_dds(tmp)
  test_dump_touchstone_country(tmp)
  expect_equal(length(list.files(tmp)), 4)
  stoner::stone_dump(test$con, "nevis-1", tmp, TRUE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  test_dump_touchstone_dds(tmp)
  test_dump_touchstone_country(tmp)
  test_dump_db_demographic_source(tmp)
  test_dump_db_demographic_statistic_type(tmp)
  test_dump_db_disease(tmp)
  expect_equal(length(list.files(tmp)), 7)
})

# The next four tests are identical except for different params
# passed to test_dump_responsibilities, so...

test_responsibilities_helper <- function(no_countries, no_outcomes) {
  test <- initialise_with_fake_resps(new_test(), no_resps = FALSE,
                                     no_countries = no_countries,
                                     no_outcomes = no_outcomes)
  tmp <- empty_dump()
  stoner::stone_dump(test$con, "nevis-1", tmp, FALSE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  test_dump_touchstone_dds(tmp)
  test_dump_touchstone_country(tmp)
  test_dump_scenario_type(tmp)
  test_dump_scenario_description(tmp)
  test_dump_responsibilities(tmp, no_countries, no_outcomes)
  expect_equal(length(list.files(tmp)), 7)
  stoner::stone_dump(test$con, "nevis-1", tmp, TRUE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  test_dump_touchstone_dds(tmp)
  test_dump_touchstone_country(tmp)
  test_dump_scenario_type(tmp)
  test_dump_scenario_description(tmp)
  test_dump_responsibilities(tmp, no_countries, no_outcomes)
  test_dump_db_demographic_source(tmp)
  test_dump_db_demographic_statistic_type(tmp)
  test_dump_db_modelling_group(tmp)
  test_dump_db_disease(tmp)
  expect_equal(length(list.files(tmp)), 11)
}

# Now test with responsibilities and expectations, but no
# burden_estimate countries or outcomes are set. This should work fine
# but with empty CSV fields.

test_that("Dump (responbilities, no outcomes or countries)", {
  test_responsibilities_helper(no_countries = TRUE, no_outcomes = TRUE)
})

# Now with just countries, no outcomes...

test_that("Dump (responbilities, no outcomes)", {
  test_responsibilities_helper(no_countries = FALSE, no_outcomes = TRUE)
})

# And with just outcomes, no countries...

test_that("Dump (responbilities, no countries)", {
  test_responsibilities_helper(no_countries = TRUE, no_outcomes = FALSE)
})

# Now test the full dump, in which we have a touchstone with
# responsibility_sets linked to it, responsibilities and expectations in
# each. responsibilities and expectations come together; I only dump
# expectations that are linked to a responsibility, in the touchstone
# we are dumping, and responsibility.expectations cannot be NA.

test_that("Complete Dump (fake expectations)", {
  test_responsibilities_helper(no_countries = FALSE, no_outcomes = FALSE)
})
