context("dump")

empty_dump <- function() {
  tmp <- file.path(tempdir(), "stoner_dump")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, showWarnings = FALSE)
  tmp
}

initialise_touchstones <- function(test) {
  standard_disease_touchstones(test)
  do_test(test)
  test
}

initialise_with_demography <- function(test) {
  standard_disease_touchstones(test)
  standard_demography(test)
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  do_test(test)
  test
}

# Edit this when responsibilities are done properly
initialise_with_fake_resps <- function(test) {
  standard_demography(test)
  create_touchstone_csv(test$path, "nevis", 1)
  create_touchstone_name_csv(test$path, "nevis")
  create_ts_dds(test$path, "nevis-1", "S1", "T1")
  create_disease_csv(test$path, c("flu", "piles"),
                                c("Elf flu", "Elf piles"), db = TRUE)

  create_scen_desc_csv(test$path, "pies", "campaign", "flu")
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

  for (ctry in c("AFG", "ZWE")) {
    DBI::dbExecute(test$con,
      "INSERT INTO burden_estimate_country_expectation
                   (burden_estimate_expectation, country)
            VALUES ($1, $2)", list(exp_id, ctry))


  }

  for (out in c("deaths", "cases")) {
    DBI::dbExecute(test$con,
      "INSERT INTO burden_estimate_outcome_expectation
                   (burden_estimate_expectation, outcome)
            VALUES ($1, $2)", list(exp_id, out))
  }

  test
}


test_dump_touchstone <- function(path) {
  f <- file.path(path, "touchstone.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "nevis-1")
  expect_equal(data$touchstone_name, "nevis")
  expect_equal(data$version, 1)
  expect_equal(data$description, "nevis (version 1)")
  expect_equal(data$status, "in-preparation")
  expect_equal(data$comment, "Comment nevis-1")
}

test_dump_touchstone_name <- function(path) {
  f <- file.path(path, "touchstone_name.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "nevis")
  expect_equal(data$description, "nevis description")
  expect_equal(data$comment, "nevis comment")
}

test_dump_touchstone_dds <- function(path) {
  f <- file.path(path, "touchstone_demographic_dataset.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
  expect_equal(data$touchstone, "nevis-1")
  expect_equal(data$demographic_source, "S1")
  expect_equal(data$demographic_statistic_type, "T1")
}

test_dump_db_demographic_source <- function(path) {
  f <- file.path(path, "db_demographic_source.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
  expect_equal(data$code, "S1")
  expect_equal(data$name, "Source 1")
}

test_dump_db_demographic_statistic_type <- function(path) {
  f <- file.path(path, "db_demographic_statistic_type.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
  expect_equal(data$code, "T1")
  expect_equal(data$name, "Type 1")
}

test_dump_touchstone_country <- function(path) {
  f <- file.path(path, "touchstone_country.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
  expect_equal(data$disease, "flu;piles")
  expect_equal(data$country, "AFG;ZWE")
  expect_equal(data$touchstone, "nevis-1")
}

test_dump_scenario_description <- function(path) {
  f <- file.path(path, "scenario_description.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "pies")
  expect_equal(data$description, "campaign")
  expect_equal(data$disease, "flu")
}

test_dump_responsibilities <- function(path) {
  f <- file.path(path, "responsibilities.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
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
  expect_equal(data$countries, "AFG;ZWE")
  expect_equal(data$outcomes, "cases;deaths")
  expect_equal(data$touchstone, "nevis-1")
}

test_dump_db_modelling_group <- function(path) {
  f <- file.path(path, "db_modelling_group.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
  expect_equal(data$id, "LAP-elf")
  expect_equal(data$institution, "Lapland Epi Centre")
  expect_equal(data$pi, "Santa")
  expect_equal(data$description, "Description")
  expect_true(is.na(data$comment))
  expect_true(is.na(data$replaced_by))
}

test_dump_db_disease <- function(path, id = "flu", name = "Elf") {
  f <- file.path(path, "db_disease.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 2)
  expect_equal(data$id, "flu")
  expect_equal(data$name, "Elf flu")
}

test_dump_db_disease_extra <- function(path) {
  f <- file.path(path, "db_disease.csv")
  expect_true(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 2)
  expect_equal(data$id, c("flu", "piles"))
  expect_equal(data$name, c("Elf flu", "Elf piles"))
}

test_that("Dump non-existent Touchstone", {
  test <- new_test()
  tmp <- empty_dump()
  expect_error(stoner::stone_dump(test$con, "turnip", tmp, TRUE),
               "Touchstone not found - ")
})

test_that("Dump works (touchstone only)", {
  test <- initialise_touchstones(new_test())
  tmp <- empty_dump()
  stoner::stone_dump(test$con, "nevis-1", tmp, TRUE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  expect_equal(length(list.files(tmp)), 2)
})

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

# Re-implement below when expections support is done

test_that("Dump works (fake expectations)", {
  test <- initialise_with_fake_resps(new_test())
  tmp <- empty_dump()
  stoner::stone_dump(test$con, "nevis-1", tmp, FALSE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  test_dump_touchstone_dds(tmp)
  test_dump_touchstone_country(tmp)
  test_dump_scenario_description(tmp)
  test_dump_responsibilities(tmp)
  expect_equal(length(list.files(tmp)), 6)
  stoner::stone_dump(test$con, "nevis-1", tmp, TRUE)
  test_dump_touchstone(tmp)
  test_dump_touchstone_name(tmp)
  test_dump_touchstone_dds(tmp)
  test_dump_touchstone_country(tmp)
  test_dump_scenario_description(tmp)
  test_dump_responsibilities(tmp)
  test_dump_db_demographic_source(tmp)
  test_dump_db_demographic_statistic_type(tmp)
  test_dump_db_modelling_group(tmp)
  test_dump_db_disease_extra(tmp)
  expect_equal(length(list.files(tmp)), 10)
})
