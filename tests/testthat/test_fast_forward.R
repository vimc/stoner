context("fast_forward")

# Here, we do tests on the fast_forward.csv file

# Cols: modelling_group, touchstone_from, touchstone_to, scenario
#
# where modelling_group and scenario can be * for wildcard
# or a semi-colon list of modelling_groups or scenario_descriptions

test_that("Empty ff csv extracts nothing", {

  # No CSV provided

  x <- extract_fast_forward(list(), NULL, NULL)
  expect_true(is.null(x))

  # CSV but no rows. How do we create a data frame with no rows?

  csv <- data.frame(modelling_group = "", touchstone_from = "",
                    touchstone_to = "", scenario = "")
  csv <- csv[csv$modelling_group %in% "Jethro Tull", ]

  x <- extract_fast_forward(list(fast_forward_csv = csv, NULL, NULL))
  expect_true(is.null(x))
})

write_ff_csv <- function(test, ...) {
  df <- ff_csv(...)
  write.csv(df, file.path(test$path, "meta", "fast_forward.csv"),
                          row.names = FALSE)
}

ff_csv <- function(modelling_group, scenario, touchstone_from, touchstone_to) {
  data.frame(stringsAsFactors = FALSE,
             modelling_group = modelling_group,
             scenario = scenario,
             touchstone_from = touchstone_from,
             touchstone_to = touchstone_to)
}

test_that("FF CSV cannot coexist with other CSVs", {
  test <- new_test()
  write_ff_csv(test, "*", "*", "t1", "t2")
  write.csv(ff_csv("*", "*", "t1", "t2"),
            file.path(test$path, "meta", "touchstone.csv"),
            row.names = FALSE)
  expect_error(do_test(test),
    "fast_forward.csv, if specified, must be the only csv")

})

test_that("FF CSV with incorrect modelling_group / scenario / touchstone", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  do_test(test)

  # Invalid touchstone

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("*", "*", "spud-1", "spud-2")), "", test$con),
    "Required touchstone\\(s) not found: spud-1, spud-2")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("*", "*", "kili-1", "spud-2")), "", test$con),
    "Required touchstone\\(s) not found: spud-2")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("*", "*", "spud-1", "kili-1")), "", test$con),
    "Required touchstone\\(s) not found: spud-1")

  # Invalid modelling_group

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-spud", "*", "kili-1", "nevis-1")), "", test$con),
    "Modelling group\\(s) not found: LAP-spud")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv(c("LAP-elf", "LAP-spud"), "*", "kili-1", "nevis-1")), "", test$con),
    "Modelling group\\(s) not found: LAP-spud")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf;LAP-spud", "*", "kili-1", "nevis-1")), "", test$con),
    "Modelling group\\(s) not found: LAP-spud")

  # Invalid scenario

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf", "hot_potato", "kili-1", "nevis-1")), "", test$con),
    "Scenario\\(s) not found: hot_potato")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf", "hot_potato;hot_chocolate", "kili-1", "nevis-1")), "", test$con),
    "Scenario\\(s) not found: hot_potato")

  expect_error(
    extract_fast_forward(list(
      fast_forward_csv = ff_csv("LAP-elf", c("hot_potato", "hot_chocolate"), "kili-1", "nevis-1")), "", test$con),
    "Scenario\\(s) not found: hot_potato")
})

test_that("Main FF functionality", {

  # There is a hefty amount of support needed for adding
  # burden estimate sets...

  # Set up some touchstones, groups and scenarios - but can't use
  # stoner to do it, as FF needs to be a standalone thing.

  test <- new_test()

  # Two touchstones, nevis-1 and kili-1 - we'll add 2 more versions

  standard_disease_touchstones(test)

  ts_file <- file.path(test$path, "meta", "db_touchstone.csv")
  write.csv(rbind(
    read.csv(ts_file),
    data.frame(
      id = c("nevis-2", "kili-2"),
      touchstone_name = c("nevis", "kili"),
      version = c(2, 2),
      description = c("nevis (version 2)", "kili (version 2)"),
      status = c("in-preparation", "in-preparation"),
      comment = c("Nevis 2", "Kili 2"))), ts_file, row.names = FALSE)

  # Three groups. LAP-elf, EBHQ-bunny, R-deer
  standard_modelling_groups(test)

  # Four scenario_descriptions: hot_chocolate, pies, mistletoe, holly
  standard_responsibility_support(test)

  # One built in scenario, and we'll add some more
  # New scenarios for the new touchstones

  sc_file <- file.path(test$path, "meta", "db_scenario.csv")
  write.csv(rbind(
    read.csv(sc_file),
    data.frame(
      id = 2:4,
      touchstone = c("nevis-2", "kili-1", "kili-2"),
      scenario_description = c("hot_chocolate", "pies", "pies"),
      focal_coverage_set = c(NA, NA, NA))), sc_file, row.names = FALSE)

  # Now we can import all the db_ files, then remove them,
  # so following import will be just the FF.csv file

  do_test(test)
  dbfiles <- list.files(file.path(test$path, "meta"))
  unlink(file.path(test$path, "meta", dbfiles))

  # Also need to setup a model (with a null version)

  DBI::dbExecute(test$con, "INSERT INTO model
    (id, modelling_group, description, citation, is_current,
     current_version, disease, gender_specific, gender) VALUES
     ($1, $2, $3, $4, $5, $6, $7, $8, $9)",
    list("elf-suite", "LAP-elf", "Description", "Citation", TRUE,
         NA, "flu", FALSE, NA))

  # Add model version

  model_version <- DBI::dbGetQuery(test$con, "INSERT INTO model_version
    (model, version, note, fingerprint, code, is_dynamic) VALUES
    ($1, $2, $3, $4, $5, $6) RETURNING id",
    list("elf-suite", 1, "", NA, NA, FALSE))$id

  DBI::dbExecute(test$con, "
    UPDATE model SET current_version = $1
     WHERE id = 'elf-suite'", model_version)

  # Add upload user

  DBI::dbExecute(test$con, "INSERT INTO app_user
    (username, name ,email, password_hash) VALUES
    ($1, $2, $3, $4)", list("Elf", "Elf", "elf@lapland.edu", ""))

  WLF <- DBI::dbGetQuery(test$con, "
    SELECT nid FROM country WHERE id='WLF'")$nid

  bo_cases <- DBI::dbGetQuery(test$con, "
    SELECT id FROM burden_outcome
     WHERE code='cases'")$id

  # Dummy expectations (which will, naughtily, share between everyone)

  expec <- DBI::dbGetQuery(test$con, "
    INSERT INTO burden_estimate_expectation
                (year_min_inclusive, year_max_inclusive,
                 age_min_inclusive, age_max_inclusive,
                 cohort_min_inclusive, cohort_max_inclusive,
                 description, version) VALUES
                 ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING id",
   list(2000, 2001, 0, 1, 2000, 2000, 'Test', 'Test Version'))

  pathetic_data <- data.frame(burden_estimate_set = rep(NA, 4),
                              country = rep(WLF, 4),
                              year = c(2000,2001,2000,2001),
                              age = c(0, 0, 1, 1),
                              burden_outcome = rep(bo_cases, 4),
                              value = sample(4),
                              model_run = rep(NA, 4))



  # The rest will all be do with with responsibility_set
  # and responsibility, so...

  clear <- function(con) {
    DBI::dbExecute(con, "UPDATE responsibility SET current_burden_estimate_set = NULL")
    DBI::dbExecute(con, "DELETE FROM burden_estimate")
    DBI::dbExecute(con, "DELETE FROM burden_estimate_set")
    DBI::dbExecute(con, "DELETE FROM responsibility")
    DBI::dbExecute(con, "DELETE FROM responsibility_set")
  }

  add_responsibility_set <- function(con, group, touchstone) {
    DBI::dbGetQuery(con, "
      INSERT INTO responsibility_set (modelling_group, touchstone, status)
             VALUES ($1, $2, $3) RETURNING id",
      list(group, touchstone, "incomplete"))$id
  }

  add_responsibility <- function(con, touchstone, responsibility_set,
                                 scenario_description) {

    scenario_id <- DBI::dbGetQuery(con, "
      SELECT id FROM scenario
       WHERE scenario_description = $1
         AND touchstone = $2",
      list(scenario_description, touchstone))$id

    DBI::dbGetQuery(con, "
      INSERT INTO responsibility (responsibility_set, scenario)
           VALUES ($1, $2) RETURNING id",
        list(responsibility_set, scenario_id))$id
  }

  add_burden_estimate_set <- function(con, responsibility) {
    DBI::dbGetQuery(con, "
      INSERT INTO burden_estimate_set (responsibility, model_version,
                                       run_info, interpolated, uploaded_by)
           VALUES ($1, '1', 'dummy', FALSE, 'Elf') RETURNING id",
                    list(responsibility))$id
  }

  add_burden_estimates <- function(con, df, bes, resp) {
    df$burden_estimate_set <- bes
    DBI::dbAppendTable(con, "burden_estimate", df)
    DBI::dbExecute(con, "UPDATE responsibility
                            SET current_burden_estimate_set = $1
                          WHERE id = $2", list(bes, resp))
  }

  # And finally we are ready to do proper tests.
  ###################################################################
  # 1. Single group.
  # 2. Responsibility_set (and hence responsibility) doesn't exist
  # 3. Single scenario
  # 4. Model group and scenario named explicitly in file
  ###################################################################

  clear(test$con)
  resp_set <- add_responsibility_set(test$con, "LAP-elf", "nevis-1")
  resp <- add_responsibility(test$con, "nevis-1", resp_set, "hot_chocolate")
  bes <- add_burden_estimate_set(test$con, resp)
  add_burden_estimates(test$con, pathetic_data, bes, resp)
  write_ff_csv(test, "LAP-elf", "hot_chocolate", "nevis-1", "nevis-2")

  # To be sure, the 2nd responsibility_set doesn't exist.

  get_responsibility_set <- function(modelling_group, touchstone) {
    DBI::dbGetQuery(test$con, "
    SELECT id FROM responsibility_set
     WHERE modelling_group = $1
       AND touchstone = $2", list(modelling_group, touchstone))$id
  }

  expect_equal(0, length(get_responsibility_set("LAP-Elf", "nevis-2")))

  # Run the FF import.

  do_test(test)

  # We expect:
  #   1. a new responsibility_set,

  new_rset <- get_responsibility_set("LAP-elf", "nevis-2")
  expect_equal(1, length(new_rset))

  #   2. a new responsibility for that set

  get_responsibilities <- function(responsibility = NA, responsibility_set = NA) {
    if (!is.na(responsibility_set)) {
      DBI::dbGetQuery(test$con, "
        SELECT * FROM responsibility WHERE responsibility_set IN ($1)",
          paste(responsibility_set, collapse = ","))
    } else {
      DBI::dbGetQuery(test$con, "
        SELECT * FROM responsibility WHERE id IN ($1)",
          paste(responsibility, collapse = ","))
    }
  }

  new_resps <- get_responsibilities(responsibility_set = new_rset)
  expect_equal(1, nrow(new_resps))

  #   3. That resp.  has a current_burden_estimate_set matching earlier bes
  expect_equal(new_resps$current_burden_estimate_set, bes)

  #   4. Old responsibility is still there, with NA for
  #      current_burden_estimate_set

  old_resp <- get_responsibilities(responsibility = resp)
  expect_true(is.na(old_resp$current_burden_estimate_set))

 })
