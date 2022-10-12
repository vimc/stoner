context("prune")

#####################################
# Test the filter generator function

test_that("Filter generator works", {
  expect_equal(stoner:::get_filters(NULL, NULL, NULL, NULL), "")
  expect_equal(get_filters("A", NULL, NULL, NULL), "WHERE scenario.touchstone IN ('A')")
  expect_equal(get_filters(c("A", "B"), NULL, NULL, NULL), "WHERE scenario.touchstone IN ('A','B')")
  expect_equal(get_filters(NULL, "A", NULL, NULL), "WHERE modelling_group IN ('A')")
  expect_equal(get_filters(NULL, c("A", "B"), NULL, NULL), "WHERE modelling_group IN ('A','B')")
  expect_equal(get_filters(NULL, NULL, "A", NULL), "WHERE disease IN ('A')")
  expect_equal(get_filters(NULL, NULL, c("A", "B"), NULL), "WHERE disease IN ('A','B')")
  expect_equal(get_filters(NULL, NULL, NULL, "A"), "WHERE scenario_description IN ('A')")
  expect_equal(get_filters(NULL, NULL, NULL, c("A", "B")), "WHERE scenario_description IN ('A','B')")
  expect_equal(get_filters("A", "D", NULL, NULL), "WHERE scenario.touchstone IN ('A') AND modelling_group IN ('D')")
  expect_equal(get_filters(NULL, c("A", "B"), c("C", "D"), NULL), "WHERE modelling_group IN ('A','B') AND disease IN ('C','D')")
  expect_equal(get_filters("Z", c("A", "B"), c("C", "D"), NULL), "WHERE scenario.touchstone IN ('Z') AND modelling_group IN ('A','B') AND disease IN ('C','D')")
  expect_equal(get_filters("Z", c("A", "B"), c("C", "D"), "Y"), "WHERE scenario.touchstone IN ('Z') AND modelling_group IN ('A','B') AND disease IN ('C','D') AND scenario_description IN ('Y')")
})

write_prune_csv <- function(test, ...) {
  df <- prune_csv(...)
  write.csv(df, file.path(test$path, "meta", "prune.csv"),
            row.names = FALSE)
}

prune_csv <- function(modelling_group, disease, scenario, touchstone) {
  data.frame(stringsAsFactors = FALSE,
             modelling_group = modelling_group,
             disease = disease,
             scenario = scenario,
             touchstone = touchstone)
}


#######################################################################
# Prune-stone behaviour.

prepare_test_prune <- function(test) {
  clear_test_resps(test)
  expec <- dummy_expectation(test)
  resp_set_a1 <- add_responsibility_set(test, "LAP-elf", "nevis-1")
  resp_set_a2 <- add_responsibility_set(test, "LAP-elf", "nevis-2")
  resp_set_b1 <- add_responsibility_set(test, "EBHQ-bunny", "nevis-1")
  resp_set_b2 <- add_responsibility_set(test, "EBHQ-bunny", "nevis-2")

  resp_a1_1 <- add_responsibility(test, "nevis-1", resp_set_a1, "hot_chocolate", expec)
  resp_a2_1 <- add_responsibility(test, "nevis-2", resp_set_a2, "hot_chocolate", expec)
  resp_b1_1 <- add_responsibility(test, "nevis-1", resp_set_b1, "pies", expec)
  resp_b2_1 <- add_responsibility(test, "nevis-2", resp_set_b2, "pies", expec)

  bes_a1_1 <- add_burden_estimate_set(test, resp_a1_1)
  bes_a2_1 <- add_burden_estimate_set(test, resp_a2_1)
  bes_b1_1 <- add_burden_estimate_set(test, resp_b1_1)
  bes_b2_1 <- add_burden_estimate_set(test, resp_b2_1)
  bes_a1_2 <- add_burden_estimate_set(test, resp_a1_1)
  bes_a2_2 <- add_burden_estimate_set(test, resp_a2_1)
  bes_b1_2 <- add_burden_estimate_set(test, resp_b1_1)
  bes_b2_2 <- add_burden_estimate_set(test, resp_b2_1)

  add_burden_estimates(test, pathetic_data(test), bes_a1_1, resp_a1_1)
  add_burden_estimates(test, pathetic_data(test), bes_a2_1, resp_a2_1)
  add_burden_estimates(test, pathetic_data(test), bes_b1_1, resp_b1_1)
  add_burden_estimates(test, pathetic_data(test), bes_b2_1, resp_b2_1)
  add_burden_estimates(test, pathetic_data(test), bes_a1_2, resp_a1_1)
  add_burden_estimates(test, pathetic_data(test), bes_a2_2, resp_a2_1)
  add_burden_estimates(test, pathetic_data(test), bes_b1_2, resp_b1_1)
  add_burden_estimates(test, pathetic_data(test), bes_b2_2, resp_b2_1)

  list(a11 = bes_a1_1, a12 = bes_a1_2,
       a21 = bes_a2_1, a22 = bes_a2_2,
       b11 = bes_b1_1, b12 = bes_b1_2,
       b21 = bes_b2_1, b22 = bes_b2_2)

}

test_that("Prune tests", {

  test <- init_for_ff_prune()

  ########################################################
  # Test with all wildcards. We expect
  # just the most recent 4 to remain

  write_prune_csv(test, "*", "*", "*", "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
    "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
    "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set

  expect_false(any(c(res$a11, res$a21, res$b11, res$b21) %in% bes))
  expect_true(all(c(res$a12, res$a22, res$b12, res$b22) %in% bes))
  expect_false(any(c(res$a11, res$a21, res$b11, res$b21) %in% be))
  expect_true(all(c(res$a12, res$a22, res$b12, res$b22) %in% be))

  #########################################
  # Test with a single modelling group

  write_prune_csv(test, "LAP-elf", "*", "*", "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
    "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
    "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set

  expect_false(any(c(res$a11, res$a21) %in% bes))
  expect_true(all(c(res$a12, res$a22, res$b11, res$b21, res$b12, res$b22) %in% bes))
  expect_false(any(c(res$a11, res$a21) %in% be))
  expect_true(all(c(res$a12, res$a22, res$b11, res$b21, res$b12, res$b22) %in% be))


  #########################################
  # Test with a modelling groups separately defined

  write_prune_csv(test, c("LAP-elf", "EBHQ-bunny"), "*", "*", "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
    "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
    "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set

  expect_false(any(c(res$a11, res$a21, res$b11, res$b21) %in% bes))
  expect_true(all(c(res$a12, res$a22, res$b12, res$b22) %in% bes))
  expect_false(any(c(res$a11, res$a21, res$b11, res$b21) %in% be))
  expect_true(all(c(res$a12, res$a22, res$b12, res$b22) %in% be))


  #########################################
  # Test modelling group not found

  write_prune_csv(test, "Spud", "*", "*", "*")
  res <- prepare_test_prune(test)
  expect_error(do_test(test), "modelling_group not found: Spud")

  ##############################################
  # Test nothing to do on valid modelling group

  write_prune_csv(test, "R-deer", "*", "*", "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
    "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
    "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set
  expect_true(all(unlist(res) %in% bes))
  expect_true(all(unlist(res) %in% be))

  ##############################################
  # Test limiting by disease

  write_prune_csv(test, "*", "flu", "*", "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
    "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
    "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set
  expect_false(any(c(res$a11, res$a21, res$b11, res$b21) %in% bes))
  expect_true(all(c(res$a12, res$a22, res$b12, res$b22) %in% bes))
  expect_false(any(c(res$a11, res$a21, res$b11, res$b21) %in% be))
  expect_true(all(c(res$a12, res$a22, res$b12, res$b22) %in% be))

  write_prune_csv(test, "*", "piles", "*", "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
    "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
    "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set
  expect_true(all(unlist(res) %in% bes))
  expect_true(all(unlist(res) %in% be))

  write_prune_csv(test, "*", c("piles", "sprout-fever"), "*", "*")
  res <- prepare_test_prune(test)
  expect_error(do_test(test), "disease not found: sprout-fever")

  ##############################################
  # Test limiting by touchstone

  write_prune_csv(test, "*", "*", "*", "nevis-1")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
                         "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
                        "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set
  expect_false(any(c(res$a11, res$b11) %in% bes))
  expect_true(all(c(res$a12, res$a21, res$a22, res$b12, res$b21, res$b22) %in% bes))
  expect_false(any(c(res$a11, res$b11) %in% be))
  expect_true(all(c(res$a12, res$a21, res$a22, res$b12, res$b21, res$b22) %in% be))

  write_prune_csv(test, "*", "*", "*", c("kili-1", "kili-2"))
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
                         "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
                        "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set
  expect_true(all(unlist(res) %in% bes))
  expect_true(all(unlist(res) %in% be))

  write_prune_csv(test, "*", "*", "*", c("potato-1", "potato-2"))
  res <- prepare_test_prune(test)
  expect_error(do_test(test), "touchstone not found: potato-1, potato-2")

  ##############################################
  # Test limiting by scenario

  write_prune_csv(test, "*", "*", "pies", "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
                         "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
                        "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set
  expect_false(any(c(res$b11, res$b21) %in% bes))
  expect_true(all(c(res$a11, res$a12, res$a21, res$a22, res$b12, res$b22) %in% bes))
  expect_false(any(c(res$b11, res$b21) %in% be))
  expect_true(all(c(res$a11, res$a12, res$a21, res$a22, res$b12, res$b22) %in% be))

  write_prune_csv(test, "*", "*", "mistletoe", "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
                         "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
                        "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set
  expect_true(all(unlist(res) %in% bes))
  expect_true(all(unlist(res) %in% be))

  write_prune_csv(test, "*", "*", "puddings", "*")
  res <- prepare_test_prune(test)
  expect_error(do_test(test), "scenario_description not found: puddings")

  ##############################################
  # Test a multi-line combo
  # We expect to prune nevis-1 from LAP-elf, and nevis-2 from EBHQ-bunny

  write_prune_csv(test, c("LAP-elf", "EBHQ-bunny"), "*", "*", c("nevis-1", "nevis-2"))
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
    "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
    "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set

  expect_false(any(c(res$a11, res$b21) %in% bes))
  expect_true(all(c(res$a12, res$a21, res$a22, res$b11, res$b12, res$b22) %in% bes))
  expect_false(any(c(res$a11, res$b21) %in% be))
  expect_true(all(c(res$a12, res$a21, res$a22, res$b11, res$b12, res$b22) %in% be))

  write_prune_csv(test, "*", c("flu", "piles"), c("pies", "hot_chocolate"), "*")
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
                         "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
                        "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set

  expect_false(any(c(res$a11, res$a21, res$b11, res$b21) %in% bes))
  expect_true(all(c(res$a12, res$a22, res$b12, res$b22) %in% bes))
  expect_false(any(c(res$a11, res$a21, res$b11, res$b21) %in% be))
  expect_true(all(c(res$a12, res$a22, res$b12, res$b22) %in% be))

})

test_that("Prune must be only CSV, if provided", {
  test <- init_for_ff_prune()
  write_prune_csv(test, "*", "*", "*", "*")
  write.csv(data.frame(touchstone = "nevis-1", demographic_source="S1",
                       demographic_statistic_type = "T1"),
            file.path(test$path, "meta", "touchstone_demographic_dataset.csv"),
            row.names=FALSE)

  expect_error(do_test(test),
             "prune.csv, if specified, must be the only csv")
})

test_that("Empty prune CSV", {
  test <- init_for_ff_prune()
  write.csv(data.frame(modelling_group = character(0),
                       touchstone = character(0), disease = character(0),
                       scenario = character(0)),
            file.path(test$path, "meta", "prune.csv"),
            row.names=FALSE)
  res <- prepare_test_prune(test)
  do_test(test)
  bes <- DBI::dbGetQuery(test$con,
    "SELECT id FROM burden_estimate_set")$id
  be <- DBI::dbGetQuery(test$con,
    "SELECT DISTINCT burden_estimate_set from burden_estimate")$burden_estimate_set
  expect_true(all(unlist(res) %in% bes))
  expect_true(all(unlist(res) %in% be))
})
