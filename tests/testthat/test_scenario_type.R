context("scenario_type")

# Here, we do tests on:

# scenario_type.csv
# Cols: id, name

test_that("A new scenario_type", {
  test <- new_test()
  create_scen_type_csv(test$path, "new_type", "A new scenario type")
  compare_csv(do_test(test), "scenario_type")
})

test_that("Identical scenario_type", {
  test <- new_test()
  create_scen_type_csv(test$path, "new_type", "A new scenario type", db = TRUE)
  create_scen_type_csv(test$path, "new_type", "A new scenario type")
  compare_csv(do_test(test), "scenario_type")
})

test_that("Duplicate ids in scenario_type.csv", {
  test <- new_test()
  create_scen_type_csv(test$path, c("type", "type"), c("The Type", "The Type"))
  expect_error(do_test(test),
               "Duplicate ids in scenario_type.csv",
               class = "expectation_failure")
})

edit_scenario_type_helper <- function(status = NULL) {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  if (!is.null(status)) {
    mess_with(test$path, "db_touchstone.csv", "status", 1, "open")
  }
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "type1", "Original Type", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "campaign", "piles",
                                  "type1", db = TRUE)
  create_scenario_csv(test$path, 1, "nevis-1", "hot_chocolate", db = TRUE)
  create_scen_type_csv(test$path, "type1", "Update The Type")
  test
}

test_that("Edit scenario type for in-prep touchstone", {
  compare_csv(do_test(edit_scenario_type_helper()), "scenario_type")
})

test_that("Edit scenario type for open touchstone - no overide", {
  expect_error(do_test(edit_scenario_type_helper("open"),
                       allow_overwrite_scenario_type = FALSE),
    "Can't edit scenario_type with id type1. Already exists with open/finished touchstone versions.")
})

test_that("Edit scenario type for open touchstone - overide", {
  compare_csv(do_test(edit_scenario_type_helper("open"),
                      allow_overwrite_scenario_type = TRUE),
                      "scenario_type")
})

test_that("Edit scenario type for unused scenario type", {
  test <- new_test()
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "type1", "Original type", db = TRUE)
  create_scen_type_csv(test$path, "type1", "Updated type")
  compare_csv(do_test(test), "scenario_type")
})

test_that("Invalid columns", {
  test <- new_test()
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "type1", "Original type")
  mess_with(test$path, "scenario_type.csv", "extra", 1, "extra")
  expect_error(do_test(test),
               "Column names incorrect in scenario_type.csv",
               class = "expectation_failure")
})
