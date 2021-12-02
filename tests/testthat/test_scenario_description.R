context("scenario_description")

# Here, we do tests on:

# scenario_description.csv
# Cols: id, description, disease

test_that("A new scenario_description", {
  test <- new_test()
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type")
  create_scen_desc_csv(test$path, "hot_chocolate", "routine", "piles", "def")
  compare_csv(do_test(test), "scenario_description")
})

test_that("Invalid disease", {
  test <- new_test()
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type")
  create_scen_desc_csv(test$path, "pies", "campaign", "reindeer_flu", "def")
  expect_error(do_test(test),
               "Diseases in scenario_description are valid",
               class = "expectation_failure")
})

test_that("Identical scenario_description", {
  test <- new_test()
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type", db = TRUE)
  create_scen_desc_csv(test$path, "pies", "routine", "piles", "def", db = TRUE)
  create_scen_desc_csv(test$path, "pies", "routine", "piles", "def")
  compare_csv(do_test(test), "scenario_description")
})

test_that("Duplicate ids in scenario_description.csv", {
  test <- new_test()
  create_disease_csv(test$path, c("piles", "flu"),
                           c("Elf Piles", "Reindeer Flu"), db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type")
  create_scen_desc_csv(test$path, c("pies", "pies"), c("routine", "routine"),
                             c("piles", "flu"), "def")
  expect_error(do_test(test),
               "Duplicate ids in scenario_description.csv",
               class = "expectation_failure")
})

test_that("Edit scenario description for in-prep touchstone", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "campaign", "piles", "def", db = TRUE)
  create_scenario_csv(test$path, 1, "nevis-1", "hot_chocolate", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "enhanced", "piles", "def")
  compare_csv(do_test(test), "scenario_description")
})

test_that("Edit scenario description for open touchstone - no overide", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  mess_with(test$path, "db_touchstone.csv", "status", 1, "open")
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "campaign", "piles", "def", db = TRUE)
  create_scenario_csv(test$path, 1, "nevis-1", "hot_chocolate", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "enhanced", "piles", "def")

  expect_error(do_test(test,
           allow_overwrite_scenario_description = FALSE),
    "Can't edit scenario_description with id hot_chocolate. Already exists with open/finished touchstone versions.")
})

test_that("Edit scenario description for open touchstone - overide", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type", db = TRUE)
  mess_with(test$path, "db_touchstone.csv", "status", 1, "open")
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "campaign", "piles", "def", db = TRUE)
  create_scenario_csv(test$path, 1, "nevis-1", "hot_chocolate", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "enhanced", "piles", "def")

  compare_csv(do_test(test, allow_overwrite_scenario_description = TRUE),
              "scenario_description")
})

test_that("Edit scenario description for unused scenario description", {
  test <- new_test()
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "enhanced", "piles", "def", db = TRUE)
  create_scen_desc_csv(test$path, "hot_chocolate", "doubly-enhanced", "piles", "def")
  compare_csv(do_test(test), "scenario_description")
})

test_that("Invalid columns", {
  test <- new_test()
  create_disease_csv(test$path, "piles", "Elf Piles", db = TRUE)
  create_scen_type_csv(test$path, "def", "Default type")
  create_scen_desc_csv(test$path, "hot_chocolate", "enhanced", "piles", "def")
  mess_with(test$path, "scenario_description.csv", "severity", 1, "severe")
  expect_error(do_test(test),
               "Column names incorrect in scenario_description.csv")
})
