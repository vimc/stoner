context("scenario_description")

# Here, we do tests on:

# scenario_description.csv
# Cols: id, description, disease

create_disease_csv <- function(path, ids, names, db = TRUE) {
  filename <- "disease.csv"
  if (db) filename <- paste0("db_", filename)

  stopifnot(length(ids) == length(names))
  stopifnot(length(names) == length(diseases))

  write.csv(data_frame(
    id = ids, name = names),
      file.path(path, "meta", filename), row.names = FALSE)
}

create_scenario_csv <- function(path, ids, touchstones, sds, db = FALSE) {
  filename <- "scenario.csv"
  if (db) filename <- pasrte0("db_", filename)

  stopifnot(length(ids) == length(touchstones))
  stopifnot(length(sds) == length(touchstones))

  write.csv(data_frame(
    id = ids, touchstone = touchstones,
    scenario_description = sds, focal_coverage_set = NA),
      file.path(path, "meta", filename), row.names = FALSE)
}

create_scen_desc_csv <- function(path, ids, descs, diseases, db = FALSE) {
  filename <- "scenario_description.csv"
  if (db) filename <- pasrte0("db_", filename)

  stopifnot(length(ids) == length(descs))
  stopifnot(length(ids) == length(diseases))

  write.csv(data_frame(
    id = ids, description = descs, disease = diseases),
    file.path(path, "meta", filename), row.names = FALSE)
}

test_that("A new scenario_description", {
  path <- empty_test_dir()
  create_disease_csv(path, "piles", "Elf Piles", db = TRUE)
  create_scen_desc_csv(path, "hot_chocolate", "routine", "piles")
  compare_csv(do_test(path), "scenario_description")
})

test_that("Invalid disease", {
  path <- empty_test_dir()
  create_disease_csv(path, "piles", "Elf Piles", db = TRUE)
  create_scen_desc_csv(path, "pies", "campaign", "reindeer_flu")
  expect_error(do_test(path),
               "Diseases in scenario_description are valid isn't true",
               class = "expectation_failure")
})

test_that("Identical scenario_description", {
  path <- empty_test_dir()
  create_disease_csv(path, "piles", "Elf Piles", db = TRUE)
  create_scen_desc_csv(path, "pies", "routine", "piles", db = TRUE)
  create_scen_desc_csv(path, "pies", "routine", "piles")
  compare_csv(do_test(path), "scenario_description")
})

test_that("Duplicate ids in scenario_description.csv", {
  path <- empty_test_dir()
  create_disease_csv(path, c("piles", "flu"),
                           c("Elf Piles", "Reindeer Flu"), db = TRUE)
  create_scen_desc_csv(path, "pies", "routine", "piles")
  create_scen_desc_csv(path, "pies", "routine", "flu")

  expect_error(do_test(path),
               "Duplicate ids in scenario_description.csv isn't false.",
               class = "expectation_failure")
})

test_that("Edit scenario description for in-prep touchstone", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  create_disease_csv(path, "piles", "Elf Piles", db = TRUE)
  create_scen_des_csv(path, "hot_chocolate", "campaign", "piles", db = TRUE)
  create_scenario_csv(path, 1, "nevis-1", "hot_chocolate", db = TRUE)
  create_scen_desc_csv(path, "hot_chocolate", "enhanced", "piles")
  compare_csv(do_test(path), "scenario_description")
})

test_that("Edit scenario description for open touchstone", {
  path <- empty_test_dir()
  create_touchstone_name_csv(path, "nevis", db = TRUE)
  create_touchstone_csv(path, "nevis", 1, db = TRUE)
  mess_with(path, "db_touchstone.csv", "status", 1, "open")
  create_disease_csv(path, "piles", "Elf Piles", db = TRUE)
  create_scen_desc_csv(path, "hot_chocolate", "campaign", "piles", db = TRUE)
  create_scenario_csv(path, 1, "nevis-1", "hot_chocolate", db = TRUE)
  create_scen_desc_csv(path, "hot_chocolate", "enhanced", "piles")

  expect_error(do_test(path,
           allow_overwrite_scenario_description = FALSE),
    "Can't edit scenario_description with id hot_chocolate. Already exists with open/finished touchstone versions.")
})

test_that("Edit scenario description for unused scenario description", {
  path <- empty_test_dir()
  create_disease_csv(path, "piles", "Elf Piles", db = TRUE)
  create_scenario_csv(path, 1, "nevis-1", "hot_chocolate", db = TRUE)
  create_scen_desc_csv(path, "hot_chocolate", "enhanced", "piles")
  compare_csv(do_test(path), "scenario_description")
})

test_that("Invalid columns", {
  path <- empty_test_dir()
  create_disease_csv(path, "piles", "Elf Piles", db = TRUE)
  create_scen_desc_csv(path, "hot_chocolate", "enhanced", "piles")
  mess_with(path, "scenario_description.csv", "severity", 1, "severe")
  expect_error(do_test(path),
               "Column names correct in scenario_description.csv isn't true",
               class = "expectation_failure")
})
