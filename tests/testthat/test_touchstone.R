context("touchstone")

# Here, we do tests on  these two CSV files...

# touchstone.csv
# Cols: id, touchstone_name, version, description, status, comment

# touchstone_name.csv
# Cols: id, description, comment

test_that("Empty import should succeed trivially", {
  test <- new_test()
  res <- do_test(test)
  expect_equal(length(res$t), 0)
})

test_that("A new touchstone and name", {
  test <- new_test()
  create_touchstone_csv(test$path, "nevis", 1)
  create_touchstone_name_csv(test$path, "nevis")
  compare_csv(do_test(test), c("touchstone", "touchstone_name"))

})

test_that("Two new touchstones and names", {
  test <- new_test()
  create_touchstone_csv(test$path, c("nevis", "kilimanjaro"), c(1, 1))
  create_touchstone_name_csv(test$path, c("nevis", "kilimanjaro"))
  compare_csv(do_test(test), c("touchstone", "touchstone_name"))
})

test_that("Two new touchstones, one touchstone name", {
  test <- new_test()
  create_touchstone_csv(test$path, c("nevis", "nevis"), c(1, 2))
  create_touchstone_name_csv(test$path, "nevis")
  compare_csv(do_test(test), c("touchstone", "touchstone_name"))
})

test_that("New touchstone, touchstone name in db, not csv", {
  test <- new_test()
  create_touchstone_csv(test$path, "kilimanjaro", 1)
  create_touchstone_name_csv(test$path, "kilimanjaro", db = TRUE)
  compare_csv(do_test(test), "touchstone")
})

test_that("Update existing touchstone_name (no refs)", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "kilimanjaro", db = TRUE)
  create_touchstone_name_csv(test$path, "kilimanjaro",
                             descriptions = "Updated Description")
  compare_csv(do_test(test), "touchstone_name")
})

test_that("Update existing touchstone_name (in prep)", {
  test <- new_test()
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_name_csv(test$path, "nevis",
                             description = "Updated Description")
  compare_csv(do_test(test), "touchstone_name")
})

test_that("Update existing touchstone details (in prep)", {
  test <- new_test()
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, comments = "Extra comment")
  res <- do_test(test)
  compare_csv(res, "touchstone")
})

test_that("Update and add touchstone and touchstone_name (in prep)", {
  test <- new_test()
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_name_csv(test$path, c("nevis", "kilimanjaro"),
      descriptions = c("Updated description", "new description"),
      comments = c("Updated comment", "first comment"))
  create_touchstone_csv(test$path, c("nevis", "kilimanjaro"), c(1, 1),
      comments = c("Updated comment", "first comment"))

  compare_csv(do_test(test), c("touchstone", "touchstone_name"))
})

test_that("Exact match", {
  test <- new_test()
  create_touchstone_csv(test$path, "nevis", 1)
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(test$path, "nevis")
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  compare_csv(do_test(test), c("touchstone", "touchstone_name"))
})

## Tests that should fail:

test_that("Add touchstone, name not found", {
  test <- new_test()
  create_touchstone_csv(test$path, "nevis", 1)
  expect_error(do_test(test),
    "All touchstone.touchstone_name are known",
    class = "expectation_failure")
})

test_that("Add touchstone, bad version", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 2)
  mess_with(test$path, "touchstone.csv", "description", 1, "nevis (version 1)")
  expect_error(do_test(test),
    "All touchstone.description are formatted correctly",
               class = "expectation_failure")
})

test_that("Add touchstone, bad id format", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis")
  create_touchstone_csv(test$path, "nevis", 1)
  mess_with(test$path, "touchstone.csv", "id", 1, "nevis#1")
  expect_error(do_test(test),
               "All touchstone.id are touchstone_name-version",
               class = "expectation_failure")
})

test_that("Edit touchstone name - not in-preparation", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_touchstone_name_csv(test$path, "nevis", descriptions = "Edited")
  mess_with(test$path, "db_touchstone.csv", "status", 1, "finished")
  expect_error(do_test(test),
    paste0("Can't edit touchstone_name id (.*). ",
           "Already exists with open/finished touchstone versions"),
    class = "simpleError")
})

test_that("Edit touchstone - not in-preparation", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, comments = "More comments")
  mess_with(test$path, "db_touchstone.csv", "status", 1, "finished")
  expect_error(do_test(test),
    "Can't edit touchstone id (.*). Already exists with open/finished status.",
    class = "simpleError")
})

test_that("touchstone CSV invalid", {
  test <- new_test()
  create_touchstone_csv(test$path, "nevis", 1)
  mess_with(test$path, "touchstone.csv", "haggis", 1, "yummy")
  expect_error(do_test(test),
               "Expected Correct columns in touchstone.csv to equal (.*)",
               class = "expectation_failure")
})

test_that("touchstone_name CSV invalid", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", 1)
  mess_with(test$path, "touchstone_name.csv", "pie_balm", 1, "clogg_banting")
  expect_error(do_test(test),
               "Expected Correct columns in touchstone_name.csv to equal (.*)",
               class = "expectation_failure")
})

test_that("Duplicate touchstone id in csv", {
  test <- new_test()
  create_touchstone_csv(test$path, c("nevis", "nevis"), c(1, 1))
  create_touchstone_name_csv(test$path, "nevis")
  expect_error(do_test(test),
               "No duplicate ids in touchstone.csv",
               class = "expectation_failure")
})

test_that("Duplicate touchstone_name in csv", {
  test <- new_test()
  create_touchstone_name_csv(test$path, c("nevis", "nevis"))
  create_touchstone_csv(test$path, "nevis", 1)
  expect_error(do_test(test),
               "No duplicate ids in touchstone_name.csv",
               class = "expectation_failure")
})

test_that("Add touchstone, bad status", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis")
  create_touchstone_csv(test$path, "nevis", 1)
  mess_with(test$path, "touchstone.csv", "status", 1, "half-baked")
  expect_error(do_test(test),
               "All touchstone.status are valid",
               class = "expectation_failure")
})

test_that("Edit touchstone, bad status", {
  test <- new_test()
  create_touchstone_name_csv(test$path, "nevis", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1)
  mess_with(test$path, "touchstone.csv", "status", 1, "gooey")
  expect_error(do_test(test),
               "All touchstone.status are valid",
               class = "expectation_failure")
})

