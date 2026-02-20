context("stochastic_upload")

# Main stochastic upload is tested from test_stochastic_process
# In here we'll just deal with the arguments.

test_that("Bad arguments", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  do_test(test)

  expect_error(stoner::stone_stochastic_upload(
    file.path(tempfile(), "non_existent_file.pq")),
      "file(.*)exists(.*)")

  new_file <- tempfile(fileext = ".pq")
  arrow::save_parquet(mtcars, new_file)

  expect_error(stone_stochastic_upload(new_file, test$con, test$con,
    "Rudolph"), "Unknown modelling group: Rudolph")

  expect_error(stone_stochastic_upload(new_file, test$con, test$con,
    "LAP-elf", "elk-fever"), "Unknown disease:")

  expect_error(stone_stochastic_upload(new_file, test$con, test$con,
    "LAP-elf", "flu", "ivinghoe-beacon-1"), "Unknown touchstone:")

  expect_error(stone_stochastic_upload(new_file, test$con, test$con,
    "LAP-elf", "flu", "nevis-1", FALSE, FALSE, TRUE),
     "Columns in pq file not as expected")
})

test_that("stochastic_upload with csv file returns useful error", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  do_test(test)

  new_file <- tempfile(fileext = ".csv")
  write.csv(x = mtcars, file = new_file)

  expect_error(
    stone_stochastic_upload(new_file, test$con, test$con,
                            "LAP-elf", "flu", "nevis-1", FALSE, FALSE, TRUE),
    "Columns in csv file not as expected")
})

test_that("stochastic_upload errors if file not csv or pq", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  do_test(test)

  new_file <- tempfile(fileext = ".rds")
  saveRDS(mtcars, file = new_file)

  expect_error(
    stone_stochastic_upload(new_file, test$con, test$con,
                            "LAP-elf", "flu", "nevis-1", FALSE, FALSE, TRUE),
    "Can only read csv or pq format stochastic data, got rds")
})
