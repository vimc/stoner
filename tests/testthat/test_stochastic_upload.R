context("stochastic_upload")

# Main stochastic upload is tested from test_stochastic_process
# In here we'll just deal with the arguments.

test_that("Bad arguments", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  do_test(test)

  expect_error(stoner::stone_stochastic_upload(
    file.path(tempfile(), "non_existent_file.csv")),
      "file(.*)exists(.*) is not TRUE")

  new_file <- tempfile(fileext = ".csv")
  write.csv(x = mtcars, file = new_file)

  expect_error(stoner::stone_stochastic_upload(new_file, test$con, test$con,
    "Rudolph"), "Unknown modelling group: Rudolph")

  expect_error(stoner::stone_stochastic_upload(new_file, test$con, test$con,
    "LAP-elf", "elk-fever"), "Unknown disease:")

  expect_error(stoner::stone_stochastic_upload(new_file, test$con, test$con,
    "LAP-elf", "flu", "ivinghoe-beacon-1"), "Unknown touchstone:")

  expect_error(stoner::stone_stochastic_upload(new_file, test$con, test$con,
    "LAP-elf", "flu", "nevis-1", FALSE, FALSE, FALSE),
    "stochastic_file database table not found")

  expect_error(stoner::stone_stochastic_upload(new_file, test$con, test$con,
    "LAP-elf", "flu", "nevis-1", FALSE, FALSE, TRUE),
     "Columns not as expected")
})
