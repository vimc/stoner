context("dalys")

test_that("Bad arguments", {
  test <- new_test()

  expect_error(stoner_dalys_for_db(FALSE, FALSE),
    "'con' must be a PqConnection object")

  expect_error(stoner_dalys_for_db(test$con, FALSE),
    "dalys_params must be a data.frame")

  expect_error(stoner_dalys_for_db(test$con, mtcars),
               "dalys_params needs columns")

  dalys_params <- data_frame(outcome = "a", proportion = 1, average_duration = 1, disability_weight = 1)
  expect_error(stoner_dalys_for_db(test$con, dalys_params),
               "No burden_estimate_id given")

  expect_error(stoner_dalys_for_db(test$con, dalys_params, "mg", "d", "t", "s", 123),
    "Provide either burden_estimate_id, or ")

  expect_error(stoner_dalys_for_db(test$con, dalys_params, burden_estimate_set_id = 1,
                                   life_table = 1),
               "life_table \\(if specified) must be data")

  expect_error(stoner_dalys_for_db(test$con, dalys_params, burden_estimate_set_id = 1,
                                   life_table = mtcars),
               "life_table \\(if specified) must have columns")

})
