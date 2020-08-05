context("stochastic_upload")

# Lots of tests for stochastic parameters...
# (Mostly for code coverage...!)

#stone_stochastic_process <- function(
#  con, modelling_group, disease,
#  touchstone, scenarios, path, files,
#  cert, index_start = NA, index_end = NA)


test_that("Bad arguments", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resps <- NULL
  for (scenario in c("pies", "hot_chocolate", "holly")) {
    df <- default_responsibility()
    df$scenario <- scenario
    resps <- rbind(resps, df)
  }
  df <- default_responsibility()
  df$modelling_group <- "EBHQ-bunny"
  df$scenario <- "mistletoe"
  df$touchstone <- "nevis-1"
  df$disease <- "piles"
  resps <- rbind(resps, df)

  create_responsibilities(test, resps)
  do_test(test)

  expect_error(stoner::stone_stochastic_process(test$con,
    "Rudolph", "", "", "", "", "", "", NA, NA),
    "Unknown modelling group:")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "Plague", "", "", "", "", "", NA, NA),
    "Unknown disease:")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "snowdon", "", "", "", "", NA, NA),
    "Unknown touchstone:")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "potato", "", "", "", NA, NA),
    "scenario potato not found in touchstone nevis-1")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "piles", "nevis-1", "hot_chocolate", "", "", "", NA, NA),
    "scenario_description hot_chocolate not valid for disease piles")

  expect_error(stoner::stone_stochastic_process(test$con,
    "R-deer", "flu", "nevis-1", "hot_chocolate", "", "", "", NA, NA),
    "No responsibility_set for group R-deer in touchstone nevis-1")

  expect_error(stoner::stone_stochastic_process(test$con,
    "EBHQ-bunny", "flu", "nevis-1", "hot_chocolate", "", "", "", NA, NA),
    paste("No responsibility for group EBHQ-bunny,",
          "scenario hot_chocolate, touchstone nevis-1"))

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", file.path(test$path, "potato"),
    "", "", NA, NA),
    "Input path not found:")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    c("file1", "file2"), "", NA, NA),
    "Incorrect files param - length should be 1 or 3")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", c(NA, NA), NA),
    "Incorrect index_start - can be NA, or length 1 or 3")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", NA, c(NA, NA)),
    "Incorrect index_end - can be NA, or length 1 or 3")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", c(NA, NA, NA), c(NA, NA, "bob")),
    "index_end must be all NA or integers")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", c(NA, NA, "gladys"), c(NA, NA, NA)),
    "index_start must be all NA or integers")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", c(NA, NA, 1), 2),
    "Mismatches of NA between index_start and index_end")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    c("f", "f:index", "f"), "", c(1, NA, 1), c(2, NA, 2)),
    "Mismatch between NA in index_start, and :index placeholder in files")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", test$path, "non_exist:index.xz",
    "", 1, 1),
    "File not found: (.*)non_exist1.xz")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", test$path, "non_exist:index.xz",
    "", 1, 1, "", c("deaths", "deaths")),
    "Duplicated outcome in deaths")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", test$path, "non_exist:index.xz",
    "", 1, 1, "", "deaths", "cases", "piles_dalys"),
    "Outcomes not found, dalys \\('piles_dalys'\\)")

  expect_error(stoner::stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", test$path, "non_exist:index.xz",
    "", 1, 1, "", "deaths", "cases", "dalys", TRUE),
    "Must have index_start and index_end as 1..200 to imply run_id")

})



