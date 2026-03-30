context("stochastic_graphs")

# Not a great amount of testing we can do here, without analysing
# the plot somehow.

test_that("stochastic_graph data transforms", {

  base <- tempdir()
  touchstone <- "t"
  disease <- "d"
  group <- "elf"
  scenario <- "opt"
  country <- "LAP"
  folder <- file.path(base, touchstone, paste0(disease, "_", group))
  filename <- paste0(group,"_", scenario, "_", country, ".pq")
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  data <- data.frame(year = rep(2000:2004, each = 5),
                     age = rep(10:14, 5), run_id = 1)
  data$deaths <- seq_len(nrow(data))
  orig <- data
  for (i in 2:5) {
    d <- orig
    d$run_id <- i
    d$deaths <- d$deaths * i
    data <- rbind(data, d)
  }
  f <- file.path(folder, filename)
  if (file.exists(f)) file.remove(f)
  arrow::write_parquet(data, file.path(folder, filename))

  # Aggregate all ages, not by cohort. Should have 1 point per year.

  res <- prepare_graph_data(base, touchstone, disease, group, country,
                            scenario, "deaths", NULL, FALSE)

  expect_equal(nrow(res), 25)    # 5 runs, 5 years
  expect_equal(res$deaths[res$run_id == 1 & res$year == 2002],
               sum(data$deaths[data$year == 2002 & data$run_id == 1]))

  # Select ages

  res <- prepare_graph_data(base, touchstone, disease, group, country,
                            scenario, "deaths", c(10, 12, 14), FALSE)

  expect_equal(nrow(res), 25)    # 5 runs, 5 years
  expect_equal(res$deaths[res$run_id == 1 & res$year == 2003],
               sum(data$deaths[data$year == 2003 & data$run_id == 1 &
                                 data$age %in% c(10, 12, 14)]))

  # By cohort

  res <- prepare_graph_data(base, touchstone, disease, group, country,
                            scenario, "deaths", c(10, 12, 14), TRUE)

  expect_equal(min(res$year), min(data$year) - max(data$age))
  expect_equal(max(res$year), max(data$year) - min(data$age))

  # Test graph - we can't really, but just check it doesn't crash.

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths"))

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths", log = TRUE))

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths", scenario2 = scenario))

  # Packit gets called if needed

  fake_result <- mockery::mock("fake_result")
  mockery::stub(stone_stochastic_graph, "prepare_central_data", fake_result)

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths", packit_id = "123",
    packit_file = "file.csv"))

  mockery::expect_called(fake_result, 1)
  mockery::expect_args(fake_result, 1, "123", "file.csv", country,
                       scenario, "deaths", NULL, FALSE)

})

test_that("stochastic_explorer data_dir handling", {
  expect_error(stochastic_explorer(file.path(tempdir(), "potato", "salad")),
                      "Cannot access the path/mount")
})

test_that("Can launch shiny app", {
  fake_path <- tempdir()
  runApp_called <- FALSE
  runApp_arg <- NULL

  local_mocked_bindings(
    runApp = function(app_dir) {
      runApp_called <<- TRUE
      runApp_arg <<- app_dir
      invisible(NULL)
    },
    .env = environment(stochastic_explorer)
  )

  withr::with_envvar(c(), {
    if (exists("data_dir", envir = .GlobalEnv)) {
      rm(data_dir, envir = .GlobalEnv)
    }
    stochastic_explorer(data_dir = fake_path)
    expect_true(exists("data_dir", envir = .GlobalEnv))
    expect_equal(get("data_dir", envir = .GlobalEnv), fake_path)
    expect_true(runApp_called)
  })
})

test_that("Age formats are reasonable", {
  expect_equal(age_string(NULL), "all ages")
  expect_equal(age_string(c(5,4,3,2,1,5,4,3,2,1)), "age 1..5")
  expect_equal(age_string(c(2,4,6,8)), "selected ages")
})

test_that("Parsing central from packit works", {
  # Packit gets called if needed

  fake <- data.frame(
    scenario_type = "RSV-rout", scenario = "RSV-rout",
    year = c(rep(2000, 4), rep(2001, 4), rep(2000, 4), rep(2001, 4)),
    age = c(rep(0, 8), rep(1, 8)),
    country = "RFP",
    burden_outcome = rep(c("cases", "dalys", "deaths", "yll"), 2),
    value = 1:16)

  rds <- tempfile(fileext = ".rds")
  saveRDS(fake, rds)

  fetch_fake <- function(id, file) rds
  mockery::stub(prepare_central_data, "fetch_packit", fetch_fake)

  res <- prepare_central_data("123", "file.csv",
    "RFP", "RSV-rout", "deaths", 0:5, TRUE)

  # Data in for death is: (year, age, deaths)
  # 2000, 0, 3
  # 2000, 1, 11
  # 2001, 0, 7
  # 2001, 1, 15
  # For cohort - this should become...
  # 1999, 11   (2000 year 1, were born in 1999)
  # 2000, 18   (2000 year 0, and 2001 year 1 born in 2000)
  # 2001, 7    (2001 year 0)

  expect_true(all.equal(res$year, c(1999, 2000, 2001)))
  expect_true(all.equal(res$deaths, c(11, 18, 7)))
})
