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

  # Expecting unaggregated data

  res <- get_graph_data(base, touchstone, disease, group, country,
                        scenario, "deaths", FALSE)

  expect_equal(nrow(res), 125)    # 5 runs, 5 years, 5 ages

  # Aggregate by year

  res2 <- aggregate_by_year(res, "deaths")
  expect_equal(nrow(res2), 25)    # 5 runs, 5 years, all age

  expect_equal(res2$deaths[res2$run_id == 1 & res2$year == 2002],
               sum(data$deaths[data$year == 2002 & data$run_id == 1]))

  # Select ages

  res2 <- aggregate_by_year(res, "deaths", c(10, 12, 14))

  expect_equal(nrow(res2), 25)    # 5 runs, 5 years
  expect_equal(res2$deaths[res2$run_id == 1 & res2$year == 2003],
               sum(data$deaths[data$year == 2003 & data$run_id == 1 &
                                 data$age %in% c(10, 12, 14)]))

  # By cohort

  res <- get_graph_data(base, touchstone, disease, group, country,
                        scenario, "deaths", TRUE)

  res2 <- aggregate_by_year(res, "deaths", c(10, 12, 14))

  expect_equal(min(res2$year), min(data$year) - max(data$age))
  expect_equal(max(res2$year), max(data$year) - min(data$age))

  # Test graph - we can't really, but just check it doesn't crash.

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths"))

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths", log = TRUE))

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    c(scenario, scenario), "deaths"))

  # Packit gets called if needed

  fake_data <- data.frame(year = 2000, age = 10, deaths = 25, run_id = 1)
  fake_result <- mockery::mock(fake_data)
  mockery::stub(stone_stochastic_graph, "get_packit_data", fake_result)

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths", packit_id = "123",
    packit_file = "file.csv"))

  mockery::expect_called(fake_result, 1)
  mockery::expect_args(fake_result, 1, "123", "file.csv", country,
                       scenario, "deaths", FALSE)

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

test_that("Filter formats are reasonable", {
  expect_equal(filter_string(NULL, "ages"), "all ages")
  expect_equal(filter_string(c(5,4,3,2,1,5,4,3,2,1), "ages"), "ages 1..5")
  expect_equal(filter_string(c(2,4,6,8), "potatoes"), "selected potatoes")
})
