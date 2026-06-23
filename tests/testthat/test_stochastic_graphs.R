context("stochastic_graphs")

# Not a great amount of testing we can do here, without analysing
# the plot somehow.

test_that("stochastic_graph data transforms", {

  base <- file.path(tempdir(), "root")
  dir.create(base, showWarnings = FALSE, recursive = TRUE)
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

  # Aggregate by age, for all years

  res2 <- aggregate_by_age(data, "deaths", NULL)
  expect_equal(nrow(res2), 25)
  expect_equal(res2$deaths[res2$run_id == 2 & res2$age == 10],
               sum(data$deaths[data$age == 10 & data$run_id == 2]))

  # Aggregate by age, for some years

  res2 <- aggregate_by_age(data, "deaths", c(2002, 2003))
  expect_equal(nrow(res2), 25)
  expect_equal(res2$deaths[res2$run_id == 2 & res2$age == 10],
               sum(data$deaths[data$age == 10 & data$run_id == 2 &
                                 data$year %in% c(2002, 2003)]))

  # Test graph - we can't really, but just check it doesn't crash; we've
  # already tested the functions being called.

  stone_stochastic_make_meta(base)

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
  fake_result <- mockery::mock(fake_data, cycle = TRUE)
  mockery::stub(stone_stochastic_graph, "get_packit_data", fake_result)

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths", packit_id = "123",
    packit_file = "file.csv"))

  mockery::expect_called(fake_result, 1)
  mockery::expect_args(fake_result, 1, "123", "file.csv", country,
                       scenario, "deaths", FALSE)

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths", xaxis = "age"))

  expect_no_error(stone_stochastic_graph(
    base, touchstone, disease, group, country,
    scenario, "deaths", xaxis = "age",
    packit_id = "123",
    packit_file = "file.csv"))


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

test_that("Argument counts are tested", {

  expect_error(stone_stochastic_graph(
    "b", c("T1", "T2", "T3"), "d", "g", "c", "s", "o"),
    "Only specify one or two touchstones")

  expect_error(stone_stochastic_graph(
    "b", NULL, "d", "g", "c", "s", "o"),
    "Only specify one or two touchstones")

  expect_error(stone_stochastic_graph(
    "b", "T1", NULL, "g", "c", "s", "o"),
    "Only specify one disease")

  expect_error(stone_stochastic_graph(
    "b", "T1", c("D1", "D2"), "g", "c", "s", "o"),
    "Only specify one disease")

  expect_error(stone_stochastic_graph(
    "b", "t", "d", NULL, "c", "s", "o"),
    "Only specify one or two modelling groups")

  expect_error(stone_stochastic_graph(
    "b", "t", "d", c("g1", "g2", "g3"), "c", "s", "o"),
    "Only specify one or two modelling groups")

  expect_error(stone_stochastic_graph(
    "b", "t", "d", "g", "c", NULL, "o"),
    "Only specify one or two scenarios")

  expect_error(stone_stochastic_graph(
    "b", "t", "d", "g", "c", c("s1", "s2", "s3"), "o"),
    "Only specify one or two scenarios")

  expect_error(stone_stochastic_graph(
    "b", c("T1", "T2"), "d", c("g1", "g2"), "c", "s", "o"),
    "Only one of `touchstones` or `groups` can be plural")

  expect_error(stone_stochastic_graph(
    "b", c("T1", "T2"), "d", c("g1", "g2"), "c", c("s1", "s2"), "o"),
    "Only one of `touchstones` or `groups` can be plural")

})

test_that("Arguments are validated", {

  expect_error(stone_stochastic_graph(
    "b", c("T1", "T2"), "d", "g", "c", c("s1", "s2"), "o", xaxis = "age"),
    "Please call stone_stochastic_meta")

  path <- file.path(tempdir(), "root2")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  meta <- data.frame(
    touchstone = c("T1", "T1", "T1", "T1", "T2", "T2", "T2", "T2"),
       disease = rep("D", 8),
         group = c("G1", "G1", "G2", "G2", "G1", "G1", "G4", "G4"),
      scenario = c("S1", "S2", "S1", "S2", "S3", "S4", "S3", "S4"),
      countries = c("A;B;C", "A;B", "A;B;C", "A;B;C", "A;B", "A;B;C", "A;B", "A;B;C"),
      outcomes = c("X;Y", "X;Y", "X;Y;Z", "X;Y;Z", "X;Y", "X;Y", "X;Y", "X;Y"))
  write.csv(meta, file.path(path, "meta.csv"), row.names = FALSE, quote = FALSE)

  # Touchstone not found

  expect_error(stone_stochastic_graph(path, "T3", "D", "G1", "A", "S1", "X"),
               "Touchstone not found: T3")

  expect_error(stone_stochastic_graph(path, c("T1", "T3"), "D", "G1", "A", "S1", "X"),
               "Touchstone not found: T3")

  # Disease not found

  expect_error(stone_stochastic_graph(path, "T1", "X", "G1", "A", "S1", "X"),
               "Disease X not found in touchstone T1")

  # Modelling group not found

  expect_error(stone_stochastic_graph(path, "T1", "D", "G3", "A", "S1", "X"),
               "Groups not found in T1: G3")

  expect_error(stone_stochastic_graph(path, c("T1", "T2"), "D", "G2", "A", "S1", "X"),
               "Groups not found in T2: G2")

  # Scenario(s) not found

  expect_error(stone_stochastic_graph(path, "T1", "D", "G1", "A", "S3", "X"),
               "Scenario S3 not found in T1, G1")

  expect_error(stone_stochastic_graph(path, "T1", "D", "G1", "A", c("S1", "S3"), "X"),
               "Scenario S3 not found in T1, G1")

  expect_error(stone_stochastic_graph(path, c("T1", "T2"), "D", "G1", "A", c("S1", "S3"), "X"),
               "Scenario S1 not found in T2, G1")

  # Country not found

  expect_error(stone_stochastic_graph(path, "T2", "D", "G1", "C", c("S3", "S4"), "X"),
               "Country C not found in T2, G1, S3")

  # Outcome(s) not found

  expect_error(stone_stochastic_graph(path, "T1", "D", c("G2", "G1"), "A", "S2", "Z"),
               "Outcome Z not found in T1, G1, S2")

  # X-axis wrong

  expect_error(stone_stochastic_graph(
    path, "T1", "D", "G1", "A", c("S1", "S2"), "X", xaxis = "potato"),
    "`xaxis` must be either `time` or `age`")

  # File not found - because we haven't made it.

  expect_error(get_graph_data(path, "T1", "D", "G1", "A", "S1", "X", FALSE),
    "Couldn't find file(.*)")

})



test_that("Packit data is arranged correctly", {
  fake <- data.frame(
    scenario_type = "RSV-rout", scenario = "RSV-rout",
    year = c(rep(2000, 4), rep(2001, 4), rep(2000, 4), rep(2001, 4)),
    age = c(rep(6, 8), rep(7, 8)),
    country = "RFP",
    burden_outcome = rep(c("cases", "dalys", "deaths", "yll"), 2),
    value = 1:16)
  fake2 <- fake
  fake2$country <- "POT"
  fake2$scenario <- "XYZ-rout"
  fake <- rbind(fake, fake2)

  rds <- tempfile(fileext = ".rds")
  saveRDS(fake, rds)

  fetch_fake <- function(id, file) rds
  mockery::stub(get_packit_data, "fetch_packit", fetch_fake)

  res <- get_packit_data("", "", "RFP", "RSV-rout", "cases", TRUE)
  expect_true(unique(res$run_id) == 1)
  fake$year <- fake$year - fake$age
  fake <- fake[fake$country == "RFP", ]
  fake <- fake[fake$scenario == "RSV-rout", ]
  fake <- fake[fake$burden_outcome == "cases", ]
  fake <- fake[order(fake$year, fake$age), ]
  res <- res[order(res$year, res$age), ]
  expect_equal(nrow(fake), nrow(res))
  expect_all_true(fake$year ==res$year)
  expect_all_true(fake$age == res$age)
  expect_all_true(fake$value == res$cases)
})
