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

})
