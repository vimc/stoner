context("stochastic_process")

# Lots of tests for stochastic parameters...

#stone_stochastic_process <- function(con, modelling_group, disease,
#                                     touchstone, scenarios, in_path, files,
#                                     cert, index_start, index_end, out_path,
#                                     deaths = "deaths", cases = "cases",
#                                     dalys = "dalys",
#                                     runid_from_file = FALSE,
#                                     allow_missing_disease = FALSE,
#                                     upload_to_annex = FALSE,
#                                     annex = NULL)

###############################################################################
# Create fake responsibility data for dummy stochastic runs

create_dummy_stoch <- function(test,
                               same_countries = TRUE,
                               simple_outcomes = TRUE) {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resps <- NULL
  variable_country <- c("AFG;IND;PAK;ZWE","AFG;IND;PAK", "IND")
  scenarios <- c("pies", "hot_chocolate", "holly")

  for (scenario in seq_along(scenarios)) {
    df <- data_frame(
      scenario = scenarios[scenario],
      countries = "AFG;ZWE",
      outcomes = "cases;deaths;dalys"
    )
    if (!same_countries) {
      df$countries <- variable_country[scenario]
    }
    if (!simple_outcomes) {
      df$outcomes <- paste0("deaths_acute;deaths_chronic;cases_acute;",
                            "cases_chronic;dalys_pneumo;dalys_men")
    }

    resps <- rbind(resps, df)
  }

  resps$modelling_group <- "LAP-elf"
  resps$disease <- "flu"
  resps$touchstone <- "nevis-1"
  resps$scenario_type <- "standard"
  resps$year_min_inclusive <- 2000
  resps$year_max_inclusive <- 2005
  resps$age_min_inclusive <- 0
  resps$age_max_inclusive <- 7
  resps$cohort_min_inclusive <- 1993
  resps$cohort_max_inclusive <- 2005
  resps
}

test_that("Bad arguments", {
  test <- new_test()
  resps <- create_dummy_stoch(test)
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



################################################################
# Reduce multi-outcomes to one

reduce_outcomes <- function(data, scenario) {

  if (!"deaths" %in% names(data)) {
    data[['deaths']] <- data$deaths_acute + data$deaths_chronic
    data$deaths_acute <- NULL
    data$deaths_chronic <- NULL
  }
  if (!"cases" %in% names(data)) {
    data[['cases']] <- data$cases_acute + data$cases_chronic
    data$cases_acute <- NULL
    data$cases_chronic <- NULL
  }
  if (!"dalys" %in% names(data)) {
    data[['dalys']] <- data$dalys_men + data$days_pneumo
    data$dalys_men <- NULL
    data$dalys_pneumo <- NULL
  }
  names(data)[names(data) == 'deaths'] <- paste0("deaths_", scenario)
  names(data)[names(data) == 'cases'] <- paste0("cases_", scenario)
  names(data)[names(data) == 'dalys'] <- paste0("dalys_", scenario)
  data
}

###################################################
# Add countries with NA data, for cases where
# there are different countries in different scenarios, so
# all scenarios have same number of rows

expand_countries <- function(data, all_countries) {
  all_countries <- sort(all_countries)
  for (i in seq_along(data)) {
    present <- sort(unique(data[[i]]$country))
    absent <- all_countries[!all_countries %in% present]
    if (length(absent) > 0) {
      d <- data[[i]][data[[i]]$country == present[1], ]
      d$deaths <- NA
      d$cases <- NA
      d$dalys <- NA
      for (j in absent) {
        d$country <- j
        data[[i]] <- rbind(data[[i]], d)
      }
    }
  }
  data
}

#####################################################
# Combine data for different scenarios into one table

bind_columns <- function(data) {
  for (i in seq_along(data)) {
    data[[i]] <- data[[i]][order(data[[i]]$run_id,
                                 data[[i]]$country,
                                 data[[i]]$year,
                                 data[[i]]$age), ]
    data[[i]]$country_name <- NULL
    data[[i]]$cohort_size <- NULL
    data[[i]]$disease <- NULL

    if (i > 1) {
      data[[i]]$run_id <- NULL
      data[[i]]$country <- NULL
      data[[i]]$year <- NULL
      data[[i]]$age <- NULL
      data[[1]] <- cbind(data[[1]], data[[i]])
    }
  }

  data[[1]]
}

######################################
# And create the fake stochastic files

convert_country <- function(test, data) {
  countries <- unique(data$country)
  lookup <- DBI::dbGetQuery(test$con, sprintf(
    "SELECT * FROM country WHERE id IN %s",
    sql_in(unique(data$country))))
  data$country <- lookup$nid[match(data$country, lookup$id)]
  data
}

random_stoch_data <- function(test, same_countries = TRUE,
                              simple_outcomes = TRUE,
                              single_file_per_scenario = TRUE,
                              include_run_id = TRUE,
                              include_disease = TRUE) {

  resps <- create_dummy_stoch(test, same_countries, simple_outcomes)
  create_responsibilities(test, resps)
  do_test(test)
  runs <- 1:200
  n_runs <- 200
  data <- list()
  files <- list()

  for (i in seq_along(resps$scenario)) {

    countries <- split_semi(resps$countries[i])
    n_countries <- length(countries)
    ages <- resps$age_min_inclusive[i]:resps$age_max_inclusive[i]
    n_ages <- length(ages)
    years <- resps$year_min_inclusive[i]:resps$year_max_inclusive[i]
    n_years <- length(years)
    outcomes <- split_semi(resps$outcomes[i])
    n_rows <- n_runs * n_countries * n_years * n_ages
    scenario <- resps$scenario[i]

    data[[scenario]] <- data_frame(
      run_id = rep(runs, n_countries * n_ages * n_years),
      country = rep(rep(countries, each = n_runs), n_ages * n_years),
      year = rep(rep(years, each = n_runs * n_countries), n_ages),
      age = rep(ages, each = n_runs * n_countries * n_years))
    if (include_disease) {
      data[[i]]$disease <- resps$disease[i]
    }
    data[[scenario]]$country_name <- data[[i]]$country
    data[[scenario]]$cohort_size <- round(runif(n_rows) * 100000)
    for (outcome in outcomes) {
      data[[scenario]][[outcome]] <- round(runif(n_rows) * 100000)
    }

    if (single_file_per_scenario) {
      write_csv(data[[scenario]],
        file.path(test$path, sprintf("LAP-elf_%s.csv", scenario)))

      files[[i]] <- "LAP-elf_:scenario.csv"

    } else {

      for (j in seq_len(n_runs)) {
        d <- data[[i]]
        d <- d[d$run_id == j, ]
        if (!include_run_id) {
          d$run_id <- NULL
        }
        write_csv(d,
          file.path(test$path,
                    sprintf("LAP-elf_%s_%s.csv", resps$scenario[i], j)))
      }
      files[[i]] <- "LAP-elf_:scenario_:index.csv"
    }
    data[[scenario]] <- reduce_outcomes(data[[scenario]], scenario)
  }

  all_countries <- unique(unlist(lapply(data,
                    function(x) unique(x$country))))

  data <- expand_countries(data, all_countries)
  data <- bind_columns(data)
  data <- convert_country(test, data)
  list(data = data, resps = resps, files = unlist(files))
}

###############################################################################

stochastic_runner <- function(same_countries = TRUE,
                              simple_outcomes = TRUE,
                              single_file_per_scenario = TRUE,
                              include_run_id = TRUE,
                              include_disease = TRUE) {
  test <- new_test()
  res <- random_stoch_data(test, same_countries, simple_outcomes,
                           single_file_per_scenario, include_run_id,
                           include_disease)

  index_start <- NA
  index_end <- NA
  if (!single_file_per_scenario) {
    index_start <- 1
    index_end <- 200
  }

  stone_stochastic_process(test$con, "LAP-elf", "flu", "nevis-1",
                           res$resps$scenario, test$path, res$files, "",
                           index_start, index_end, test$path)
  list(
    test = test,
    data = res$data,
    cal = read_csv(file.path(test$path, "LAP-elf_flu_calendar.csv")),
    cal_u5 = read_csv(file.path(test$path, "LAP-elf_flu_calendar_u5.csv")),
    coh = read_csv(file.path(test$path, "LAP-elf_flu_cohort.csv")),
    coh_u5 = read_csv(file.path(test$path, "LAP-elf_flu_cohort_u5.csv"))
  )
}

compare <- function(test, data, reduced, cohort = FALSE, u5 = FALSE) {
  if (u5) {
    data <- data[data$age < 5, ]
  }
  if (cohort) {
    data$year <- data$year - data$age
    names(reduced)[names(reduced) == 'cohort'] <- 'year'
  }
  fields <- names(data)
  fields <- fields[!fields %in% c("run_id", "country", "year", "age")]
  years <- unique(data$year)
  countries <- unique(data$country)
  for (country in countries) {
    dsub <- data[data$country == country, ]
    red <- reduced[reduced$country == country, ]
    for (run in 1:200) {
      dsub_r <- dsub[dsub$run_id == run, ]
      red_r <- red[red$run_id == run, ]
      for (year in years) {
        dsub_y <- colSums(dsub_r[dsub_r$year == year, ])
        red_y <- red_r[red_r$year == year, ]
        for (field in fields) {
          if (dsub_y[[field]] != red_y[[field]]) {
            return(FALSE)
          }
        }
      }
    }
  }
  TRUE
}

compare_all <- function(results) {
  expect_true(compare(results$test, results$data, results$cal))
  expect_true(compare(results$test, results$data, results$cal_u5, FALSE, TRUE))
  expect_true(compare(results$test, results$data, results$coh, TRUE, FALSE))
  expect_true(compare(results$test, results$data, results$coh_u5, TRUE, TRUE))
}

test_that("Stochastic - same countries, simple outcomes, single files", {
  compare_all(stochastic_runner())
})

test_that("Stochastic - same countries, simple outcomes, multi files", {
  compare_all(stochastic_runner(single_file_per_scenario = FALSE))
})
