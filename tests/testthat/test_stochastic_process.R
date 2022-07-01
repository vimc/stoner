context("stochastic_process")

###############################################################################
# Create fake responsibility data for dummy stochastic runs

create_dummy_stoch <- function(test,
                               same_countries = TRUE,
                               simple_outcomes = TRUE) {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  resps <- NULL
  variable_country <- c("AFG;IND;PAK","IND;PAK", "AFG;IND;ZWE")
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

  expect_error(stone_stochastic_process(test$con,
    "Rudolph", "", "", "", "", "", "", NA, NA, ".", bypass_cert_check = TRUE),
    "Unknown modelling group:")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "Plague", "", "", "", "", "", NA, NA, ".",
    bypass_cert_check = TRUE),
    "Unknown disease:")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "snowdon", "", "", "", "", NA, NA, ".",
    bypass_cert_check = TRUE),
    "Unknown touchstone:")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "potato", ".", "", "", NA, NA, ".",
    bypass_cert_check = TRUE),
    "scenario potato not found in touchstone nevis-1")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "piles", "nevis-1", "hot_chocolate", ".", "", "", NA, NA, ".",
    bypass_cert_check = TRUE),
    "scenario_description hot_chocolate not valid for disease piles")

  expect_error(stone_stochastic_process(test$con,
    "R-deer", "flu", "nevis-1", "hot_chocolate", ".", "", "", NA, NA, ".",
    bypass_cert_check = TRUE),
    "No responsibility_set for group R-deer in touchstone nevis-1")

  expect_error(stone_stochastic_process(test$con,
    "EBHQ-bunny", "flu", "nevis-1", "hot_chocolate", ".", "", "", NA, NA, ".",
    bypass_cert_check = TRUE),
    paste("No responsibility for group EBHQ-bunny,",
          "scenario hot_chocolate, touchstone nevis-1"))

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", file.path(test$path, "potato"),
    "", "", NA, NA, ".", bypass_cert_check = TRUE),
    "Input path not found:")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", ".",
    "", "", NA, NA, file.path(test$path, "potato"), bypass_cert_check = TRUE),
    "Output path not found:")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", ".",
    "", "", NA, NA, ".", pre_aggregation_path = file.path(test$path, "potato"),
    bypass_cert_check = TRUE),
    "Pre aggregation output path not found:")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    c("file1", "file2"), "", NA, NA, ".", bypass_cert_check = TRUE),
    "Incorrect files param - length should be 1 or 3")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", c(NA, NA), NA, ".", bypass_cert_check = TRUE),
    "Incorrect index_start - can be NA, or length 1 or 3")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", NA, c(NA, NA), ".", bypass_cert_check = TRUE),
    "Incorrect index_end - can be NA, or length 1 or 3")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", c(NA, NA, NA), c(NA, NA, "bob"),
    ".", bypass_cert_check = TRUE),
    "index_end must be all NA or integers")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", c(NA, NA, "gladys"), c(NA, NA, NA), ".",
    bypass_cert_check = TRUE),
    "index_start must be all NA or integers")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    "file_template", "", c(NA, NA, 1), 2, ".",
    bypass_cert_check = TRUE),
    "Mismatches of NA between index_start and index_end")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1",
    c("pies", "hot_chocolate", "holly"), test$path,
    c("f", "f:index", "f"), "", c(1, NA, 1), c(2, NA, 2), ".",
    bypass_cert_check = TRUE),
    "Mismatch between NA in index_start, and :index placeholder in files")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", test$path, "non_exist:index.xz",
    "", 1, 1, ".", bypass_cert_check = TRUE),
    "File not found: (.*)non_exist1.xz")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", test$path, "non_exist:index.xz",
    "", 1, 1, ".", deaths = c("deaths", "deaths"), bypass_cert_check = TRUE),
    "Duplicated outcome in deaths")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", test$path, "non_exist:index.xz",
    "", 1, 1, ".", deaths = "deaths", cases = "cases", dalys = "piles_dalys",
    bypass_cert_check = TRUE),
    "Outcomes not found, dalys \\('piles_dalys'\\)")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", test$path, "non_exist:index.xz",
    "", 1, 1, ".", deaths = "deaths", cases = "cases", dalys = "dalys",
    runid_from_file = TRUE, bypass_cert_check = TRUE),
    "Must have index_start and index_end as 1..200 to imply run_id")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", ".",
    "", "", NA, NA, ".", bypass_cert_check = TRUE, log_file = "."),
    "Log file '.' is a directory, must be a path to a file")

  expect_error(stone_stochastic_process(test$con,
    "LAP-elf", "flu", "nevis-1", "pies", ".",
    "", "", NA, NA, ".", bypass_cert_check = TRUE, log_file = "not/a/path"),
    "Cannot create file: directory 'not/a' does not exist.")
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
  if ((!"dalys" %in% names(data)) && ("dalys_men" %in% names(data))) {
    data[['dalys']] <- data$dalys_men + data$dalys_pneumo
    data$dalys_men <- NULL
    data$dalys_pneumo <- NULL
  }
  names(data)[names(data) == 'deaths'] <- paste0("deaths_", scenario)
  names(data)[names(data) == 'cases'] <- paste0("cases_", scenario)
  if ('dalys' %in% names(data)) {
    names(data)[names(data) == 'dalys'] <- paste0("dalys_", scenario)
  }
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
      outcome_cols <- names(d)
      outcome_cols <- outcome_cols[!outcome_cols %in%
        c("run_id", "country", "disease", "year", "age",
          "country_name", "cohort_size")]
      d[, outcome_cols] <- NA
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
                              include_disease = TRUE,
                              skip_dalys = FALSE) {

  resps <- create_dummy_stoch(test, same_countries, simple_outcomes)
  create_responsibilities(test, resps)
  do_test(test)
  runs <- 1:200
  n_runs <- 200
  data <- list()
  raw <- list()
  files <- list()

  for (i in seq_along(resps$scenario)) {

    countries <- split_semi(resps$countries[i])
    n_countries <- length(countries)
    ages <- resps$age_min_inclusive[i]:resps$age_max_inclusive[i]
    n_ages <- length(ages)
    years <- resps$year_min_inclusive[i]:resps$year_max_inclusive[i]
    n_years <- length(years)
    outcomes <- split_semi(resps$outcomes[i])
    if (skip_dalys) {
      outcomes <- outcomes[!grepl("dalys", outcomes)]
    }
    n_rows <- n_runs * n_countries * n_years * n_ages
    scenario <- resps$scenario[i]

    raw[[scenario]] <- data_frame(
      run_id = rep(runs, n_countries * n_ages * n_years),
      country = rep(rep(countries, each = n_runs), n_ages * n_years),
      year = rep(rep(years, each = n_runs * n_countries), n_ages),
      age = rep(ages, each = n_runs * n_countries * n_years))
    if (include_disease) {
      raw[[i]]$disease <- resps$disease[i]
    }
    raw[[scenario]]$country_name <- raw[[i]]$country
    raw[[scenario]]$cohort_size <- round(runif(n_rows) * 100000)
    for (outcome in outcomes) {
      raw[[scenario]][[outcome]] <- round(runif(n_rows) * 100000)
    }

    if (single_file_per_scenario) {
      write_csv(raw[[scenario]],
        file.path(test$path, sprintf("LAP-elf_%s.csv", scenario)))

      files[[i]] <- "LAP-elf_:scenario.csv"

    } else {

      for (j in seq_len(n_runs)) {
        d <- raw[[i]]
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
    data[[scenario]] <- reduce_outcomes(raw[[scenario]], scenario)
  }

  all_countries <- unique(unlist(lapply(data,
                    function(x) unique(x$country))))
  data <- expand_countries(data, all_countries)
  data <- bind_columns(data)
  data <- convert_country(test, data)
  list(raw = raw, data = data, resps = resps, files = unlist(files))
}

###############################################################################

stochastic_runner <- function(same_countries = TRUE,
                              simple_outcomes = TRUE,
                              single_file_per_scenario = TRUE,
                              include_run_id = TRUE,
                              include_disease = TRUE,
                              upload = FALSE,
                              allow_new_database = TRUE,
                              bypass_cert_check = TRUE,
                              dalys_df = NULL,
                              cert = "",
                              pre_aggregation_path = NULL,
                              lines = Inf,
                              log_file = NULL,
                              silent = TRUE) {

  test <- new_test()

  res <- random_stoch_data(test, same_countries, simple_outcomes,
                           single_file_per_scenario, include_run_id,
                           include_disease, !is.null(dalys_df))

  if (is.data.frame(dalys_df)) {
    fake_lifetable_db(test$con)
  }


  if (is.na(cert)) {
    cert <- valid_certificate(test$con, test$path)
  }

  index_start <- NA
  index_end <- NA
  if (!single_file_per_scenario) {
    index_start <- 1
    index_end <- 200
  }

  deaths <- "deaths"
  cases <- "cases"
  dalys <- "dalys"
  if (!simple_outcomes) {
    deaths <- c("deaths_acute", "deaths_chronic")
    cases <- c("cases_acute", "cases_chronic")
    dalys <- c("dalys_men", "dalys_pneumo")
  }

  if (!is.null(dalys_df)) {
    dalys <- dalys_df
  }

  stone_stochastic_process(test$con, "LAP-elf", "flu", "nevis-1",
                           res$resps$scenario, test$path, res$files,
                           cert = cert,
                           index_start, index_end, test$path,
                           pre_aggregation_path,
                           deaths, cases, dalys,
                           runid_from_file = !include_run_id,
                           allow_missing_disease = !include_disease,
                           upload_to_annex = upload, annex = test$con,
                           allow_new_database = allow_new_database,
                           bypass_cert_check = bypass_cert_check,
                           testing = TRUE,
                           lines = lines,
                           log_file = log_file,
                           silent = silent)
  list(
    test = test,
    raw = res$raw,
    data = res$data,
    cal = qs::qread(file.path(test$path, "LAP-elf_flu_calendar.qs")),
    cal_u5 = qs::qread(file.path(test$path, "LAP-elf_flu_calendar_u5.qs")),
    coh = qs::qread(file.path(test$path, "LAP-elf_flu_cohort.qs")),
    coh_u5 = qs::qread(file.path(test$path, "LAP-elf_flu_cohort_u5.qs"))
  )
}

compare <- function(data, reduced, cohort = FALSE, u5 = FALSE) {
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
          if (!identical(as.numeric(dsub_y[[field]]),
                         as.numeric(red_y[[field]]))) {
              return(FALSE)
          }
        }
      }
    }
  }
  TRUE
}

compare_all <- function(results) {
  expect_true(compare(results$data, results$cal))
  expect_true(compare(results$data, results$cal_u5, FALSE, TRUE))
  expect_true(compare(results$data, results$coh, TRUE, FALSE))
  expect_true(compare(results$data, results$coh_u5, TRUE, TRUE))
}

test_that("Stochastic - same countries, simple test", {
  compare_all(stochastic_runner())
})

test_that("Stochastic - same countries, multi files", {
  compare_all(stochastic_runner(single_file_per_scenario = FALSE))
  compare_all(stochastic_runner(single_file_per_scenario = FALSE,
                                include_run_id = FALSE))
})

test_that("Stochastic - same countries, multi outcomes, multi files", {
  compare_all(stochastic_runner(single_file_per_scenario = TRUE,
                                simple_outcomes = FALSE))
})

test_that("Stochastic - same countries, multi outcomes, multi files", {
  compare_all(stochastic_runner(include_disease = FALSE))
})

test_that("Stochastic - differing countries", {
  compare_all(stochastic_runner(same_countries = FALSE))
})

test_that("Stochastic - cert not found", {
  expect_error(
    stochastic_runner(bypass_cert_check = FALSE, cert = "non_existing.json"),
    "Certificate not found:")
})

test_that("Stochastic - cert OK", {
  compare_all(stochastic_runner(bypass_cert_check = FALSE, cert = NA))
})

test_that("Stochastic - check database table exists", {
  test <- new_test()
  standard_disease_touchstones(test)
  standard_responsibility_support(test)
  test <- do_test(test)
  new_file <- tempfile(fileext = ".csv")
  write.csv(mtcars, new_file)
  expect_error(stone_stochastic_upload(new_file, test$con, test$con,
    "LAP-elf", "flu", "nevis-1", FALSE, FALSE, FALSE),
    "stochastic_file database table not found")
})

test_that("Stochastic - with upload", {
  result <- stochastic_runner(upload = TRUE)
  meta <- DBI::dbReadTable(result$test$con, "stochastic_file")
  expect_equal(nrow(meta), 4)
  expect_equal(unique(meta$touchstone), "nevis-1")
  expect_equal(unique(meta$modelling_group), "LAP-elf")
  expect_equal(unique(meta$disease), "flu")
  expect_equal(1, nrow(meta[meta$is_cohort & meta$is_under5,]))
  expect_equal(1, nrow(meta[meta$is_cohort & !meta$is_under5,]))
  expect_equal(1, nrow(meta[!meta$is_cohort & !meta$is_under5,]))
  expect_equal(1, nrow(meta[!meta$is_cohort & meta$is_under5,]))
  expect_equal(unique(meta$version), 1)

  # Actually, latest research reveals that more pies in a
  # are extremely effective at repelling elf flu but, oddly,
  # only when looking across a calendar year.

  cal <- meta$id[!meta$is_cohort & !meta$is_under5]

  total <- DBI::dbGetQuery(result$test$con, sprintf("
    SELECT SUM(deaths_pies) FROM stochastic_%s", cal))$sum

  expect_equal(total, sum(result$cal$deaths_pies))

  result$cal$deaths_pies <- round(result$cal$deaths_pies / 2)

  new_qs_file <- tempfile(fileext = ".qs")
  qs::qsave(x = result$cal, file = new_qs_file)

  stone_stochastic_upload(new_qs_file, result$test$con, result$test$con,
                          "LAP-elf", "flu", "nevis-1", is_cohort = FALSE,
                          is_under5 = FALSE, allow_new_database = FALSE,
                          testing = TRUE)

  new_total <- DBI::dbGetQuery(result$test$con, sprintf("
    SELECT SUM(deaths_pies) FROM stochastic_%s", cal))$sum

  expect_equal(sum(result$cal$deaths_pies), new_total)
  new_meta <- DBI::dbReadTable(result$test$con, "stochastic_file")
  expect_equal(2, new_meta$version[!new_meta$is_cohort & !new_meta$is_under5])
})

test_that("stochastic_upload can upload csv file", {
  test <- new_test()
  ## Upload all tables upfront to the db so table ID is always the same
  result <- stochastic_runner(upload = TRUE)

  new_csv_file <- tempfile(fileext = ".csv")
  write_csv(x = result$cal_u5, file = new_csv_file)
  expect_message(
    stone_stochastic_upload(new_csv_file, result$test$con, result$test$con,
                            "LAP-elf", "flu", "nevis-1", is_cohort = FALSE,
                            is_under5 = TRUE, allow_new_database = TRUE,
                            testing = TRUE),
    "Overwriting table with id 1")

  data <- DBI::dbGetQuery(result$test$con, "SELECT * FROM stochastic_1")
  expect_equal(data, result$cal_u5)

  cohort_csv <- tempfile(fileext = ".csv")
  write_csv(x = result$coh, file = cohort_csv)
  expect_message(
    stone_stochastic_upload(cohort_csv, result$test$con, result$test$con,
                            "LAP-elf", "flu", "nevis-1", is_cohort = TRUE,
                            is_under5 = FALSE, allow_new_database = TRUE,
                            testing = TRUE),
    "Overwriting table with id 4")

  data <- DBI::dbGetQuery(result$test$con, "SELECT * FROM stochastic_4")
  expect_equal(data, result$coh)
})

##############################################################################
# DALYs related.

fake_lifetable_db <- function(con) {
  value_years <- DBI::dbGetQuery(con, "
      SELECT id FROM demographic_value_unit WHERE name='Years'")$id

  variant <- DBI::dbGetQuery(con, "
      INSERT INTO demographic_variant (code, name)
           VALUES ('elf_estimates', 'ELF Estimates')
        RETURNING id")$id

  source <- DBI::dbGetQuery(con, "
      INSERT INTO demographic_source (code, name)
           VALUES ('elf_2020', 'Elf dataset')
        RETURNING id")$id

  dst_life_ex <- DBI::dbGetQuery(con, "
      INSERT INTO demographic_statistic_type
                  (code, age_interpretation, name, year_step_size,
                   reference_date, gender_is_applicable, demographic_value_unit,
                   default_variant)
           VALUES ('life_ex', 'Age', 'life_ex', 5, '2000-07-01', TRUE, $1, $2)
        RETURNING id", list(value_years, variant))$id

  dataset <- DBI::dbGetQuery(con, "
      INSERT INTO demographic_dataset (description, demographic_source,
                                       demographic_statistic_type)
           VALUES ('Elf dataset', $1, $2)
        RETURNING id", list(source, dst_life_ex))$id

  tdd <- DBI::dbGetQuery(con, "
      INSERT INTO touchstone_demographic_dataset
                  (touchstone, demographic_dataset)
           VALUES ('nevis-1', $1)
        RETURNING id", dataset)$id

  both_gender <- DBI::dbGetQuery(con, "SELECT * FROM gender WHERE code='both'")$id

  age_from <- c(0, 1, seq(5, 100, by = 5))
  age_to <- c(0, 4,seq(9, 99, by = 5), 120)
  years <- seq(1950, 2095, by = 5)

  fake_life_ex <- data_frame(
    year = rep(years, each = length(age_from)),
    age_from = age_from,
    age_to = age_to,
    value = 120 - age_from,
    country = rep(c("AFG", "ZWE"), each = length(age_from) * length(years)),
    demographic_variant = variant,
    demographic_source = source,
    demographic_statistic_type = dst_life_ex,
    demographic_dataset = dataset,
    gender = both_gender)

  DBI::dbWriteTable(con, "demographic_statistic", fake_life_ex, append = TRUE)
}

test_that("Stochastic - with DALYs", {
  dalys_df <- data_frame(
    outcome = c("cases_acute", "deaths_chronic"),
    proportion = c(0.1, 0.2),
    average_duration = c(20, 1000),
    disability_weight = c(0.4, 0.6))

  result <- stochastic_runner(upload = FALSE, dalys_df = dalys_df,
                              simple_outcomes = FALSE)

  lt <- stoner_life_table(result$test$con, "nevis-1", 2000, 2100, TRUE)

  dalys_pies <- NULL
  for (country in c("AFG", "ZWE")) {

    country_nid <- DBI::dbGetQuery(result$test$con,
      "SELECT nid FROM country WHERE id=$1", country)$nid

    for (scenario in c("pies", "holly", "hot_chocolate")) {
      dalys <- paste0("dalys_", scenario)
      data <- result$raw[[scenario]][result$raw[[scenario]]$country == country, ]
      data$.code <- paste(data$country, data$year, data$age, sep="-")
      data$life_ex <- lt$value[match(data$.code, lt$.code)]
      data[[dalys]] <-
        (data$cases_acute * 0.1 * pmin(20, data$life_ex) * 0.4) +
        (data$deaths_chronic * 0.2 * data$life_ex * 0.6)

      if (scenario == "pies") {
        dalys_pies <- rbind(dalys_pies,
          data[data$run_id == 1,])
      }

      split_runs <- split(data, data$run_id)

      df_cal_all <- NULL
      df_cal_u5_all <- NULL
      df_coh_all <- NULL
      df_coh_u5_all <- NULL

      for (run in split_runs) {

        #### Calendar

        df_cal <- NULL
        df_cal_u5 <- NULL
        years <- split(run, run$year)
        for (year in years) {
          df_cal <- rbind(df_cal, data_frame(
            run_id = unique(run$run_id),
            year = unique(year$year),
            dalys = sum(year[[dalys]])
          ))

          year <- year[year$age < 5, ]
          df_cal_u5 <- rbind(df_cal_u5, data_frame(
            run_id = unique(run$run_id),
            year = unique(year$year),
            dalys = sum(year[[dalys]])
          ))

        }
        df_cal_all <- rbind(df_cal_all, df_cal)
        df_cal_u5_all <- rbind(df_cal_u5_all, df_cal_u5)

        #### Cohort

        run$cohort <- run$year - run$age
        df_coh <- NULL
        df_coh_u5 <- NULL
        cohorts <- split(run, run$cohort)
        for (cohort in cohorts) {
          df_coh <- rbind(df_coh, data_frame(
            run_id = unique(run$run_id),
            cohort = unique(cohort$cohort),
            dalys = sum(cohort[[dalys]])
          ))

          cohort <- cohort[cohort$age < 5, ]
          if (nrow(cohort) > 0) {
            df_coh_u5 <- rbind(df_coh_u5, data_frame(
              run_id = unique(run$run_id),
              cohort = unique(cohort$cohort),
              dalys = sum(cohort[[dalys]])
            ))
          }
        }
        df_coh_all <- rbind(df_coh_all, df_coh)
        df_coh_u5_all <- rbind(df_coh_u5_all, df_coh_u5)
      }

      threshold <- 1e-7
      expect_true(all(abs(df_cal_all$dalys - result$cal[[dalys]][result$cal$country == country_nid]) < threshold))
      expect_true(all(abs(df_coh_all$dalys - result$cah[[dalys]][result$coh$country == country_nid]) < threshold))
      expect_true(all(abs(df_cal_u5_all$dalys - result$cal_u5[[dalys]][result$cal_u5$country == country_nid]) < threshold))
      expect_true(all(abs(df_coh_u5_all$dalys - result$coh_u5[[dalys]][result$coh_u5$country == country_nid]) < threshold))
    }
  }

  # Also test non-stochastic DALYs here, since we have the data we need...

  # Create a burden estimate set, and all the table support that goes with...

  pies <- result$raw$pies
  con <- result$test$con
  resp_id <- DBI::dbGetQuery(con, "
    SELECT responsibility.id FROM responsibility
      JOIN responsibility_set
        ON responsibility.responsibility_set = responsibility_set.id
      JOIN scenario
        ON responsibility.scenario = scenario.id
     WHERE scenario_description = 'pies'
       AND modelling_group = 'LAP-elf'
       AND responsibility_set.touchstone = 'nevis-1'")$id

  DBI::dbExecute(con, "
    INSERT INTO model
                (id, modelling_group, description, is_current)
        VALUES ('LAP-elf', 'LAP-elf', 'Ho, ho, ho', TRUE)")

  model_version <- DBI::dbGetQuery(con, "
    INSERT INTO model_version
                (model, version)
         VALUEs ('LAP-elf', 1)
      RETURNING id")$id

  DBI::dbExecute(con, "
    INSERT INTO app_user
                (username, name, email)
         VALUES ('Elf','Head Elf', 'head@lapland.edu')")

  new_bes <- DBI::dbGetQuery(con, "
    INSERT INTO burden_estimate_set
                (model_version, responsibility, run_info, interpolated, complete,
                 uploaded_by)
         VALUES ($1, $2, 'Info', FALSE, TRUE, 'Elf')
      RETURNING id", list(model_version, resp_id))$id

  burdens <- DBI::dbGetQuery(con, "
    SELECT id, code
      FROM burden_outcome
     WHERE code IN ('deaths_acute', 'deaths_chronic',
                    'cases_acute', 'cases_chronic', 'cohort_size')")

  countries <- DBI::dbGetQuery(con, "
    SELECT id, nid FROM country WHERE id in ('AFG','ZWE')")


  pies <- pies[pies$run_id == 1,]

  # Bake pies into burden_estimate format

  pies$disease <- NULL
  pies$run_id <- NULL
  pies$country_name <- NULL
  pies$country <- countries$nid[match(pies$country, countries$id)]

  for (outcome in c("deaths_acute", "deaths_chronic", "cases_acute",
                    "cases_chronic", "cohort_size")) {
    data <- pies[, c("country", "year", "age", outcome)]
    data$burden_estimate_set <- new_bes
    data$burden_outcome <- burdens$id[burdens$code == outcome]
    names(data)[names(data) == outcome] <- "value"
    data$model_run <- NA
    DBI::dbWriteTable(con, "burden_estimate", data, append = TRUE)
  }

  DBI::dbExecute(con,
    "UPDATE responsibility
        SET current_burden_estimate_set = $1
      WHERE id = $2", list(new_bes, resp_id))

  # Hurrah. We can *finally* test DALYs.

  out <- tempfile(fileext = ".qs")
  dat <- stoner_dalys_for_db(con, dalys_df,
                              burden_estimate_set_id = new_bes,
                              output_file = out)
  dat2 <- stoner_dalys_for_db(con, dalys_df,
                                     "LAP-elf", "flu", "nevis-1", "pies",
                                     output_file = out)

  df <- qs::qread(out)

  expect_identical(dat, dat2)
  expect_equal(dat$data$dalys, df$dalys)
  unlink(out)

  # Check db method got the same answer as doing dalys from the files

  dalys_pies <- dalys_pies[
    order(dalys_pies$country, dalys_pies$year, dalys_pies$age), ]

  expect_equal(dat$data$dalys, dalys_pies$dalys_pies)

})

test_that("preaggregated data can be saved to disk", {
  t <- tempfile()
  dir.create(t, FALSE, TRUE)
  output <- stochastic_runner(pre_aggregation_path = t)

  files <- list.files(t)
  expect_length(files, 2) ## 2 countries, 1 modelling group, 1 disease = 2 files
  expect_setequal(files, c("LAP-elf_flu_4_pre_aggregation.qs",
                           "LAP-elf_flu_716_pre_aggregation.qs"))

  country_4 <- qs::qread(file.path(t, "LAP-elf_flu_4_pre_aggregation.qs"))
  expect_setequal(colnames(country_4),
                  c("country", "year", "run_id", "age", "deaths_pies",
                    "cases_pies", "dalys_pies", "deaths_hot_chocolate",
                    "cases_hot_chocolate", "dalys_hot_chocolate",
                    "deaths_holly", "cases_holly", "dalys_holly"))
  expect_true(all(country_4$country == 4))

  country_716 <- qs::qread(file.path(t, "LAP-elf_flu_716_pre_aggregation.qs"))
  expect_setequal(colnames(country_716),
                  c("country", "year", "run_id", "age", "deaths_pies",
                    "cases_pies", "dalys_pies", "deaths_hot_chocolate",
                    "cases_hot_chocolate", "dalys_hot_chocolate",
                    "deaths_holly", "cases_holly", "dalys_holly"))
  expect_true(all(country_716$country == 716))
})

test_that("info about stochastic processing can be logged", {
  t <- tempfile()
  dir.create(t, FALSE, TRUE)
  log <- file.path(t, "new_file.txt")
  pre_aggregation_path <- tempfile()
  dir.create(pre_aggregation_path, FALSE, TRUE)
  output <- stochastic_runner(log_file = log,
                              pre_aggregation_path = pre_aggregation_path,
                              silent = FALSE)

  logs <- readLines(log)
  expect_true(any(grepl(paste0("Validated inputs, processing scenario data ",
                               "for modelling_group: LAP-elf, disease: flu"),
                        logs)))
  expect_true(any(grepl("[Elapsed: 0.\\d+ secs]", logs)))
  expect_true(any(grepl(paste0("Processing for modelling_group: LAP-elf, ",
                               "disease: flu completed in "), logs)))
  expect_true(any(grepl("size \\d+\\.\\d KiB", logs)))
})

test_that("Stochastic - can run with subset of data", {
  out <- stochastic_runner(lines = 10)
  expect_equal(nrow(out$cal), 10)
  expect_equal(nrow(out$cal_u5), 10)
  expect_equal(nrow(out$coh), 10)
  expect_equal(nrow(out$coh_u5), 10)
})
