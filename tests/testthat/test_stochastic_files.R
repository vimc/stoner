context("stochastic_files")

fake_data <- function() {
  data.frame(
    disease = "elf-piles",
    country = "LAP",
    year = rep(2000:2005, each = 6),
    age = rep(0:5, 6),
    cases = 1:36,
    cohort_size = c(100, 200, 300, 400, 500, 600),
    deaths = 31:66,
    dalys = 61:96,
    yll = 91:126,
    run_id = 1)
}

test_that("Standardise works with :scenario and :index", {
  fake <- fake_data()
  fake2 <- fake
  fake2$run_id <- 2
  fake <- rbind(fake, fake2)

  tmpin <- tempdir()
  tmpout <- tempdir()

  # Clean-up folder if necessary...

  for (f in c("north_pole_optimistic_LAP.pq",
              "north_pole_fatalistic_LAP.pq")) {
    if (file.exists(file.path(tmpout, f))) {
      file.remove(file.path(tmpout, f))
    }
  }

  tmpfile <- tempfile(tmpdir = tmpin)
  write.csv(fake, paste0(tmpfile, "_optimistic_1"), row.names = FALSE)
  write.csv(fake, paste0(tmpfile, "_fatalistic_1"), row.names = FALSE)

  fake$country <- "POL"
  write.csv(fake, paste0(tmpfile, "_optimistic_2"), row.names = FALSE)
  write.csv(fake, paste0(tmpfile, "_fatalistic_2"), row.names = FALSE)

  stone_stochastic_standardise(
    group = "north_pole",
    in_path = tmpin,
    out_path = tmpout,
    scenarios = c("optimistic", "fatalistic"),
    files = paste0(basename(tmpfile), "_:scenario_:index"),
    index = 1:2
  )

  files <- list.files(path = tmpout)
  expect_true("north_pole_optimistic_LAP.pq" %in% files)
  expect_true("north_pole_optimistic_POL.pq" %in% files)
  expect_true("north_pole_fatalistic_LAP.pq" %in% files)
  expect_true("north_pole_fatalistic_POL.pq" %in% files)

  pq <- arrow::read_parquet(file.path(tmpout, "north_pole_optimistic_LAP.pq"))
  expect_true(unique(pq$country) == "LAP")
  expect_true(all.equal(sort(unique(pq$run_id)), 1:2))
  expect_true(all.equal(sort(unique(pq$age)), 0:5))
  expect_true(all.equal(sort(unique(pq$year)), 2000:2005))
})


test_that("Standardise works with missing run_id column", {
  fake <- fake_data()
  fake$run_id <- NULL

  tmpin <- tempdir()
  tmpout <- tempdir()
  tmpfile <- tempfile(tmpdir = tmpin)
  for (i in 1:200) {
    write.csv(fake, paste0(tmpfile, sprintf("_opt_%d", i)), row.names = FALSE)
    write.csv(fake, paste0(tmpfile, sprintf("_fat_%d", i)), row.names = FALSE)
  }

  stone_stochastic_standardise(
    group = "north_pole",
    in_path = tmpin,
    out_path = tmpout,
    scenarios = c("opt", "fat"),
    files = paste0(basename(tmpfile), "_:scenario_:index"),
    index = 1:200
  )

  files <- list.files(path = tmpout)
  expect_true("north_pole_fat_LAP.pq" %in% files)
  expect_true("north_pole_opt_LAP.pq" %in% files)

  pq <- arrow::read_parquet(file.path(tmpout, "north_pole_fat_LAP.pq"))
  tab <- table(pq$run_id)
  expect_all_equal(as.integer(tab), 36)
  expect_equal(length(tab), 200)
  expect_true(all.equal(names(tab), as.character(1:200)))
})


test_that("Create central - bad path / files", {

  expect_error(stone_stochastic_central("P:/potato", "", "", "", ""),
               "Path not found:")

  tmp <- tempdir()
  path <- file.path(tmp, "touchstone", "disease_group")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  expect_error(stone_stochastic_central(tmp, "touchstone", "disease", "group",
                                        "scenario"),
               "No .pq files found")

  file.create(file.path(path, "potato.pq"), showWarnings = FALSE)

  expect_error(stone_stochastic_central(tmp, "touchstone", "disease", "group",
                                        "scenario"),
               "No files matching scenario")
})

test_that("Create central works", {
  df1 <- fake_data()
  df2 <- df1
  df2$deaths <- df2$deaths * 2
  df2$run_id <- 2
  df1 <- rbind(df1, df2)

  df2 <- df1
  df2$country <- "NPL"
  df2$cases <- df2$cases * 2

  tmpdir <- tempdir()
  path <- file.path(tmpdir, "tc", "piles_northpole")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  arrow::write_parquet(df1, file.path(path, "northpole_sc1_LAP.pq"))
  arrow::write_parquet(df2, file.path(path, "northpole_sc1_NPL.pq"))
  stone_stochastic_central(tmpdir, "tc", "piles", "northpole", "sc1")
  expect_true(file.exists(file.path(path, "northpole_sc1_central.pq")))

  central <- as.data.frame(
    arrow::read_parquet(file.path(path, "northpole_sc1_central.pq")))
  expect_false("run_id" %in% names(central))
  expect_true(length(unique(central$country)) == 2)
  p1 <- central[central$year == 2002 & central$age == 3, ]

  expect_true(p1$deaths[p1$country == "LAP"] ==
        (df1$deaths[df1$year == 2002 & df1$age == 3 & df1$run_id == 1] +
         df1$deaths[df1$year == 2002 & df1$age == 3 & df1$run_id == 2]) / 2)

  expect_true(p1$cases[p1$country == "NPL"] == 2 * p1$cases[p1$country == "LAP"])
})
