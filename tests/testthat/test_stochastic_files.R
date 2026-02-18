context("stochastic_files")

test_that("Standardise works with :scenario and :index", {
  fake <- data.frame(
    disease = "elf-piles",
    country = "LAP",
    year = rep(2000:2005, each = 5),
    age = rep(0:5, 5),
    cases = 1:30,
    cohort_size = c(100, 200, 300, 400, 500, 600),
    deaths = 31:60,
    dalys = 61:90,
    yll = 91:120,
    run_id = 1)
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

