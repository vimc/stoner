###############################################################################

extract_burden_estimate_country_expectation <- function(path, con) {
  e <- list()

  e$bece_csv <- read_meta(path,
                          "burden_estimate_country_expectation.csv")

  e$countries <- db_get(con, "country", "id",
                        unique(unlist(split_semi(e$bece_csv$countries))), "id")

  e
}

test_extract_burden_estimate_country_expectation <- function(e) {
  expect_true(all(unique(unlist(split_semi(e$bece_csv$countries))) %in%
              e$countries$id), label = "All expectation countries recognised")

  expect_equal(sort(names(e$bece_csv)),
               c("countries", "modelling_group", "scenarios", "touchstone"),
               label = "Correct columns in expectation country csv")

  expect_equal(0, sum(unlist(lapply(split_semi(e$bece_csv),
                  function(x) any(duplicated(x))))),
               label = "No duplicate expectation countries in a scenario")

}

###############################################################################

transform_burden_estimate_country_expectation <- function(e, t_so_far) {

}

test_transform_burden_estimate_country_expectation <- function(t) {

}

###############################################################################

load_burden_estimate_country_expectation <- function(transformed_data, con) {

}
