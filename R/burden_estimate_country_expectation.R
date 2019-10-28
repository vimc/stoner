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

  expect_equal(0, sum(unlist(lapply(split_semi(e$bece_csv$countries),
                  function(x) any(duplicated(x))))),
               label = "No duplicate expectation countries in any scenario")

}

###############################################################################

transform_burden_estimate_country_expectation <- function(e, t_so_far) {

  get_responsibility_sets <- function(modelling_group, touchstone, e) {
    rsets <- e$responsibility_set[
      e$responsibility_set$modelling_group == modelling_group &
        e$responsibility_set$touchstone == touchstone, ]

    if (nrow(rsets) > 1) {
      stop(sprintf("Error - multiple responsibility-sets for %s %s",
                   modelling_group, touchstone))
    }
    rsets$id
  }

  get_expectations_wildcard<- function(e, row, rset) {
    unique(e$responsibility[e$responsibility$responsibility_set == rset, ]$expectations)
  }

  get_expectations_scenario <- function(e, row, rset) {
    scenarios <- data_frame(scenario = split_semi(row$scenarios)[[1]])
    scenarios$id <- e$scenario$id[match(
      scenarios[['scenario']], e$scenario$scenario_description)]

    unique(e$responsibility[e$responsibility$responsibility_set == rset &
             e$responsibility$scenario %in% scenarios$id, ]$expectations)
  }

  transform_bece_expec <- function(df, row, expecs) {
    for (expec in expecs) {
      if (expec %in% df$burden_estimate_expectation) {
        stop("Expectation already found for %s", expec)
      }
    }

    countries <- split_semi(row$countries)[[1]]

    df <- rbind(df, data_frame(
        burden_estimate_expectation = rep(expecs, each = length(countries)),
        country = rep(countries, length(expecs))
      )
    )
    df
  }

  bece_df <- data_frame(burden_estimate_expectation = NA, country = NA)

  for (r in seq_len(nrow(e$bece_csv))) {
    row <- e$bece_csv[r, ]
    rset <- get_responsibility_sets(row$modelling_group, row$touchstone, e)

    if (row$scenarios == '*') {
      expecs <- get_expectations_wildcard(e, row, rset)
    } else {
      expecs <- get_expectations_scenario(e, row, rset)
    }
    bece_df <- transform_bece_expec(bece_df, row, expecs)
  }

  list(burden_estimate_country_expectation = bece_df)

}

test_transform_burden_estimate_country_expectation <- function(t) {
  bece <- t$burden_estimate_country_expectation
  bece$mash <- paste(bece$burden_estimate_expectation, bece$country, sep = '#')
  expect_false(any(duplicated(bece$mash)),
    label = "No duplicate burden estimate expectation countries")
}

###############################################################################

load_burden_estimate_country_expectation <- function(transformed_data, con) {
  to_edit <- add_return_edits("burden_estimate_expectation_country",
                              transformed_data, con)
}
