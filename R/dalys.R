##' One modelling group does not provide DALYs, and we need to
##' calculate those, both for their central estimates (already
##' uploaded to Montagu), and their stochastic estimates (provided
##' as CSV files). The DALYs calculation is a simple function of
##' the life expectancy of people (of a given age and country in a
##' given year), and various weighted contributions of different burden
##' outcomes.
##'
##' @export
##' @title Calculating DALYs for stochastic or central estimates
##' @param con DBI connection to a Montagu database. Used for retrieving
##' demographic data for life expectancy.
##' @param touchstone The touchstone (including version); the demographic
##' data retrieved will be specific to this touchstone.
##' @param data A data.frame containing burden estimates - either central or
##' stochastic. The columns for both kinds will include `country` (which may be
##' a Montagu numerical id, or the 3-letter code), `year`, `age`, and a number
##' of burden outcomes.Stochastic data will also have a `run_id`.
##' @param dalys_params A data.frame containing how to calculate dalys; each
##' row represents a condition, and the four columns describe how to calculate
##' the cost of that condition across the population in that year. For each
##' condition, the `outcome` column must provide a burden outcome (which will
##' be a column name in `data`). The `proportion` is the proportion of people
##' reported as that outcome, who suffer this condition. `average_duration` is
##' the number of years for which the condition is suffered, set to greater
##' than 120 for a life-long period. And `disability_weight` is a measure of
##' how severe the implications are of this particular condition. See
##' http://ghdx.healthdata.org/record/ihme-data/gbd-2017-disability-weights
##' for where these figures come from; the final figures are decided in
##' discussion with the groups.
##' @param life_table If `NULL`, then the life table will be looked up and
##' interpolated to yearly for age and time. This takes a bit of time, so the
##' resulting data is part of what this function returns, and the life table
##' part can be provided here to speed things up. If provided, it should be a
##' data.frame with the columns `value` and `.code`; the latter is in the
##' format country-year-age. The type of country in `.code` should match with
##' the type of the country in `data`. The years and ages will be interpolated
##' down to single-years.
##' @param year_min The first year of the range in which to calculate DALYs.
##' (Default 2000)
##' @param year_max The final year of the range in which to calculate DALYs.
##' (Default 2100)
##' @return A list with two named components; `life_table` is the result of
##' calling the life-table function, which can then be specified in subsequent
##' calls to `stoner_calculate_dalys`, and `data` is a version of the original
##' dataset, now with a `dalys` column. Note that if `data` already contained
##' a `dalys` column when provided to the function, that column will
##' be overwritten with the new results.
stoner_calculate_dalys <- function(con, touchstone, data, dalys_params,
                                   life_table = NULL,
                                   year_min = 2000, year_max = 2100) {
  # Type tests

  test_args(con, dalys_params, life_table)

  # Accept countries as either character or numerical id.

  char_countries <- is.character(data$country[1])

  res <- list()

  # Accept either a previously cached life-table (as it takes a few seconds),
  # or generate it if called with NULL.

  if (is.null(life_table)) {
    life_table <- stoner_life_table(con, touchstone, year_min, year_max,
                                    char_countries)
    res$life_table <- life_table
  }

  # Factor out multiplication from the dalys_params, as we do this every time.

  dalys_params$adjusted_weight <-
    dalys_params$disability_weight * dalys_params$proportion

  dalys_data <- NULL

  data$.code <- sprintf("%s-%s-%s", data$country, data$year, data$age)
  data$.life_ex <- life_table$value[match(data$.code, life_table$.code)]
  for (param_no in seq_len(nrow(dalys_params))) {
    param <- dalys_params[param_no, ]
    dalys_data[[param_no]] <- data[[param$outcome]] *
      pmin(data$.life_ex, param$average_duration) * param$adjusted_weight
  }

  data[['dalys']] <- rowSums(as.data.frame(dalys_data))
  data$.life_ex <- NULL
  data$.code <- NULL
  res$data <- data
  res
}

test_args <- function(con, dalys_params, life_table) {
  assert_connection(con)

  if (class(dalys_params) != "data.frame") {
    stop("dalys_params must be a data.frame")
  }

  assert_set_equal(names(dalys_params),
    c("average_duration", "disability_weight","outcome", "proportion"),
      paste("dalys_params needs columns outcome, proportion,",
            "average_duration and disablility_weight"))
  

  if (!is.null(life_table)) {
    if (class(life_table) != "data.frame") {
      stop("life_table (if specified) must be data.frame")
    }
    assert_set_equal(names(life_table), c("code", "value"),
      "life_table (if specified) must have columns .code and value")
  }
}


##' @title Calculate life expectancy table
##' @description Normally this will be called internally from
##' `stoner_calculate_dalys`, but for testing purposes is also exposed.
##' @param con DBI connection to a Montagu database. Used for retrieving
##' demographic data for life expectancy.
##' @param touchstone The touchstone (including version); the demographic
##' data retrieved will be specific to this touchstone.
##' @param year_min The first year of the range in which to calculate DALYs.
##' (Default 2000)
##' @param year_max The final year of the range in which to calculate DALYs.
##' (Default 2100)
##' @param char_countries A logical that if TRUE specifies that the countries
##' in the resulting life table should be expressed in 3-character form, or
##' otherwise numerical.
##' @return A data.frame with columns `value` and `.code`. The value is the
##' expected years of life left for a person in a particular country, a
##' particular year, and of a particular age. The `.code` is in the form
##' `country-year-age`, and both year and age are interpolated to
##' single years (whereas the UNWPP life table data is mostly in 5-year bands).
stoner_life_table <- function(con, touchstone, year_min, year_max,
                              char_countries = TRUE) {

  country_id <- if (char_countries) "country.id" else "nid"

  life_ex <- DBI::dbGetQuery(con, sprintf("
    SELECT %s as country, year, age_from, value
     FROM demographic_statistic
     JOIN touchstone_demographic_dataset
       ON touchstone_demographic_dataset.demographic_dataset =
          demographic_statistic.demographic_dataset
     JOIN touchstone
       ON touchstone.id = touchstone_demographic_dataset.touchstone
     JOIN demographic_statistic_type
       ON demographic_statistic_type.id =
          demographic_statistic.demographic_statistic_type
     JOIN gender
       ON gender.id = demographic_statistic.gender
     JOIN country
       ON country.id = demographic_statistic.country
    WHERE demographic_statistic_type.code = 'life_ex'
      AND touchstone.id = $1
      AND gender.code = 'both'
 ORDER BY year, age_from", country_id), touchstone)

  # Interpolate to single years

  life_ex_by_country_age <- split(life_ex, list(life_ex$country,
                                                life_ex$age_from))

  life_ex_interp_year <- rbindlist(lapply(life_ex_by_country_age,
    function(df) {
      interp <- stats::splinefun(
        x = df$year,
        y = df$value,
        method = "natural")

      data_frame(.code = sprintf("%s-%s", df$country[1], year_min:year_max),
                   age = df$age_from[1],
                 value = interp(year_min:year_max))
    }
  ))

  # Interpolate to single ages - preserving age 0.

  age_zero <- life_ex_interp_year[life_ex_interp_year$age == 0, ]
  age_zero$.code <- sprintf("%s-0", age_zero$.code)
  age_zero$age <- NULL

  age_nonzero <- life_ex_interp_year[life_ex_interp_year$age != 0, ]

  life_ex_by_country_age <- split(age_nonzero, age_nonzero$.code)

  age_midpoints <- c(2.5, seq(7, 97, 5), 110)

  df  <- rbind(age_zero, rbindlist(lapply(life_ex_by_country_age,
    function(df) {
      interp <- stats::splinefun(
        x = age_midpoints,
        y = df$value,
        method = "monoH.FC")

      data_frame(.code = sprintf("%s-%s", df$.code[1], 1:100),
        value = interp(1:100))
    }
  )))
}

##' @export
##' @title Create CSV with DALYs for an uploaded burden estimate set
##' @importFrom utils write.csv
##' @description When a group has uploaded a burden estimate set without DALYs,
##' this function can retrieve that burden estimate set, extend it with DALYs,
##' and write a new CSV file ready for uploading through the Montagu portal.
##' @param con DBI connection to a Montagu database, for retrieving
##' the burden_estimate set.
##' @param dalys_params A data.frame containing how to calculate dalys; each
##' row represents a condition, and the four columns describe how to calculate
##' the cost of that condition across the population in that year. For each
##' condition, the `outcome` column must provide a burden outcome (which will
##' be a column name in `data`). The `proportion` is the proportion of people
##' reported as that outcome, who suffer this condition. `average_duration` is
##' the number of years for which the condition is suffered, set to greater
##' than 120 for a life-long period. And `disability_weight` is a measure of
##' how severe the implications are of this particular condition. See
##' http://ghdx.healthdata.org/record/ihme-data/gbd-2017-disability-weights
##' for where these figures come from; the final figures are decided in
##' discussion with the groups.
##' @param modelling_group If the modelling group, disease, touchstone and
##' scenario are all provided, then we lookup the current (most recently
##' uploaded) burden estimate set matching those details.
##' @param disease If specified, the name of the disease in question
##' @param touchstone If specified, the touchstone (including version) for
##' the set.
##' @param scenario If specified, the name of the scenario for the set.
##' @param burden_estimate_set_id Alternatively, if modelling_group, disease,
##' touchstone and scenario are NULL, the numerical id of a particular burden
##' estimate set can be specified here, to extend that estimate set with
##' DALYs. Otherwise, leave as NULL to look up by the other four
##' fields.
##' @param output_file If provided, then write the output CSV with the
##' additional DALYs field to this filename. The file will be ready to be
##' uploaded to Montagu.
##' @param life_table If provided, then re-use the life table provided by a
##' previous call to this function. Otherwise, it can be left at NULL to
##' generate the life-table.
##' @param year_min The first year of the range in which to calculate DALYs.
##' (Default 2000)
##' @param year_max The final year of the range in which to calculate DALYs.
##' (Default 2100)
##' @return A list with two components, `data` is a data.frame of the complete
##' DALYs extended data, and `life_table` is the life table used for
##' calculating the DALYs, should that be required again.
stoner_dalys_for_db <- function(con, dalys_params, modelling_group = NULL,
                                disease = NULL, touchstone = NULL,
                                scenario = NULL, burden_estimate_set_id = NULL,
                                output_file = NULL, life_table = NULL,
                                year_min = 2000, year_max = 2100) {

  test_args(con, dalys_params, life_table)

  if (is.null(burden_estimate_set_id)) {
    if ((is.null(modelling_group)) || (is.null(disease)) ||
        (is.null(touchstone)) || (is.null(scenario))) {
      stop(paste("No burden_estimate_id given - need modelling_group, disease,",
                 "touchstone and scenario"))
    }
  } else {
    if ((!is.null(modelling_group)) || (!is.null(disease)) ||
        (!is.null(touchstone)) || (!is.null(scenario))) {
      stop(paste("Provide either burden_estimate_id, or ",
           "{modelling_group, disease, touchstone, scenario} - not both"))
    }
  }

  # Lookup the burden estimate set id if not provided.

  if (is.null(burden_estimate_set_id)) {
    burden_estimate_set_id <- DBI::dbGetQuery(con, "
      SELECT current_burden_estimate_set
        FROM responsibility
        JOIN responsibility_set
          ON responsibility.responsibility_set = responsibility_set.id
        JOIN scenario
          ON responsibility.scenario = scenario.id
        JOIN scenario_description
          ON scenario_description.id = scenario.scenario_description
       WHERE scenario.touchstone = $1
         AND scenario_description.disease = $2
         AND modelling_group = $3
         AND scenario.scenario_description = $4",
    list(touchstone, disease,
         modelling_group, scenario))$current_burden_estimate_set

  } else {

    meta <- DBI::dbGetQuery(con, "
      SELECT modelling_group, scenario.touchstone, disease,
             scenario_description
        FROM responsibility
        JOIN responsibility_set
          ON responsibility_set.id = responsibility.responsibility_set
        JOIN scenario
          ON responsibility.scenario = scenario.id
        JOIN scenario_description
          ON scenario_description.id = scenario.scenario_description
      WHERE responsibility.current_burden_estimate_set = $1",
                    burden_estimate_set_id)
    modelling_group <- meta$modelling_group
    touchstone <- meta$touchstone
    disease <- meta$disease
    scenario <- meta$scenario_description
  }

  # Fetch the burden estimate set, translating the burden outcome code.

  data <- DBI::dbGetQuery(con, "
    SELECT country.id as country, year, age, burden_outcome.code, value
      FROM burden_estimate
      JOIN country
        ON burden_estimate.country = country.nid
      JOIN burden_outcome
        ON burden_outcome.id = burden_outcome
     WHERE burden_estimate_set = $1
     ORDER BY country, year, age", burden_estimate_set_id)

  # Convert to wide format, with burden outcomes in columns.

  data <- split(data, data$code)

  # I am not 100% sure if split is guaranteed to keep the original order of
  # country, year, age. If it does, next line can be removed.

  data <- lapply(data, function(x) x[order(x$country, x$year, x$age), ])

  names(data[[1]])[names(data[[1]]) == 'value'] <- data[[1]]$code[1]
  data[[1]]$code <- NULL

  for (page in 2:length(data)) {
    data[[1]][[data[[page]]$code[page]]] <- data[[page]]$value
  }
  data <- data[[1]]

  # Now calculate DALYs.

  data_dalys <- stoner_calculate_dalys(con, touchstone, data, dalys_params,
                                       life_table, year_min, year_max)

  # And add columns (disease, country_name) necessary for uploading
  # back into Montagu.

  data_dalys$data$disease <- disease

  countries <- DBI::dbGetQuery(con, "SELECT * FROM country")
  data_dalys$data$country_name <- countries$name[
    match(data_dalys$data$country, countries$id)]

  # Re-order columns for Montagu.

  cols <- names(data_dalys$data)
  cols <- cols[!cols %in% c("disease", "year", "age", "country",
                            "country_name", "cohort_size")]
  data_dalys$data <- data_dalys$data[, c("disease", "year", "age", "country",
                                         "country_name", "cohort_size", cols)]

  # Optionally write a CSV file out.

  if (!is.null(output_file)) {
    write.csv(data_dalys$data, output_file, row.names = FALSE)
  }

  data_dalys
}
