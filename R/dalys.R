stoner_calculate_dalys <- function(con, touchstone, data, dalys_params, life_table = NULL,
                                   year_min = 2000, year_max = 2100) {

  # Accept countries as either character or numerical id.

  char_countries <- is.character(data$country[1])

  res <- list()

  # Accept either a previously cached life-table (as it takes a few seconds), or generate
  # it if called with NULL.

  if (is.null(life_table)) {
    life_table <- stoner_life_table(con, touchstone, year_min, year_max, char_countries)
    res$life_table <- life_table
  }

  # Factor out a multiplication from the dalys_params, as we do this every time.

  dalys_params$adjusted_weight <- dalys_params$disability_weight * dalys_params$proportion

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

stoner_life_table <- function(con, touchstone, year_min, year_max, char_countries = TRUE) {

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
       ON demographic_statistic_type.id = demographic_statistic.demographic_statistic_type
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

#############################################################################
test_dalys <- function() {


get_burden_estimates <- function(id) {
  data <- DBI::dbGetQuery(con, "SELECT country.id as country, year, age, burden_outcome.code, value
                          FROM burden_estimate
                          JOIN country
                            ON burden_estimate.country = country.nid
                          JOIN burden_outcome
                            ON burden_outcome.id = burden_outcome
                         WHERE burden_estimate_set = $1
                      ORDER BY country, year, age", id)


  data <- split(data, data$code)

  # I am not 100% sure if split is guaranteed to keep the original order of
  # country, year, age. If it does, next line can be removed.

  data <- lapply(data, function(x) x[order(x$country, x$year, x$age), ])

  names(data[[1]])[names(data[[1]]) == 'value'] <- data[[1]]$code[1]
  data[[1]]$code <- NULL

  for (page in 2:length(data)) {
    data[[1]][[data[[page]]$code[page]]] <- data[[page]]$value
  }
  data[[1]]
}

data_rota_novac <- stoner_calculate_dalys(con, "201910gavi-5", get_burden_estimates(1807), list_params_rota)
data_rota_routine <- stoner_calculate_dalys(con, "201910gavi-5", get_burden_estimates(1806), list_params_rota)
data_rota_best <- stoner_calculate_dalys(con, "201910gavi-5", get_burden_estimates(1805), list_params_rota)
data_rota_novac$disease <- "Rota"
data_rota_routine$disease <- "Rota"
data_rota_best$disease <- "Rota"
countries <- DBI::dbGetQuery(con, "SELECT * FROM country")

data_rota_novac$country_name <- countries$name[match(data_rota_novac$country, countries$id)]
data_rota_best$country_name <- countries$name[match(data_rota_best$country, countries$id)]
data_rota_routine$country_name <- countries$name[match(data_rota_routine$country, countries$id)]

data_rota_novac <- data_rota_novac[, c("disease", "year", "age", "country", "country_name", "cohort_size", "cases", "deaths", "dalys")]
data_rota_best <- data_rota_best[, c("disease", "year", "age", "country", "country_name", "cohort_size", "cases", "deaths", "dalys")]
data_rota_routine <- data_rota_routine[, c("disease", "year", "age", "country", "country_name", "cohort_size", "cases", "deaths", "dalys")]

write.csv(data_rota_novac, "list_rota_novac.csv", row.names = FALSE)
write.csv(data_rota_best, "list_rota_best.csv", row.names = FALSE)
write.csv(data_rota_routine, "list_rota_routine.csv", row.names = FALSE)

}
