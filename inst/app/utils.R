# Helper functions to dig information from the
# shared file system.

# Functions ending in _ts will find things common
# between two touchstones, for comparisons.

# Functions ending in _mg will find things common
# between to modelling groups, for comparisons.

data_dir <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics"


get_touchstones <- function() {
  basename(list.dirs(data_dir, recursive = FALSE))
}

get_diseases <- function(touchstone) {
  path <- file.path(data_dir, touchstone)
  dirs <- basename(list.dirs(path, recursive = FALSE))
  unique(unlist(lapply(strsplit(dirs, "_"), `[[`, 1)))
}

get_diseases_ts <- function(touchstone1, touchstone2) {
  res <- get_diseases(touchstone1)
  res[res %in% get_diseases(touchstone2)]
}

get_groups <- function(touchstone, disease) {
  path <- file.path(data_dir, touchstone)
  dirs <- basename(list.dirs(path, recursive = FALSE))
  dirs <- dirs[substr(dirs, 1, nchar(disease) + 1) == paste0(disease, "_")]
  unique(substring(dirs, nchar(disease) + 2))
}

get_groups_ts <- function(touchstone1, touchstone2, disease) {
  res <- get_groups(touchstone1, disease)
  res[res %in% get_groups(touchstone2, disease)]
}

get_scenarios <- function(touchstone, disease, group) {
  path <- file.path(data_dir, touchstone, paste(disease, group, sep = "_"))
  files <- basename(list.files(path, recursive = FALSE))
  files <- gsub(".pq", "", files)
  files <- substring(files, nchar(group) + 2)
  ends <- unlist(lapply(gregexpr("_", files), `[[`, 1)) - 1
  unique(substring(files, 1, ends))
}

get_scenarios_ts <- function(touchstone1, touchstone2, disease, group) {
  res <- get_scenarios(touchstone1, disease, group)
  res[res %in% get_scenarios(touchstone2, disease, group)]
}

get_scenarios_mg <- function(touchstone, disease, group1, group2) {
  res <- get_scenarios(touchstone, disease, group1)
  res[res %in% get_scenarios(touchstone, disease, group2)]
}

get_countries <- function(touchstone, disease, group, scenario, extra_scenario = NULL) {
  scenarios <- c(scenario, extra_scenario)
  final <- NULL
  for (scenario in scenarios) {
    path <- file.path(data_dir, touchstone, paste(disease, group, sep = "_"))
    files <- basename(list.files(path, recursive = FALSE))
    files <- gsub(".pq", "", files)
    files <- substring(files, nchar(group) + 2)
    files <- files[substring(files, 1, nchar(scenario) + 1) == paste0(scenario, "_")]
    ends <- unlist(lapply(gregexpr("_", files), `[[`, 1)) + 1
    if (is.null(final)) final <- unique(substring(files, ends))
    else final <- final[final %in% unique(substring(files, ends))]
  }
  final
}

get_countries_ts <- function(touchstone1, touchstone2, disease, group, scenario) {
  res <- get_countries(touchstone1, disease, group, scenario)
  res[res %in% get_countries(touchstone2, disease, group, scenario)]
}

get_countries_mg <- function(touchstone, disease, group1, group2, scenario) {
  res <- get_countries(touchstone, disease, group1, scenario)
  res[res %in% get_countries(touchstone, disease, group2, scenario)]
}

get_outcomes <- function(touchstone, disease, group, scenario, country) {
  path <- file.path(data_dir, touchstone, paste(disease, group, sep = "_"))
  thefile <- sprintf("%s_%s_%s.pq", group, scenario, country)
  tbl <- arrow::read_parquet(file.path(path, thefile), col_select = NULL)
  cols <- names(tbl)
  sort(cols[!cols %in% c("disease", "run_id", "year", "age",
                    "country", "cohort_size")])
}

get_outcomes_ts <- function(touchstone1, touchstone2, disease, group,
                            scenario, country) {
  res <- get_outcomes(touchstone1, disease, group, scenario, country)
  res[res %in% get_outcomes(touchstone2, disease, group, scenario, country)]
}

get_outcomes_mg <- function(touchstone, disease, group1, group2, scenario,
                            country) {
  res <- get_outcomes(touchstone, disease, group1, scenario, country)
  res[res %in% get_outcomes(touchstone, disease, group2, scenario, country)]
}


# GUI helpers.

# Update a dropdown. Keep the previously selected value if possible,
# otherwise select the first one.

update_dropdown_keep <- function(session, id, choices, selected) {
  if ((is.null(selected)) ||
      (!selected %in% choices)) selected <- choices[[1]]
  updateSelectInput(session, id, choices = choices, selected = selected)
  selected
}

# The next events recurse through the dropdowns updating available
# options. In a messy attempt to avoid duplication, the prefix is
# "b" for the simple burden panel, "i" for impact,
# "bts" for multi-touchstone burden, "its" for multi-touchstone impact
# "bmg" for multi-group burden, and "img" for multi-group impact.

update_touchstone <- function(session, touchstone, input, prefix = "b") {
  d <- paste(prefix, "disease", sep = "_")
  diseases <- get_diseases(touchstone)
  disease <- update_dropdown_keep(session, d, diseases, input[[d]])
  update_disease(session, touchstone, disease, input, prefix)
}

update_touchstone_ts <- function(session, touchstone1, touchstone2,
                                 input, prefix = "bts") {
  d <- paste(prefix, "disease", sep = "_")
  diseases <- get_diseases_ts(touchstone1, touchstone2)
  disease <- update_dropdown_keep(session, d, diseases, input[[d]])
  update_disease_ts(session, touchstone1, touchstone2, disease, input, prefix)
}

update_disease <- function(session, touchstone, disease, input, prefix = "b") {
  if (is.null(disease) || (disease == "")) return()
  g <- paste(prefix, "group", sep = "_")
  groups <- get_groups(touchstone, disease)
  group <- update_dropdown_keep(session, g, groups, input[[g]])
  update_group(session, touchstone, disease, group, input, prefix)
}

update_disease_ts <- function(session, touchstone1, touchstone2, disease,
                              input, prefix = "bts") {
  if (is.null(disease) || (disease == "")) return()
  g <- paste(prefix, "group", sep = "_")
  groups <- get_groups_ts(touchstone1, touchstone2, disease)
  group <- update_dropdown_keep(session, g, groups, input[[g]])
  update_group_ts(session, touchstone1, touchstone2, disease, group,
                  input, prefix)
}

update_disease_mg <- function(session, touchstone, disease, input,
                              prefix = "bmg") {
  if (is.null(disease) || (disease == "")) return()
  g1 <- paste(prefix, "group1", sep = "_")
  g2 <- paste(prefix, "group2", sep = "_")
  groups <- get_groups(touchstone, disease)
  group1 <- update_dropdown_keep(session, g1, groups, input[[g1]])
  group2 <- update_dropdown_keep(session, g2, groups, input[[g2]])
  update_group_mg(session, touchstone, disease, group1, group2,
                  input, prefix)
}

update_group <- function(session, touchstone, disease, group, input, prefix = "b",
                         suffix = "") {
  if ((prefix == "i") && (suffix == "")) {
    suffix <- c("1", "2")
  }
  if (is.null(group) || (group == "")) return()

  for (suff in suffix) {
    sc <- sprintf("%s_scenario%s", prefix, suff)
    scenarios <- get_scenarios(touchstone, disease, group)
    scenario <- update_dropdown_keep(session, sc, scenarios, input[[sc]])
  }
  update_scenario(session, touchstone, disease, group, scenario, input, prefix)
}

update_group_ts <- function(session, touchstone1, touchstone2, disease, group,
                            input, prefix = "bts", suffix = "") {
  if ((prefix == "its") && (suffix == "")) {
    suffix <- c("1", "2")
  }
  if (is.null(group) || (group == "")) return()

  for (suff in suffix) {
    sc <- sprintf("%s_scenario%s", prefix, suff)
    scenarios <- get_scenarios_ts(touchstone1, touchstone2, disease, group)
    scenario <- update_dropdown_keep(session, sc, scenarios, input[[sc]])
  }
  update_scenario_ts(session, touchstone1, touchstone2, disease, group,
                     scenario, input, prefix)
}

update_group_mg <- function(session, touchstone, disease, group1, group2,
                            input, prefix = "bmg", suffix = "") {
  if (is.null(group1) || is.null(group2) || (group1 == "") || (group2 == "")) return()
  sc <- sprintf("%s_scenario", prefix)
  scenarios <- get_scenarios_mg(touchstone, disease, group1, group2)
  scenario <- update_dropdown_keep(session, sc, scenarios, input[[sc]])
  update_scenario_mg(session, touchstone, disease, group1, group2,
                     scenario, input, prefix)
}

update_scenario <- function(session, touchstone, disease, group, scenario, input,
                            prefix = "b", extra_scenario = NULL) {
  if (is.null(scenario) || (scenario == "")) return()
  cc <- paste(prefix, "country", sep = "_")
  countries <- get_countries(touchstone, disease, group, scenario, extra_scenario)
  country <- update_dropdown_keep(session, cc, countries, input[[cc]])
  update_country(session, touchstone, disease, group, scenario,
                 country, input, prefix)
}

update_scenario_ts <- function(session, touchstone1, touchstone2, disease, group,
                               scenario, input, prefix = "bts") {
  if (is.null(scenario) || (scenario == "")) return()
  cc <- paste(prefix, "country", sep = "_")
  countries <- get_countries_ts(touchstone1, touchstone2, disease, group,
                                scenario)
  country <- update_dropdown_keep(session, cc, countries, input[[cc]])
  update_country_ts(session, touchstone1, touchstone2, disease, group,
                    scenario, country, input, prefix)
}

update_scenario_mg <- function(session, touchstone, disease, group1, group2,
                               scenario, input, prefix = "bmg") {
  if (is.null(scenario) || (scenario == "")) return()
  if (is.null(group1) || is.null(group2) || (group1 == "") || (group2 == "")) return()
  cc <- paste(prefix, "country", sep = "_")
  countries <- get_countries_mg(touchstone, disease, group1, group2,
                                scenario)
  country <- update_dropdown_keep(session, cc, countries, input[[cc]])
  update_country_mg(session, touchstone, disease, group1, group2,
                    scenario, country, input, prefix)
}

update_country <- function(session, touchstone, disease, group, scenario, country,
                           input, prefix = "b") {
  if (is.null(country) || (country == "")) return()
  o <- paste(prefix, "outcome", sep = "_")
  outcomes <- get_outcomes(touchstone, disease, group, scenario, country)
  update_dropdown_keep(session, o, outcomes, input[[o]])
}

update_country_ts <- function(session, touchstone1, touchstone2, disease, group,
                              scenario, country, input, prefix = "bts") {
  if (is.null(country) || (country == "")) return()
  o <- paste(prefix, "outcome", sep = "_")
  outcomes <- get_outcomes_ts(touchstone1, touchstone2, disease, group,
                              scenario, country)
  update_dropdown_keep(session, o, outcomes, input[[o]])
}

update_country_mg <- function(session, touchstone, disease, group1, group2,
                              scenario, country, input, prefix = "bmg") {
  if (is.null(country) || (country == "")) return()
  if (is.null(group1) || is.null(group2) || (group1 == "") || (group2 == "")) return()
  if (is.null(scenario) || (scenario == "")) return()
  o <- paste(prefix, "outcome", sep = "_")
  outcomes <- get_outcomes_mg(touchstone, disease, group1, group2,
                              scenario, country)
  update_dropdown_keep(session, o, outcomes, input[[o]])
}
