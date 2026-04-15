# Helper functions to dig information from the
# shared file system.

# Functions ending in _ts will find things common
# between two touchstones, for comparisons.

# Functions ending in _mg will find things common
# between to modelling groups, for comparisons.

get_touchstones <- function() {
  sort(unique(basename(list.dirs(data_dir, recursive = FALSE))))
}

get_diseases <- function(touchstone1, touchstone2) {
  lookup <- function(touchstone, res = NULL) {
    if (is.null(touchstone)) return(res)
    path <- file.path(data_dir, touchstone)
    dirs <- basename(list.dirs(path, recursive = FALSE))
    unique(unlist(lapply(strsplit(dirs, "_"), `[[`, 1)))
  }
  res <- lookup(touchstone1)
  sort(unique(res[res %in% lookup(touchstone2, res)]))
}

get_groups <- function(touchstone1, touchstone2, disease) {
  lookup <- function(touchstone, disease, res = NULL) {
    if (is.null(touchstone)) return(res)
    path <- file.path(data_dir, touchstone)
    dirs <- basename(list.dirs(path, recursive = FALSE))
    dirs <- dirs[substr(dirs, 1, nchar(disease) + 1) == paste0(disease, "_")]
    unique(substring(dirs, nchar(disease) + 2))
  }
  res <- lookup(touchstone1, disease)
  sort(unique(res[res %in% lookup(touchstone2, disease, res)]))
}

get_scenarios <- function(touchstone1, touchstone2, disease, group1, group2) {
  lookup <- function(touchstone, disease, group, res = NULL) {
    if ((is.null(touchstone)) || (is.null(group))) return(res)
    path <- file.path(data_dir, touchstone, paste(disease, group, sep = "_"))
    files <- basename(list.files(path, recursive = FALSE))
    files <- gsub(".pq", "", files)
    files <- substring(files, nchar(group) + 2)
    ends <- unlist(lapply(gregexpr("_", files), `[[`, 1)) - 1
    unique(substring(files, 1, ends))
  }

  res <- lookup(touchstone1, disease, group1)
  res <- res[res %in% lookup(touchstone2, disease, group1, res)]
  res <- res[res %in% lookup(touchstone1, disease, group2, res)]
  sort(unique(res[res %in% lookup(touchstone2, disease, group2, res)]))
}

get_countries <- function(touchstone1, touchstone2, disease, group1, group2,
                          scenario1, scenario2) {
  lookup <- function(touchstone, disease, group, scenario, res = NULL) {
    if ((is.null(touchstone)) || (is.null(group)) || (is.null(scenario))) {
      return(res)
    }

    path <- file.path(data_dir, touchstone, paste(disease, group, sep = "_"))
    files <- basename(list.files(path, recursive = FALSE))
    files <- gsub(".pq", "", files)
    files <- substring(files, nchar(group) + 2)
    files <- files[substring(files, 1, nchar(scenario) + 1) == paste0(scenario, "_")]
    ends <- unlist(lapply(gregexpr("_", files), `[[`, 1)) + 1
    unique(substring(files, ends))
  }
  res <- lookup(touchstone1, disease, group1, scenario1)
  res <- res[res %in% lookup(touchstone2, disease, group1, scenario1, res)]
  res <- res[res %in% lookup(touchstone1, disease, group2, scenario1, res)]
  res <- res[res %in% lookup(touchstone2, disease, group2, scenario1, res)]
  res <- res[res %in% lookup(touchstone1, disease, group1, scenario2, res)]
  res <- res[res %in% lookup(touchstone2, disease, group1, scenario2, res)]
  res <- res[res %in% lookup(touchstone1, disease, group2, scenario2, res)]
  sort(unique(res[res %in% lookup(touchstone2, disease, group2, scenario2, res)]))
}

get_outcomes <- function(touchstone1, touchstone2, disease, group1, group2,
                         scenario1, scenario2, country) {
  lookup <- function(touchstone, disease, group, scenario, country, res = NULL) {
    if ((is.null(touchstone)) || (is.null(group)) || (is.null(scenario))) {
      return(res)
    }
    path <- file.path(data_dir, touchstone, paste(disease, group, sep = "_"))
    thefile <- sprintf("%s_%s_%s.pq", group, scenario, country)
    tbl <- arrow::read_parquet(file.path(path, thefile), col_select = NULL)
    cols <- names(tbl)
    sort(cols[!cols %in% c("disease", "run_id", "year", "age",
                           "country", "cohort_size")])
  }
  res <- lookup(touchstone1, disease, group1, scenario1, country)
  res <- res[res %in% lookup(touchstone2, disease, group1, scenario1, country, res)]
  res <- res[res %in% lookup(touchstone1, disease, group2, scenario1, country, res)]
  res <- res[res %in% lookup(touchstone2, disease, group2, scenario1, country, res)]
  res <- res[res %in% lookup(touchstone1, disease, group1, scenario2, country, res)]
  res <- res[res %in% lookup(touchstone2, disease, group1, scenario2, country, res)]
  res <- res[res %in% lookup(touchstone1, disease, group2, scenario2, country, res)]
  sort(unique(res[res %in% lookup(touchstone2, disease, group2, scenario2, country, res)]))
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

# It's messy as sometimes we have two touchstones, groups, or scenarios.

update_touchstone <- function(session, ts1, ts2, input, prefix) {
  d <- paste(prefix, "disease", sep = "_")
  diseases <- get_diseases(ts1, ts2)
  disease <- update_dropdown_keep(session, d, diseases, input[[d]])
  update_disease(session, ts1, ts2, disease, input, prefix)
}

update_disease <- function(session, ts1, ts2, disease, input, prefix) {
  if (is.null(disease) || (disease == "")) return()
  groups <- get_groups(ts1, ts2, disease)
  if (grepl("mg", prefix)) {
    g1 <- paste(prefix, "group1", sep = "_")
    g2 <- paste(prefix, "group2", sep = "_")
    group1 <- update_dropdown_keep(session, g1, groups, input[[g1]])
    group2 <- update_dropdown_keep(session, g2, groups, input[[g2]])
    update_group(session, ts1, ts2, disease, group1, group2, input, prefix)
  } else {
    g <- paste(prefix, "group", sep = "_")
    group1 <- update_dropdown_keep(session, g, groups, input[[g]])
    group2 <- NULL
  }
  update_group(session, ts1, ts2, disease, group1, group2, input, prefix)
}

update_group <- function(session, ts1, ts2, disease, g1, g2, input, prefix) {
  if (is.null(g1) || (g1 == "")) return()
  scenarios <- get_scenarios(ts1, ts2, disease, g1, g2)
  if (grepl("i", prefix)) {
    sc <- sprintf("%s_scenario1", prefix)
    s1 <- update_dropdown_keep(session, sc, scenarios, input[[sc]])
    sc <- sprintf("%s_scenario2", prefix)
    s2 <- update_dropdown_keep(session, sc, scenarios, input[[sc]])
  } else {
    sc <- sprintf("%s_scenario", prefix)
    s1 <- update_dropdown_keep(session, sc, scenarios, input[[sc]])
    s2 <- NULL
  }
  update_scenario(session, ts1, ts2, disease, g1, g2, s1, s2, input, prefix)
}

update_scenario <- function(session, ts1, ts2, disease, g1, g2, s1, s2,
                            input, prefix) {
  if (is.null(s1) || (s1 == "")) return()

  cc <- paste(prefix, "country", sep = "_")
  countries <- get_countries(ts1, ts2, disease, g1, g2, s1, s2)
  country <- update_dropdown_keep(session, cc, countries, input[[cc]])
  update_country(session, ts1, ts2, disease, g1, g2, s1, s2, country,
                 input, prefix)
}

update_country <- function(session, ts1, ts2, disease, g1, g2, s1, s2, country,
                           input, prefix) {
  if (is.null(country) || (country == "")) return()
  if (is.null(g1) || (g1 == "")) return()
  o <- paste(prefix, "outcome", sep = "_")
  outcomes <- get_outcomes(ts1, ts2, disease, g1, g2, s1, s2, country)
  update_dropdown_keep(session, o, outcomes, input[[o]])
}
