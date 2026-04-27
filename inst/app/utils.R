# Helper functions to dig information from the
# shared file system.

# Functions ending in _ts will find things common
# between two touchstones, for comparisons.

# Functions ending in _mg will find things common
# between to modelling groups, for comparisons.

meta <- read.csv(file.path(data_dir, "meta.csv"))

get_touchstones <- function() {
  sort(unique(meta$touchstone), decreasing = TRUE)
}

get_diseases <- function(touchstone1, touchstone2 = NULL) {
  res <- meta$disease[meta$touchstone %in% touchstone1]
  if (!is.null(touchstone2)) {
    res <- intersect(res, meta$disease[meta$touchstone %in% touchstone2])
  }
  sort(unique(res))
}

get_groups <- function(touchstone1, touchstone2, disease) {
  m <- meta[meta$disease %in% disease, ]
  res <- m$group[m$touchstone %in% touchstone1]
  if (!is.null(touchstone2)) {
    res <- intersect(res, m$group[m$touchstone %in% touchstone2])
  }
  sort(unique(res))
}

get_scenarios <- function(touchstone1, touchstone2, disease, group1, group2) {
  lookup <- function(touchstone, disease, group, res = NULL) {
    if ((is.null(touchstone)) || (is.null(group))) return(res)
    sort(unique(meta$scenario[(meta$touchstone %in% touchstone) &
                              (meta$disease %in% disease) &
                              (meta$group %in% group)]))
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
    cts <- meta$countries[(meta$touchstone %in% touchstone) &
                          (meta$disease %in% disease) &
                          (meta$group %in% group) &
                          (meta$scenario %in% scenario)]
    sort(unique(strsplit(cts, ";")[[1]]))
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
    outs <- meta$outcomes[(meta$touchstone %in% touchstone) &
                          (meta$disease %in% disease) &
                          (meta$group %in% group) &
                          (meta$scenario %in% scenario)]
    sort(unique(strsplit(outs, ";")[[1]]))
  }

  res <- lookup(touchstone1, disease, group1, scenario1, country)
  res <- res[res %in% lookup(touchstone2, disease, group1, scenario1, country, res)]
  res <- res[res %in% lookup(touchstone1, disease, group2, scenario1, country, res)]
  res <- res[res %in% lookup(touchstone2, disease, group2, scenario1, country, res)]
  res <- res[res %in% lookup(touchstone1, disease, group1, scenario2, country, res)]
  res <- res[res %in% lookup(touchstone2, disease, group1, scenario2, country, res)]
  res <- res[res %in% lookup(touchstone1, disease, group2, scenario2, country, res)]
  res <- sort(unique(res[res %in% lookup(touchstone2, disease, group2, scenario2, country, res)]))
  res
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
