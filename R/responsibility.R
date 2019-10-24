###############################################################################

extract_responsibility <- function(path, con) {
  e <- list()

  e[['responsibility_csv']] <- read_meta(path, "responsibility.csv")

  e[['responsibility']] <- DBI::dbGetQuery(con, sprintf("
    SELECT *
      FROM responsibility
      JOIN scenario
        ON responsibility.scenario = scenario.id
     WHERE scenario.touchstone IN %s",
    sql_in(unique(e$responsibility_csv$touchstone))))

  e[['responsibility_next_id']] <- next_id(con, "responsibility", "id")

  e[['scenario']] <- db_get(con, "scenario", "touchstone",
                            e$responsibility_csv$touchstone)


  e[['responsibility_set']] <- db_get(con, "responsibility_set", "touchstone",
                                      e$responsibility_csv$touchstone)

}

test_extract_responsibility <- function(e) {
  expect_equal(sort(names(e$responsibility_csv)),
               c("modelling_group", "scenario", "touchstone"))
}

###############################################################################

transform_responsibility <- function(e, t_so_far) {

  transform_responsibility_set <- function(e) {
    responsibility_set <- data_frame(
      modelling_group = e$responsibility_csv$modelling_group,
      touchstone = e$responsibility_csv$touchstone)

    responsibility_set$id <- mash_id(responsibility_set, e$responsibility_set,
                                     c("modelling_group", "touchstone"))
    responsibility_set$already_exists_db <- !is.na(responsibility_set$id)
    responsibility_set
  }

  transform_scenario <- function(e) {
    all_scenarios <- strsplit(e$responsibility_csv$scenario, ";")

    scenario <- data_frame(
      touchstone = rep(e$responsibility_csv$touchstone,
                       times = lengths(all_scenarios)),
      scenario_description = unlist(all_scenarios))

    scenario$mash <- paste(scenario$scenario_description,
                           scenario$touchstone, sep = '#')
    scenario <- scenario[!duplicated(scenario$mash), ]
    scenario$mash <- NULL
    scenario$id <- mash_id(scenario, e$scenario,
                           c("touchstone", "scenario_description"))
    scenario$already_exists_db <- !is.na(scenario$id)
    scenario$focal_coverage_set <- e$scenario$focal_coverage_set[
      match(scenario$id, e$scenario$id)]

    scenario
  }

  transform_responsibility_table <- function(e, t_so_far) {

    # Build the responsibility table itself.
    # Cols: id, responsiblity_set, scenario (numerical),
    # current_burden_stimate_Set, current_stochastic_burden_estimate_set,
    # is_open, expectation

    # The CSV provides us with touchstone,modelling_group,scenario (desc)
    # (scenario is semi-colon separated.

    modelling_groups <- e$responsibility_csv$modelling_group
    touchstones <- e$responsibility_csv$touchstone
    all_scenarios <- strsplit(e$responsibility_csv$scenario, ";")

    responsibility <- data_frame(
      touchstone = rep(touchstones, times = lengths(all_scenarios)),
      modelling_group = rep(modelling_groups, times = lengths(all_scenarios)),
      scenario_description = unlist(all_scenarios))

    # Convert scenario_description into numerical id.

    responsibility$scenario <- mash_id(responsibility, e$scenario,
                                 c("touchstone", "scenario_description"))

    # Responsibility_set was previously calculated, and is modelling_group &
    # touchstone specific.

    responsibility$responsibility_set <-
      mash_id(responsibility, t$responsibility_set,
              c("modelling_group", "touchstone"))

    # Fetch matching current_burden_estimate_set from database

    responsibility$current_burden_estimate_set <-
      mash_id(responsibility, e$responsibility, "id",
              "current_burden_estimate_set")

    # Fetch matching current_stochastic_burden_estimate_set from database

    responsibility$current_stochastic_burden_estimate_set <-
      mash_id(responsibility, e$responsibility, "id",
              "current_stochastic_burden_estimate_set")

    # Fetch is_open status from database

    responsibility$is_open <-
      mash_id(responsibility, e$responsibility, "id", "is_open")

    # Expectations we have to work harder with. The expectations may
    # be in t_so_far$burden_estimate_expectation, or they may be
    # in the database.

    new_exps <- t_so_far$burden_estimate_expectation
    new_exps <- new_exps[!new_exps$already_exists_db, ]
    new_exps$already_exists_db <- NULL
    all_expectations <- rbind(e$burden_estimate_expectation,
                              new_exps)

    # e$expections_csv contains touchstone, modelling_group and
    # scenario cols - scenario might be wild-card (*) for all
    # scenarios.
    responsibility$expectations <- NA

    for (r in seq_len(nrow(responsibility))) {
      resp <- responsibility[r, ]

      matches <- e$expectations_csv[
        e$expectations_csv$modelling_group == resp$modelling_group &
        e$expectations_csv$touchstone == resp$touchstone,
      ]

      # Single row (either wildcard (*) or multi-scenarios)

      if (nrow(matches) == 1) {
        if ((!matches$scenario == "*") &&
           (!resp$scenario %in% unlist(strsplit(matches$scenario)))) {
             stop(sprintf("No expectation for %s, %s, %s",
               resp$modelling_group, resp$touchstone,
               resp$scenario_description))
        }

        responsibility$expectations[r] <-
          all_expectations$id[
            all_expectations$description == matches$description]

      # Multiple rows. Should be one matching row/entry.

      } else {
        all_scenarios <- strsplit(matches$scenario, ";")
        index <- which(unlist(lapply(all_scenarios,
            function(x) resp$scenario_description %in% x)))

        if (length(index) != 1) {
          stop(sprintf("Error finding expectation for %s %s %s",
            resp$modelling_group, resp$touchstone,
            resp$scenario_description))
        }

        responsibility$expectations[r] <-
          all_expectations$id[
            all_expectations$description == matches$description[index]]
      }
    }

    # Fetch matching ids from database, verifying against any
    # changes of expectation

    responsibility$id <- mash_id(responsibility, e$responsibility,
            c("responsibility_set", "scenario", "expectations"))

    # Allocate new ids for any NAs. Editing not really possible
    # with responsibilities. Perhaps we need to support deletion
    # for in-preparation touchstones.

    which_nas <- which(is.na(responsibility$id))
    responsibility$id[which_nas] <- seq(
      from = e$responsibility_next_id,
        by = 1, length.out = length(which_nas))

    responsibility
  }

  t <- list()
  t$responsibility_set <- transform_responsibility_set(e)
  t$scenario <- transform_scenario(e)
  t[['responsibility']] <- transform_responsibility_table(e, t_so_far)

  t
}

test_transform_responsibility <- function(t) {
  expect_false(any(is.na(t$scenario$id)))
  expect_false(any(is.na(t$responsibility)))
  expect_false(any(is.na(t$responsibility_set)))
}

###############################################################################

load_responsibility <- function(transformed_data, con) {

  load_scenario <- function(transformed_data, con) {
    to_edit <- add_return_edits("scenario", transformed_data, con)
  }

  load_responsibility_set <- function(transformed_data, con) {
    to_edit <- add_return_edits("responsibility_set", transformed_data, con)
  }

  load_responsibility_table <- function(transformed_data, con) {
    to_edit <- add_return_edits("responsibility", transformed_data, con)
  }

  load_scenario(transformed_data, con)
  load_responsibility_set(transformed_data, con)
  load_responsibility_table(transformed_data, con)
}
