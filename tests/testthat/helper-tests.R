test_path <- function(context, path) {
  file.path("examples", context, path)
}

cache <- new.env(parent = emptyenv())

new_test <- function(clear_db = TRUE, clear_files = TRUE) {
  # Create stoner_test/meta in a temporary dir,
  # delete any existing files within it.
  # Also get new db connection, and
  # empty any leftover rows from previous tests.

  res <- list()
  path <- tempdir()
  res$path <- file.path(path, "stoner_test")
  dir.create(res$path, showWarnings = FALSE)
  inner <- file.path(res$path, "meta")
  dir.create(inner, showWarnings = FALSE)
  files <- dir(inner, full.names = TRUE)
  if (clear_files) {
    file.remove(files)
  }
  cache$con <- cache$con %||% test_db_connection()
  res$con <- cache$con
  if (clear_db) {
    DBI::dbExecute(res$con, "UPDATE responsibility
                                SET current_burden_estimate_set = NULL")
    DBI::dbExecute(res$con, "DELETE FROM burden_estimate")
    DBI::dbExecute(res$con, "DELETE FROM burden_estimate_set")
    DBI::dbExecute(res$con, "DELETE FROM app_user")
    DBI::dbExecute(res$con, "DELETE FROM model_run_parameter_set")
    DBI::dbExecute(res$con, "DELETE FROM model_version")
    DBI::dbExecute(res$con, "DELETE FROM model")
    DBI::dbExecute(res$con, "DELETE FROM upload_info")
    DBI::dbExecute(res$con, "DELETE FROM demographic_statistic")
    DBI::dbExecute(res$con, "DELETE FROM touchstone_demographic_dataset")
    DBI::dbExecute(res$con, "DELETE FROM demographic_dataset")
    DBI::dbExecute(res$con, "DELETE FROM demographic_statistic_type")
    DBI::dbExecute(res$con, "DELETE FROM demographic_variant")
    DBI::dbExecute(res$con, "DELETE FROM demographic_source")
    DBI::dbExecute(res$con, "DELETE FROM touchstone_country")
    DBI::dbExecute(res$con, "DELETE FROM burden_estimate_country_expectation")
    DBI::dbExecute(res$con, "DELETE FROM burden_estimate_outcome_expectation")
    DBI::dbExecute(res$con, "DELETE FROM responsibility")
    DBI::dbExecute(res$con, "DELETE FROM burden_estimate_expectation")
    DBI::dbExecute(res$con, "DELETE FROM responsibility_set")
    DBI::dbExecute(res$con, "DELETE FROM scenario")
    DBI::dbExecute(res$con, "DELETE FROM scenario_description")
    DBI::dbExecute(res$con, "DELETE FROM scenario_type")
    DBI::dbExecute(res$con, "DELETE FROM disease")
    DBI::dbExecute(res$con, "DELETE FROM touchstone")
    DBI::dbExecute(res$con, "DELETE FROM touchstone_name")
    DBI::dbExecute(res$con, "DELETE FROM modelling_group")

    # Scramble serial ids

    tables <- c("scenario", "responsibility", "responsibility_set",
                "burden_estimate_expectation")
    for (table in seq_along(tables)) {
      DBI::dbExecute(res$con, sprintf("SELECT setval('%s_id_seq', %s)",
        tables[table], (1000 * table)))
    }
  }

  res
}

clear_files <- function(test) {
  files <- list.files(file.path(test$path, "meta"))
  unlink(file.path(test$path, "meta", files))
}

test_prepare <- function(path, con = NULL) {
  db_tables <- c("touchstone_name", "touchstone", "disease",
                 "scenario_type", "scenario_description", "scenario",
                 "demographic_variant", "demographic_source",
                 "demographic_statistic_type",
                 "demographic_dataset",
                 "touchstone_demographic_dataset",
                 "touchstone_country", "modelling_group",
                 "responsibility_set", "burden_estimate_expectation",
                 "responsibility", "burden_estimate_country_expectation",
                 "burden_estimate_outcome_expectation")

  i <- 0
  for (table in db_tables) {
    i <- i + 1
    csv_file <- read_meta(path, sprintf("db_%s.csv", table))
    if (!is.null(csv_file)) {
      DBI::dbWriteTable(con, table, csv_file, append = TRUE)
    }
    if (("id" %in% names(csv_file)) &&
       (is.numeric(csv_file$id))) {
      DBI::dbExecute(con, sprintf("SELECT setval('%s_id_seq', %s)",
        table, (100 * i) + max(as.integer(csv_file$id))))
    }
  }
}

test_compare_csv_db <- function(con, csv, db) {
  tab <- DBI::dbReadTable(con, db)
  expect_true(all(sort(names(tab)) == sort(names(csv))),
              label = "DB Compare: Column names match")

  expect_equal(nrow(tab), nrow(csv),
               label = "DB Compare: Row count matches")

  cols <- sort(names(tab))
  tab$mash <- mash(tab)
  csv$mash <- mash(csv)
  expect_true(all(sort(tab$mash) == sort(csv$mash)),
              label = "DB Compare: Data matches")
}

compare_csv <- function(res, tables) {
  expect_true(all(vapply(seq_along(tables), function(ti) {
    table <- tables[ti]
    test_compare_csv_db(
      res$con, res$e[[sprintf("%s_csv",table)]], table)}, FALSE)))
}

mess_with <- function(path, csv, col, row, text) {
  data <- read_csv(file.path(path, "meta", csv))
  if (row == 0) {
    names(data)[names(data) == col] <- text
  } else {
    data[[col]][row] <- text
  }
  write_csv(data, file.path(path, "meta", csv))
}

db_file <- function(db, f) {
  if (!db) f else sprintf("db_%s", f)
}

create_touchstone_csv <- function(path, names, versions,
                                  descriptions = NULL,
                                  comments = NULL,
                                  status = NULL,
                                  db = FALSE) {

  comments <- comments %||% sprintf("Comment %s-%s", names, versions)
  descriptions <- descriptions %||% sprintf("%s description", names)
  status <- status %||% "in-preparation"
  write_csv(data_frame(
    id = sprintf("%s-%s", names, versions),
    touchstone_name = names,
    version = versions,
    description = sprintf("%s (version %s)", names, versions),
    status = status,
    comment = comments),
    file.path(path, "meta", db_file(db, "touchstone.csv")))
}

create_touchstone_name_csv <- function(path, names,
                                       descriptions = NULL,
                                       comments = NULL,
                                       db = FALSE) {

  descriptions <- descriptions %||% sprintf("%s description", names)
  comments <- comments %||% sprintf("%s comment", names)

  invisible(write_csv(data_frame(
    id = names,
    description = descriptions,
    comment = sprintf("%s comment", names)),
    file.path(path, "meta", db_file(db, "touchstone_name.csv"))))
}

create_disease_csv <- function(path, ids, names, db = TRUE) {
  write_csv(data_frame(
    id = ids, name = names),
    file.path(path, "meta", db_file(db, "disease.csv")))
}

create_scenario_csv <- function(path, ids, touchstones, sds, db = FALSE) {
  write_csv(data_frame(
    id = ids, touchstone = touchstones,
    scenario_description = sds, focal_coverage_set = NA),
    file.path(path, "meta", db_file(db, "scenario.csv")))
}

create_scen_type_csv <- function(path, ids, names, db = FALSE) {
  write_csv(data_frame(
    id = ids, name = names),
    file.path(path, "meta", db_file(db, "scenario_type.csv")))
}

create_scen_desc_csv <- function(path, ids, descs, diseases, types, db = FALSE) {
  write_csv(data_frame(
    id = ids, description = descs, disease = diseases, scenario_type = types),
    file.path(path, "meta", db_file(db, "scenario_description.csv")))
}

create_ts_country_csv <- function(path, tstones, diseases,
                                  countries, db = FALSE) {
  write_csv(data_frame(
    touchstone = tstones, disease = diseases, country = countries),
    file.path(path, "meta", db_file(db, "touchstone_country.csv")))
}

create_ts_dds <- function(path, tstones, sources, types, db = FALSE) {
  write_csv(data_frame(
    touchstone = tstones, demographic_source = sources,
    demographic_statistic_type = types),
    file.path(path, "meta",
      db_file(db, "touchstone_demographic_dataset.csv")))
}

create_modelling_groups <- function(path, ids, institutions, pis, descriptions,
                                    comments, replaced_bys, db = TRUE) {

  write.csv(data_frame(id = ids, institution = institutions,
                       pi = pis, description = descriptions,
                       comment = comments, replaced_by = replaced_bys),

    file.path(path, "meta", db_file(db, "modelling_group.csv")),
              row.names = FALSE)
}


standard_demography <- function(test, make_source = TRUE,
                                      make_type = TRUE,
                                      make_dataset = TRUE,
                                      rows = 1) {
  vid <- DBI::dbGetQuery(test$con, "
    INSERT INTO demographic_variant (code, name) VALUES ('V1', 'Variant 1')
      RETURNING id")$id

  src <- NA
  if (make_source) {
    src <- vapply(seq_len(rows), function(r) {
      DBI::dbGetQuery(test$con, sprintf("
        INSERT INTO demographic_source (code, name)
             VALUES ('S%s', 'Source %s')
          RETURNING id", r, r))$id }, 0)
  }
  type <- NA
  if (make_type) {
    type <- vapply(seq_len(rows), function(r) {
      DBI::dbGetQuery(test$con, sprintf("
        INSERT INTO demographic_statistic_type
          (code, age_interpretation, name, year_step_size, reference_date,
           gender_is_applicable, demographic_value_unit, default_variant) VALUES
           ('T%s', 'age', 'Type %s', 5, '2017-01-01', FALSE, 1, $1)
           RETURNING id", r, r), vid)$id }, 0)
  }
  dset <- NA
  if (make_dataset & make_type & make_source) {
    dset <- vapply(seq_len(rows * rows), function(r) {
      src_i <- 1 + ((r - 1) %/% rows)
      type_i <- 1 + ((r - 1) %% rows)

      DBI::dbGetQuery(test$con, sprintf("
        INSERT INTO demographic_dataset
          (description, demographic_source,
                        demographic_statistic_type) VALUES
          ('D%s', $1, $2) RETURNING id", r),
                      list(src[src_i], type[type_i]))$id }, 0)
  }
  list(variant_id = vid, source_id = src,
       type_id = type, dset_id = dset)
}

standard_disease <- function(test) {
  create_disease_csv(test$path, c("flu", "piles"),
                                c("Elf flu", "Elf piles"), db = TRUE)
}

standard_disease_touchstones <- function(test, db = TRUE) {
  standard_disease(test)
  create_touchstone_csv(test$path, c("nevis", "kili"),
                                   c(1, 1), db = db)
  create_touchstone_name_csv(test$path,
                                   c("nevis", "kili"), db = db)
}

standard_modelling_groups <- function(test) {
  create_modelling_groups(test$path,
                          c("LAP-elf", "EBHQ-bunny", "R-deer"),
                          c("Lapland Epi Centre", "EBHQ", "Lapland zoo"),
                          c("Santa Claus", "Easter Bunny", "A reindeer"),
                          c("Lapland Epi (chief-elf)",
                            "Easter Bunny Head Quarters (bunny-minion)",
                            "Lapland Reindeer Research Centre"),
                          c(NA, NA, NA),
                          c(NA, NA, NA))
}

standard_responsibility_support <- function(test, db = TRUE) {
  standard_modelling_groups(test)

  create_scen_type_csv(test$path,
                       c("type1", "type2"),
                       c("Best type", "Even better type"), db = TRUE)

  create_scen_desc_csv(test$path,
    c("hot_chocolate", "pies", "mistletoe", "holly"),
    c("campaign", "routine", "campaign", "routine"),
    c("flu", "flu", "piles", "flu"),
    c("type1", "type2", "type1", "type2"), db = TRUE)

  create_scenario_csv(test$path, 1, "nevis-1", "hot_chocolate", db = TRUE)
}

create_responsibilities <- function(test, resp, db = FALSE) {

  df <- data_frame(
    modelling_group = resp$modelling_group,
    disease = resp$disease,
    touchstone = resp$touchstone,
    scenario = resp$scenario,
    scenario_type = resp$scenario_type,
    age_min_inclusive = resp$age_min_inclusive,
    age_max_inclusive = resp$age_max_inclusive,
    cohort_min_inclusive = resp$cohort_min_inclusive,
    cohort_max_inclusive = resp$cohort_max_inclusive,
    year_min_inclusive = resp$year_min_inclusive,
    year_max_inclusive = resp$year_max_inclusive,
    countries = resp$countries,
    outcomes = resp$outcomes
  )

  write.csv(df,
            file.path(test$path, "meta",
                      db_file(db, "responsibilities.csv")),
            row.names = FALSE
  )
}

default_responsibility <- function() {
  data_frame(
    modelling_group = "LAP-elf",
    disease = "flu",
    touchstone = "nevis-1",
    scenario = "pies",
    scenario_type = "standard",
    age_min_inclusive = 0,
    age_max_inclusive = 100,
    cohort_min_inclusive = 1900,
    cohort_max_inclusive = 2100,
    year_min_inclusive = 2000,
    year_max_inclusive = 2100,
    countries = "AFG;ZWE",
    outcomes = "cases;deaths"
  )
}

valid_certificate <- function(con, path) {

  resp_set <- DBI::dbGetQuery(con,
                              "SELECT * FROM responsibility_set LIMIT 1")

  upload_info <- DBI::dbGetQuery(con, "
    INSERT INTO upload_info (uploaded_by, uploaded_on)
    VALUES ('comet', NOW()) RETURNING id")$id

  DBI::dbExecute(con, "
    INSERT INTO model (id, modelling_group, description, disease,
                       gender_specific)
          VALUES ('test_model', 'LAP-elf', 'Description', 'flu', FALSE)")

  model_version <- DBI::dbGetQuery(con, "
    INSERT INTO model_version (model, version)
          VALUES ('test_model', 1) RETURNING id")

  mrps_id <- DBI::dbGetQuery(con, sprintf("
    INSERT INTO model_run_parameter_set
                (responsibility_set, upload_info, model_version)
        VALUES (%s, %s, %s) RETURNING id",
                                               resp_set$id, upload_info, model_version))$id


  new_file <- tempfile(fileext = ".json", tmpdir = path)
  dummy <- sprintf('[
    {
      "id": %s,
      "disease": "flu",
      "uploaded_by": "comet",
      "uploaded_on": "2020-07-27T15:14:27.969Z"
    },
    {
      "signature": "sigdata"
    }
  ]', mrps_id)

  writeLines(dummy, new_file)
  basename(new_file)
}

test_run_import <- function(path, con = NULL, ...) {
  e <- stone_extract(path, con)
  stone_test_extract(e)
  t <- stone_transform(e)
  stone_test_transform(t)
  l <- stone_load(t, con, ...)
  list(e = e, t = t)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

do_test <- function(test, ...) {
  test_prepare(test$path, test$con)
  c(test_run_import(test$path, test$con, ...), con = test$con)
}

# Create a new, empty stoner_dump temp folder.

empty_dump <- function() {
  tmp <- file.path(tempdir(), "stoner_dump")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, showWarnings = FALSE)
  tmp
}
