test_path <- function(context, path) {
  file.path("examples", context, path)
}

cache <- new.env(parent = emptyenv())

new_test <- function() {
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
  file.remove(files)
  cache$con <- cache$con %||% test_db_connection()
  res$con <- cache$con
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
  DBI::dbExecute(res$con, "DELETE FROM disease")
  DBI::dbExecute(res$con, "DELETE FROM touchstone")
  DBI::dbExecute(res$con, "DELETE FROM touchstone_name")
  DBI::dbExecute(res$con, "DELETE FROM modelling_group")
  res
}

clear_files <- function(test) {
  files <- list.files(file.path(test$path, "meta"))
  unlink(file.path(test$path, "meta", files))
}

test_prepare <- function(path, con = NULL) {
  db_tables <- c("touchstone_name", "touchstone", "disease",
                 "scenario_description", "scenario",
                 "demographic_variant", "demographic_source",
                 "demographic_statistic_type",
                 "demographic_dataset",
                 "touchstone_demographic_dataset",
                 "touchstone_country", "modelling_group",
                 "responsibility_set", "burden_estimate_expectation",
                 "responsibility", "burden_estimate_country_expectation",
                 "burden_estimate_outcome_expectation")

  for (table in db_tables) {
    csv_file <- read_meta(path, sprintf("db_%s.csv", table))
    if (!is.null(csv_file)) {
      DBI::dbWriteTable(con, table, csv_file, append = TRUE)
    }
    if (("id" %in% names(csv_file)) &&
       (is.numeric(csv_file$id))) {
      DBI::dbExecute(con, sprintf("SELECT setval('%s_id_seq', %s)",
        table, max(as.integer(csv_file$id))))
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

create_scen_desc_csv <- function(path, ids, descs, diseases, db = FALSE) {
  write_csv(data_frame(
    id = ids, description = descs, disease = diseases),
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

standard_disease_touchstones <- function(test, db = TRUE) {
  create_disease_csv(test$path, "flu", "Elf flu", db = TRUE)
  create_touchstone_csv(test$path, "nevis", 1, db = db)
  create_touchstone_name_csv(test$path, "nevis", db = db)
}

standard_responsibility_support <- function(test, db = TRUE) {
  create_modelling_groups(test$path,
    c("LAP-elf", "EBHQ-bunny"),
    c("Lapland Epi Centre", "EBHQ"),
    c("Santa Claus", "Easter Bunny"),
    c("Lapland Epi (chief-elf)", "Easter Bunny Head Quarters (bunny-minion)"),
    c(NA, NA),
    c(NA, NA))

  create_scen_desc_csv(test$path,
    c("hot_chocolate", "pies"),
    c("campaign", "routine"), "flu", db = TRUE)

  create_scenario_csv(test$path, 1, "nevis-1", "hot_chocolate", db = TRUE)
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


