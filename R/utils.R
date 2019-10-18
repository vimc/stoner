read_meta <- function(path, filename) {
  read.csv(file.path(path, "meta", filename),
           stringsAsFactors = FALSE)
}

meta_exists <- function(path, filename) {
  file.exists(file.path(path, "meta", filename))
}

data_frame <- function(...) {
  data.frame(stringsAsFactors = FALSE, ...)
}

sql_in_char <- function(strings) {
  paste0("('", paste(strings, collapse = "','"), "')")
}

sql_in_numeric <- function(numerics) {
  paste0("(", paste(numerics, collapse = ","), ")")
}

sql_in <- function(things) {
  if (is.character(things)) sql_in_char(things)
  else if (is.numeric(things)) sql_in_numeric(things)
  else stop("Can't convert things with sql_in")
}

db_get <- function(con, table, id_field = NULL, id_values = NULL, select = "*") {
  sql <- sprintf("SELECT %s FROM %s", select, table)
  if (!is.null(id_field)) {
    sql <- sprintf("%s WHERE %s IN %s", sql, id_field, sql_in(id_values))
  }
  DBI::dbGetQuery(con, sql)
}

next_id<- function(con, table, id_field) {
  1L + as.numeric(DBI::dbGetQuery(con,
    sprintf("SELECT max(%s) FROM %s", table, id_field)))
}

# Return a vector of logicals, of whether each row in table1
# occurs somewhere in table 2.

non_unique <- function(table1, table2) {

  mash <- function(tab) {
    tab <- tab[, order(names(tab))]
    for (r in seq_len(nrow(tab))) {
      tab$all_fields_mashed <- paste(as.character(tab[r, ]), collapse = '#')
    }
    tab
  }

  is.na(match(mash(table1)$all_fields_mashed, mash(table2)$all_fields_mashed))

}


fill_in_keys <- function(csv, db_table, csv_field, db_field) {

}

# If a .csv file has been provided, then load that csv file,
# the accompanying database table (filtered by id from the csv
# file), and if the id is numeric, also find the first free
# numerical id.
#
# Result: a list of <table>, <table>_csv and potentially <table>_next_id

extract_table <- function(table, id_field = NULL) {

  ret <- list()
  if (meta_exists(path, paste0(table, ".csv"))) {

    csv <- read_meta(path, paste0(table, ".csv"))
    ret[[paste0(table, "_csv")]] <- csv

    ids <- NULL
    if (!is.null(id_field)) ids <- csv[[id_field]]

    ret[[table]] <- db_get(con, table, id_field, ids)

    if ((!is.null(id_field)) && (is.numeric(csv[[id_field]]))) {
      ret[[paste0(table, "_next_id")]] <- next_id(con, table, id_field)
    }
  }
  ret
}

# If the given table is in the extracted_data,
# create a copy of it, and set the already_exists_db
# to indicate whether the row is already present in
# the database or not.

copy_unique_flag <- function(extracted_data, tab) {
  t <- list()
  if (tab %in% names(extracted_data)) {
    t[[tab]] <- e[[paste0(tab, "_csv")]]
    t[[tab]]$already_exists_db <- non_unique(t[[tab]], e[[tab]])
  }
  t
}

