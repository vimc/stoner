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

next_id <- function(con, table, id_field) {
  1L + as.numeric(DBI::dbGetQuery(con,
    sprintf("SELECT max(%s) FROM %s", id_field, table)))
}

# If a .csv file has been provided, then load that csv file,
# the accompanying database table (filtered by id from the csv
# file), and if the id is numeric, also find the first free
# numerical id.
#
# Result: a list of <table>, <table>_csv and potentially <table>_next_id

extract_table <- function(path, con, table, id_field = NULL) {

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


# Return a vector of characters for a table, each entry being all the fields
# of that table mashed together, separated by '#'

mash <- function(tab) {
  fields <- sort(names(tab))
  df_args <- c(tab, sep = "#")
  do.call(paste, df_args)
}

# Return a vector of logicals, of whether each row in table1
# occurs somewhere in table 2.

line_occurs_in <- function(table1, table2) {
  if (nrow(table2) == 0) {
    FALSE
  } else {
    mash(table1) %in% mash(table2)
  }

}

# If the given table is in the extracted_data,
# create a copy of it, and set the already_exists_db
# to indicate whether the row is already present in
# the database or not.

copy_unique_flag <- function(extracted_data, tab) {
  t <- list()
  if (tab %in% names(extracted_data)) {
    t[[tab]] <- extracted_data[[paste0(tab, "_csv")]]
    t[[tab]]$already_exists_db <- line_occurs_in(t[[tab]], extracted_data[[tab]])
  }
  t
}

# For each row in csv_table, does it exist in db_table?
# If so, set id_field in csv_table to the matching id in db_table.
# If not, assign new key for that row.

fill_in_keys <- function(csv_table, db_table, id_field, next_id) {

  db_table$mash <- mash(db_table[, names(db_table) != id_field])
  csv_table$mash <- mash(csv_table)

  # Copy existing keys

  csv_table[[id_field]] <- db_table[[id_field]][match(csv_table$mash, db_table$mash)]

  csv_table <- csv_table[, names(csv_table) != 'mash']

  csv_table$already_in_db <- !is.na(csv_table$id)

  # For any NAs, assign new keys, starting at next_id

  which_nas <- which(is.na(csv_table[[id_field]]))

  csv_table[[id_field]][which_nas] <- seq(from = next_id, by = 1,
                                          length.out = length(which_nas))

  csv_table

}

# Add any rows in transformed_data[[table_name]] to the data where the id
# does not already exist in that table. Return the leftovers, which are
# edits to the table.

add_return_edits <- function(table_name, transformed_data, con) {
  data <- transformed_data[[table_name]]
  ids_found <- db_get(con, table_name, "id", data$id, "id")$id
  to_add <- data[!data$id %in% ids_found, ]

  if (nrow(to_add) > 0) {
    DBI::dbWriteTable(con, table_name, to_add, append = TRUE)
  }

  data[data$id %in% ids_found, ]
}
