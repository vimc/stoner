read_meta <- function(path, filename) {
  if (meta_exists(path, filename)) {
    utils::read.csv(file.path(path, "meta", filename),
           stringsAsFactors = FALSE)
  } else {
    NULL
  }
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
  if (tab %in% names(extracted_data) &&
      paste0(tab, "_csv") %in% names(extracted_data)) {
    t[[tab]] <- extracted_data[[paste0(tab, "_csv")]]
    t[[tab]]$already_exists_db <- line_occurs_in(t[[tab]], extracted_data[[tab]])
  }
  t
}

# Add any rows in transformed_data[[table_name]] to the data where the id
# does not already exist in that table. Return the leftovers, which are
# edits to the table.

add_return_edits <- function(table_name, transformed_data, con) {
  if (!table_name %in% names(transformed_data)) {
    return(data_frame())
  }

  data <- transformed_data[[table_name]]
  ids_found <- db_get(con, table_name, "id", data$id, "id")$id
  to_add <- data[!data$id %in% ids_found, ]

  if (nrow(to_add) > 0) {
    DBI::dbWriteTable(con, table_name, to_add, append = TRUE)
  }

  data[data$id %in% ids_found, ]
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
