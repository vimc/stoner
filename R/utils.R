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
  sprintf("('%s')", paste(strings, collapse = "','"))
}

sql_in_numeric <- function(numerics) {
  sprintf("(%s)", paste(numerics, collapse = ","))
}

sql_in <- function(things) {
  if (is.character(things)) {
    sql_in_char(things)
  } else if (is.numeric(things)) {
    sql_in_numeric(things)
  } else {
    stop("Can't convert things with sql_in")
  }
}

db_get <- function(con, table, id_field = NULL, id_values = NULL, select = "*") {
  sql <- sprintf("SELECT %s FROM %s", select, table)
  if (!is.null(id_field)) {
    sql <- sprintf("%s WHERE %s IN %s", sql, id_field, sql_in(id_values))
  }
  DBI::dbGetQuery(con, sql)
}

# Return a vector of characters for a table, each entry being all the fields
# of that table mashed together, separated by '\r'

mash <- function(tab, fields = NULL) {
  if (!is.null(fields)) {
    tab <- tab[, fields]
  }
  df_args <- c(tab, sep = "\r")
  do.call(paste, df_args)
}

# Return a vector of logicals, of whether each row in table1
# occurs somewhere in table 2.

line_occurs_in <- function(table1, table2) {
  if (nrow(table2) == 0) {
    rep(FALSE, nrow(table1))
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

# For rows that contain a serial "id" column. Add all the rows with a negative id,
# letting the db choose the idea. Move the given ids to "fake_ids", and record the
# ids the database chose as 'id'. Return a list of the added rows (with the
# id fields), and a list of the rows that are edits.

add_serial_rows <- function(table_name, transformed_data, con, id_field = "id",
                            fake_id_field = "fake_ids") {

  if (!table_name %in% names(transformed_data)) {
    return(list(adds = data_frame(), edits = data_frame()))
  }

  data <- transformed_data[[table_name]]
  to_add <- data[data[[id_field]] < 0, ]
  to_edit <- data[data[[id_field]] >= 0, ]

  if (nrow(to_add) == 0) {
    return(list(adds = to_add, edits = to_edit))
  }

  fake_ids <- to_add[[id_field]]
  to_add[[id_field]] <- NULL
  DBI::dbWriteTable(con, table_name, to_add, append = TRUE)
  to_add[[fake_id_field]] <- fake_ids
  to_add[[id_field]] <- rev(DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM %s ORDER BY %s DESC LIMIT %s",
    id_field, table_name, id_field, nrow(to_add)))$id)

  list(adds = to_add, edits = to_edit)
}

add_non_serial_rows <- function(table_name, transformed_data, con,
                                id_field = "id") {
  if (!table_name %in% names(transformed_data)) {
    return(data_frame())
  }

  data <- transformed_data[[table_name]]
  ids_found <- db_get(con, table_name, id_field, data[[id_field]], id_field)$id
  to_add <- data[!data[[id_field]] %in% ids_found, ]

  if (nrow(to_add) > 0) {
    DBI::dbWriteTable(con, table_name, to_add, append = TRUE)
  }

  data[data[[id_field]] %in% ids_found, ]
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}

check_faulty_serials <- function(con) {
  df <- data_frame(sequence =
                     DBI::dbGetQuery(con, "
                                     SELECT *
                                     FROM information_schema.sequences")$sequence_name)
  df$table <- gsub("_id_seq", "", df$sequence)
  df <- df[df$table %in% DBI::dbListTables(con), ]

  df$maxes <- unlist(lapply(df$table, function(x)
    as.numeric(DBI::dbGetQuery(con, sprintf("SELECT max(id) FROM %s", x)))))

  df <- df[!is.na(df$maxes), ]

  for (r in seq_len(nrow(df))) {
    df$last_value[r] <- tryCatch({
      x <- df$sequence[r]
      as.numeric(DBI::dbGetQuery(con,
                                 sprintf("SELECT last_value FROM %s", x))$last_value)

    })
  }

  df <- df[df$maxes>df$last_value, ]
  if (nrow(df) > 0) {
    x <- print(df)
    stop("Error - db serial numbers were corrupted")
  } else {
    message("Tested faulty serials - OK!")
  }
}
