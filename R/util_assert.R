assert_nonmissing <- function(x, name = deparse(substitute(x))) {
  if (any(is.na(x))) {
    stop(sprintf("'%s' must not be NA", name), call. = FALSE)
  }
}

assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}

assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be a logical", name), call. = FALSE)
  }
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be a character", name), call. = FALSE)
  }
}

assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_character(x, name)
  assert_scalar(x, name)
  assert_nonmissing(x, name)
  if (!nzchar(x)) {
    stop(sprintf("'%s' must be nonempty", name), call. = FALSE)
  }
}

assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_logical(x, name)
  assert_scalar(x, name)
  assert_nonmissing(x, name)
}

assert_connection <- function(x, name = deparse(substitute(x))) {
  if (!inherits(x, "PqConnection")) {
    stop(sprintf("'%s' must be a PqConnection object", name), call. = FALSE)
  }
}
