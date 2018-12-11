# General -----------------------------------------------------------------
is_near <- function (x, y, tol = 10^(-8)) {
  abs(x - y) < tol
}

is_string <- function(x) {
  is.character(x) && (length(x) == 1)
}

is_single_number <- function(x) {
  is.numeric(x) && (length(x) == 1)
}

is_truefalse <- function(x) {
  identical(x, TRUE) || identical(x, FALSE)
}

inversing <- function(f, interval, f_type, n_grid = 10001) {
  x_grid <- seq(from = interval[1], to = interval[2], length.out = n_grid)
  y_grid <- f(x_grid)

  # Removing duplicates from end is needed for correct inversion of "raw" ECDF
  # Removing all non-finite elements is needed for correct interpolation
  is_good_y <- !duplicated.default(y_grid, fromLast = f_type == "raw") &
    is.finite(y_grid)
  x_grid <- x_grid[is_good_y]
  y_grid <- y_grid[is_good_y]

  if (length(x_grid) == 1) {
    only_val <- x_grid[1]

    res <- function(v) {rep(only_val, length(v))}
  } else {
    approx_method <- approx_method_from_type(f_type)

    res <- stats::approxfun(
      x = y_grid, y = x_grid, method = approx_method, rule = 2
    )
  }

  # For efficient memory management
  rm(list = c("x_grid", "y_grid", "is_good_y", "f"), envir = environment())

  res
}

approx_method_from_type <- function(chr) {
  switch(
    chr, raw = "constant", smooth = "linear", stop_collapse("Invalid type.")
  )
}

stretch_range <- function(x, ext = 10^(-6)) {
  x + ext * c(-1, 1)
}

add_class <- function(x, cl) {
  x_cl <- class(x)
  n_cl <- length(cl)

  if (x_cl[1] != cl[n_cl]) {
    class(x) <- c(cl, x_cl)
  } else {
    class(x) <- c(cl[-n_cl], x_cl)
  }

  x
}

copy_attrs <- function(to, from) {
  attributes(to) <- attributes(from)

  to
}

dedupl_list <- function(l) {
  l_names <- names(l)

  if (is.null(l_names)) {
    l
  } else {
    l[!duplicated(l_names) | (l_names == "")]
  }
}


# Notifications -----------------------------------------------------------
stop_collapse <- function(...) {
  stop(collapse_nullable(...), call. = FALSE)
}

warning_collapse <- function(...) {
  warning(collapse_nullable(...), call. = FALSE)
}

message_collapse <- function(...) {
  message(collapse_nullable(...))
}

# This allows to print 'NULL' in for code which evaluates in `NULL`
collapse_nullable <- function(...) {
  dots <- lapply(list(...), capture_null)

  dot_length <- vapply(dots, length, numeric(1))
  if (any(dot_length > 1)) {
    stop("All arguments must be length 1.", call. = FALSE)
  }

  do.call(paste0, dots)
}

capture_null <- function(x) {
  if (is.null(x)) {
    "NULL"
  } else {
    x
  }
}
