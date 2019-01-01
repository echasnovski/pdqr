# General -----------------------------------------------------------------
is_near <- function (x, y, tol = 10^(-8)) {
  abs(x - y) < tol
}

is_string <- function(x) {
  is.character(x) && (length(x) == 1)
}

is_single_number <- function(x, min_val = NULL, max_val = NULL) {
  res <- is.numeric(x) && (length(x) == 1) && is.finite(x)
  is_geq <- if (is.null(min_val)) {TRUE} else {x >= min_val}
  is_leq <- if (is.null(max_val)) {TRUE} else {x <= max_val}

  res && is_geq && is_leq
}

is_truefalse <- function(x) {
  identical(x, TRUE) || identical(x, FALSE)
}

inversing <- function(f, interval, n_grid = 10001, ...,
                      .approxfun_args = list()) {
  x_grid <- seq(from = interval[1], to = interval[2], length.out = n_grid)
  y_grid <- f(x_grid, ...)

  # Removing all non-finite elements is needed for correct interpolation
  is_fin_y <- is.finite(y_grid)
  x_grid <- x_grid[is_fin_y]
  y_grid <- y_grid[is_fin_y]

  call_args <- dedupl_list(c(
    .approxfun_args,
    list(x = y_grid, y = x_grid, method = "linear", rule = 2)
  ))
  do.call(stats::approxfun, call_args)
}

approx_lin <- function(x, y) {
  # It is assumed that all `x` are unique
  if (is.unsorted(x)) {
    x_ord <- order(x)
    x <- x[x_ord]
    y <- y[x_ord]
    # For efficient memory management
    rm(x_ord)
  }

  function(v) {
    res <- numeric(length(v))

    bounds <- range(x)
    is_inside <- (v >= bounds[1]) & (v <= bounds[2])

    # `all.inside = TRUE` is needed to account for case `v` equals `bounds[2]`
    x_ind <- findInterval(v[is_inside], x, all.inside = TRUE)
    slopes <- (y[x_ind+1] - y[x_ind]) / (x[x_ind+1] - x[x_ind])
    res[is_inside] <- slopes * (v[is_inside] - x[x_ind]) + y[x_ind]

    res
  }
}

extrap_lin <- function(x_1, x_2, y_1, y_2, x_target) {
  slope <- (y_2 - y_1) / (x_2 - x_1)
  inter <- y_1 - slope * x_1

  slope * x_target + inter
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

swap <- function(l, name1, name2) {
  res <- l
  res[[name1]] <- l[[name2]]
  res[[name2]] <- l[[name1]]

  res
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
