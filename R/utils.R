# General predicates ------------------------------------------------------
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


# Function and vector manipulations ---------------------------------------
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

impute_inf <- function(x, y, vec_name) {
  is_inf <- is.infinite(y)
  is_fin <- is.finite(y)
  if (!all(is_inf | is_fin)) {
    stop_collapse(
      "All values in ", vec_name, " should be finite or infinite numbers ",
      "(with no `NA`s)."
    )
  }
  if (sum(is_fin) < 3) {
    stop_collapse(vec_name, " should have at least 3 finite values.")
  }
  if (!any(is_inf)) {
    return(y)
  }

  inf_inds <- which(is_inf)
  x_inf <- x[is_inf]

  fin_inds <- which(is_fin)
  x_fin <- x[is_fin]
  y_fin <- y[is_fin]

  l_ind <- findInterval(inf_inds, fin_inds)

  # Extrapolate linearly based on two nearest *from left* points with finite
  # `y`. If there are none, return `-Inf`.
  left_imputation <- impute_linearly(
    ind_1 = l_ind - 1, ind_2 = l_ind,
    x = x_fin, y = y_fin, x_target = x_inf,
    direction = "left"
  )
  # Extrapolate linearly based on two nearest *from right* points with finite
  # `y`. If there are none, return `-Inf`.
  right_imputation <- impute_linearly(
    ind_1 = l_ind + 1, ind_2 = l_ind + 2,
    x = x_fin, y = y_fin, x_target = x_inf,
    direction = "right"
  )

  # Compute maximum from two linear imputations
  y[is_inf] <- pmax(left_imputation, right_imputation)

  y
}

impute_linearly <- function(ind_1, ind_2, x, y, x_target, direction) {
  res <- numeric(length(x_target))
  condition_bad <- switch(
    direction, left = ind_2 < 2, right = ind_1 > length(x) - 1
  )
  res[condition_bad] <- -Inf

  not_bad <- !condition_bad
  res[not_bad] <- extrap_lin(
    x_1 = x[ind_1[not_bad]], x_2 = x[ind_2[not_bad]],
    y_1 = y[ind_1[not_bad]], y_2 = y[ind_2[not_bad]],
    x_target = x_target[not_bad]
  )

  res
}

extrap_lin <- function(x_1, x_2, y_1, y_2, x_target) {
  slope <- (y_2 - y_1) / (x_2 - x_1)
  inter <- y_1 - slope * x_1

  slope * x_target + inter
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

stretch_range <- function(x, ext = 10^(-6)) {
  x + ext * c(-1, 1)
}


# List manipulations ------------------------------------------------------
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


# Class and attributes manipulations --------------------------------------
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
