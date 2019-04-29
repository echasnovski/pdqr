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


# Get information about objects -------------------------------------------
neigh_dist <- function(vec, neigh_type = "min", def_dist = NULL) {
  if (is.unsorted(vec)) {
    vec_ord <- order(vec)
    inv_vec_ord <- order(vec_ord)
  } else {
    vec_ord <- seq_along(vec)
    inv_vec_ord <- vec_ord
  }

  vec_sorted <- vec[vec_ord]

  if (is.null(def_dist)) {
    def_dist <- if (neigh_type == "min") {Inf} else {-Inf}
  }

  diff_vec <- diff(vec_sorted)
  left_dist <- c(def_dist, diff_vec)
  right_dist <- c(diff_vec, def_dist)

  if (neigh_type == "min") {
    sorted_dist <- pmin(left_dist, right_dist)
  } else {
    sorted_dist <- pmax(left_dist, right_dist)
  }

  sorted_dist[inv_vec_ord]
}


# Function and vector manipulations ---------------------------------------
recycle_vec <- function(vec, n) {
  vec_name <- paste0("`", deparse(substitute(vec)), "`")

  if (!(length(vec) %in% c(1, n))) {
    stop_collapse(vec_name, " should have length 1 or ", n, ".")
  }

  rep(vec, length.out = n)
}

inversing <- function(f, interval, ..., n_grid = 10001,
                      approxfun_args = list()) {
  x_grid <- seq_between(interval, length.out = n_grid)
  y_grid <- f(x_grid, ...)

  # Impute infinity `y` values by linear extrapolations
  y_grid <- impute_inf(x_grid, y_grid, "`y` grid during inversing")

  call_args <- c_dedupl(
    approxfun_args,
    list(x = y_grid, y = x_grid, method = "linear", rule = 2)
  )
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

stretch_range <- function(x, ext = 10^(-6)) {
  x + ext * c(-1, 1)
}

seq_between <- function(seq_range, ...) {
  seq(from = seq_range[1], to = seq_range[2], ...)
}

dotprod <- function(x, y) {
  sum(x*y, na.rm = TRUE)
}

coalesce_pair <- function(x, y) {
  res <- x
  x_is_na <- is.na(x)
  res[x_is_na] <- y[x_is_na]

  res
}

integrate_safely <- function(f, lower, upper, n_grid = 10001, ...) {
  tryCatch(
    expr = stats::integrate(f, lower, upper, ...)[["value"]],
    error = function(e) {
      tryCatch(
        expr = {
          x <- seq(lower, upper, length.out = n_grid)
          y <- f(x)
          y <- impute_inf(x, y, "`y`")

          trapez_integral(x, y)
        },
        error = function(e) {
          stop_collapse(
            "Can't compute integral from ", lower, " to ", upper, "."
          )
        }
      )
    }
  )
}

inf_to_na <- function(x) {
  ifelse(is.infinite(x), NA_real_, x)
}

all_same <- function(x, tolerance = 0) {
  isTRUE(all.equal(x, rep(x[1], length(x)), tolerance = tolerance))
}


# List manipulations ------------------------------------------------------
c_dedupl <- function(...) {
  l <- c(...)

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


# Distance functions ------------------------------------------------------
find_nearest_ind <- function(x, set) {
  # Returns `length(x)` **indicies of `set`** which are closest to respective
  # `x` elements.
  if (length(set) == 1) {
    return(rep(1, length(x)))
  }

  if (is.unsorted(set)) {
    set_ord <- order(set)
  } else {
    set_ord <- seq_along(set)
  }
  set_sorted <- set[set_ord]

  # Find index of the biggest set point to the left
  x_ind <- findInterval(x, set_sorted, all.inside = TRUE)
  # Possibly correct found index to represent the closest point
  x_ind <- x_ind + (set_sorted[x_ind + 1] - x < x - set_sorted[x_ind])

  set_ord[x_ind]
}

find_nearest_match <- function(x, set) {
  # Returns `length(x)` **unique indicies of `set`** which are closest to
  # respective `x` elements. Uses naive iterative greedy search. For this to
  # work properly, `length(x)` should be not greater than `length(set)`.
  if (length(set) < length(x)) {
    stop_collapse(
      "Can't find unique matching because `set` has fewer elements than `x`."
    )
  }

  res <- integer(length(x))

  x_to_match <- seq_along(x)
  set_to_match <- seq_along(set)

  # Use greedy search of "matching nearest" values. Note that this might result
  # in very long distances between actual matches. That is why output should be
  # viewed as an unordered set of unique indicies of `set` that are "closest" to
  # `x`.
  while(length(x_to_match) > 0) {
    # Match "current" `x` and `set`
    x_matches <- find_nearest_ind(x[x_to_match], set[set_to_match])

    # Use only not duplicated matches, because output should be unique in terms
    # of `set` indicies
    good_match <- !duplicated(x_matches)
    set_matched <- set_to_match[x_matches[good_match]]

    # Update result (indicies of the nearest match) within "good matches"
    res[x_to_match[good_match]] <- set_matched

    # Update "current" `x` and `set` by removing already "used" elements
    x_to_match <- x_to_match[!good_match]
    set_to_match <- setdiff(set_to_match, set_matched)
  }

  res
}

find_neigh_subset <- function(x, set, n_subset, type = "min") {
  # Returns `n_subset` **indicies of `x`** which are closest to the `set` (as a
  # set).
  if (n_subset > length(x)) {
    stop_collapse(
      "Can't find neighborhood subset because `n_subset` is bigger than ",
      "length of `x`."
    )
  }

  dist_x_set <- abs(x - set[find_nearest_ind(x, set)])

  order(dist_x_set, decreasing = type == "max")[seq_len(n_subset)]
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


# Pdqr approximation error ------------------------------------------------
#' Diagnose pdqr approximation
#'
#' `pdqr_approx_error()` computes errors that are results of 'pdqr'
#' approximation, which occurs because of possible tail trimming and assuming
#' piecewise linearity of density function in case of "infin" type. For an easy
#' view summary, use [summary()][base::summary()].
#'
#' @param f A p-, d-, or q-function to diagnose. Usually the output of one of
#'   [as_p()], [as_d()], or [as_q()] default methods.
#' @param ref_f A "true" distribution function of the same class as `f`. Usually
#'   the input to the aforementioned `as_*()` function.
#' @param ... Other arguments to `ref_f`. If they were supplied to `as_*()`
#'   function, then the exact same values must be supplied here.
#' @param gran Degree of grid "granularity" in case of "infin" type: number of
#'   subintervals to be produced inside every interval of density linearity.
#'   Should be not less than 1 (indicator that original column from
#'   ["x_tbl"][meta_x_tbl()] will be used, see details).
#' @param remove_infinity Whether to remove rows corresponding to infinite
#'   error.
#'
#' @details Errors are computed as difference between "true" value (output of
#' `ref_f`) and output of pdqr-function `f`. They are computed at "granulated"
#' `gran` times grid (which is an "x" column of "x_tbl" in case `f` is p- or
#' d-function and "cumprob" column if q-function). They are usually negative
#' because of possible tail trimming of reference distribution.
#'
#' **Notes**:
#' - `gran` argument for "fin" type is always 1.
#' - Quantile pdqr approximation of "fin" distribution with infinite tale(s) can
#' result into "all one" summary of error. This is expected output and is
#' because test grid is chosen to be quantiles of pdqr-distribution which due to
#' renormalization can differ by one from reference ones. For example:
#' `summary(pdqr_approx_error(as_p(ppois, lambda = 10), ppois, lambda = 10))`.
#'
#' @return A data frame with the following columns:
#' - **grid** <dbl> : A grid at which errors are computed.
#' - **error** <dbl> : Errors which are computed as `ref_f(grid, ...) -
#' f(grid)`.
#' - **abserror** <dbl> : Absolute value of "error" column.
#'
#' @examples
#' d_norm <- as_d(dnorm)
#' error_norm <- pdqr_approx_error(d_norm, dnorm)
#' summary(error_norm)
#'
#' # Setting `gran` results into different number of rows in output
#' error_norm_2 <- pdqr_approx_error(d_norm, dnorm, gran = 1)
#' nrow(meta_x_tbl(d_norm)) == nrow(error_norm_2)
#'
#' # By default infinity errors are removed
#' d_beta <- as_d(dbeta, shape1 = 0.3, shape2 = 0.7)
#' error_beta_1 <- pdqr_approx_error(d_beta, dbeta, shape1 = 0.3, shape2 = 0.7)
#' summary(error_beta_1)
#'
#' # To not remove them, set `remove_infinity` to `FALSE`
#' error_beta_2 <- pdqr_approx_error(
#'   d_beta, dbeta, shape1 = 0.3, shape2 = 0.7, remove_infinity = FALSE
#' )
#' summary(error_beta_2)
#'
#' @export
pdqr_approx_error <- function(f, ref_f, ..., gran = 10,
                              remove_infinity = TRUE) {
  assert_pdqr_fun(f)
  if (!(get_pdqr_class(f) %in% c("p", "d", "q"))) {
    stop_collapse("`f` should be p-, d-, or q-function.")
  }
  assert_type(ref_f, is.function)
  assert_type(
    gran, is_single_number,
    type_name = "single number more than 1", min_val = 1
  )
  assert_type(
    remove_infinity, is_truefalse, type_name = "`TRUE` or `FALSE`"
  )

  gran <- ceiling(gran)
  grid <- switch(
    meta_type(f),
    fin = granulate_grid(f, gran = 1),
    infin = granulate_grid(f, gran = gran)
  )

  error <- ref_f(grid, ...) - f(grid)
  if (remove_infinity) {
    error_isnt_infinite <- !is.infinite(error)
    grid <- grid[error_isnt_infinite]
    error <- error[error_isnt_infinite]
  }

  data.frame(grid = grid, error = error, abserror = abs(error))
}

granulate_grid <- function(f, gran) {
  x_tbl <- meta_x_tbl(f)
  vec <- switch(
    get_pdqr_class(f),
    d = x_tbl[["x"]],
    p = x_tbl[["x"]],
    q = x_tbl[["cumprob"]]
  )

  if (gran == 1) {
    return(vec)
  }

  n <- length(vec)
  step <- (vec[-1] - vec[-n]) / gran

  res <- rep(vec[-n], each = gran) +
    rep(step, each = gran) * rep(0:(gran-1), times = n-1)

  # Add missing last element
  c(res, vec[n])
}
