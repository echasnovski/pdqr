# General predicates ------------------------------------------------------
is_near <- function (x, y, tol = 10^(-8)) {
  abs(x - y) < tol
}

is_zero <- function(x) {
  is_near(x, 0, tol = 1e-12)
}

is_string <- function(x) {
  is.character(x) && (length(x) == 1)
}

is_single_color <- function(x) {
    tryCatch(
      {
        col <- grDevices::col2rgb(x)

        is.matrix(col) && (ncol(col) == 1)
      },
      error = function(e) {FALSE}
    )
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

# This is an euristic for a computation of "effective" height of density.
# Currently this is used for a nice plotting in presence of dirac-like entries
# in "x_tbl"
compute_d_con_ylim <- function(f) {
  f_x_tbl <- meta_x_tbl(f)
  x <- f_x_tbl[["x"]]
  y <- f_x_tbl[["y"]]

  x_neigh_dist <- neigh_dist(x, neigh_type = "max")

  # Here `2e-8` is used instead of default `1e-8` to account for possible
  # numerical representation inaccuracies.
  y_non_dirac <- y[x_neigh_dist >= 2e-8]
  if ((length(y_non_dirac) == 0) || (max(y_non_dirac) == 0)) {
    # The case when all entries seems to be dirac-like. So all points should be
    # used in computing `ylim`.
    range(y)
  } else {
    # The case when only some entries are dirac-like and the respective "y"s
    # should be removed.
    range(y_non_dirac)
  }
}


# Function and vector manipulations ---------------------------------------
recycle_vec <- function(vec, n) {
  vec_name <- enbacktick(deparse(substitute(vec)))

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

enbacktick <- function(x) {
  paste0("`", x, "`")
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

alternate <- function(x, y) {
  # It is assumed that `x` and `y` have the same lengths
  n <- length(x)
  comb <- c(x, y)
  inds <- rep(seq_len(n), each = 2) + rep(c(0, n), times = n)

  comb[inds]
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
#' piecewise linearity of density function in case of "continuous" type. For an
#' easy view summary, use [summary()][base::summary()].
#'
#' @param f A p-, d-, or q-function to diagnose. Usually the output of one of
#'   [as_p()], [as_d()], or [as_q()] default methods.
#' @param ref_f A "true" distribution function of the same [class][meta_class()]
#'   as `f`. Usually the input to the aforementioned `as_*()` function.
#' @param ... Other arguments to `ref_f`. If they were supplied to `as_*()`
#'   function, then the exact same values must be supplied here.
#' @param gran Degree of grid "granularity" in case of "continuous" type: number
#'   of subintervals to be produced inside every interval of density linearity.
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
#' - `gran` argument for "discrete" type is always 1.
#' - Quantile pdqr approximation of "discrete" distribution with infinite
#' tale(s) can result into "all one" summary of error. This is expected output
#' and is because test grid is chosen to be quantiles of pdqr-distribution which
#' due to renormalization can differ by one from reference ones. For example:
#' `summary(pdqr_approx_error(as_p(ppois, lambda = 10), ppois, lambda = 10))`.
#'
#' @return A data frame with the following columns:
#' - **grid** <dbl> : A grid at which errors are computed.
#' - **error** <dbl> : Errors which are computed as `ref_f(grid, ...) -
#' f(grid)`.
#' - **abserror** <dbl> : Absolute value of "error" column.
#'
#' @seealso [enpoint()] for representing pdqr-function as a set of points with
#'   desirable number of rows.
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
  if (!(meta_class(f) %in% c("p", "d", "q"))) {
    stop_collapse("`f` should be p-, d-, or q-function.")
  }
  assert_missing(ref_f, "reference distribution function")
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
    discrete = granulate_grid(f, gran = 1),
    continuous = granulate_grid(f, gran = gran)
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
    meta_class(f),
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


# Represent pdqr-function as a set of points ------------------------------
#' Represent pdqr-function as a set of points
#'
#' Function `enpoint()` suggests a reasonable default ways of converting
#' pdqr-function into a data frame of numerical values (points) with desirable
#' number of rows. Representation of pdqr-function as a set of numbers helps to
#' conduct analysis using approaches outside of 'pdqr' package. For example, one
#' can visually display pdqr-function with some other plotting functionality.
#'
#' @param f A pdqr-function.
#' @param n_points Desired number of points in the output. Not used in case of
#'   "discrete" type p-, d-, and q-function `f`.
#'
#' @details Structure of output depends on [class][meta_class()] and
#' [type][meta_type()] of input pdqr-function `f`:
#' - **P-functions** are represented with "x" (for "x" values) and "p" (for
#' cumulative probability at "x" points) columns:
#'     - For "continuous" type, "x" is taken as an equidistant grid (with
#'     `n_points` elements) on input's [support][meta_support()].
#'     - For "discrete" type, "x" is taken directly from ["x_tbl"
#'     metadata][meta_x_tbl()] without using `n_points` argument.
#' - **D-functions** are represented with "x" column and one more (for values of
#' d-function at "x" points):
#'     - For "continuous" type, second column is named "y" and is computed as
#'     values of `f` at elements of "x" column (which is the same grid as in
#'     p-function case).
#'     - For "discrete" it is named "prob". Both "x" and "prob" columns are
#'     taken from "x_tbl" metadata.
#' - **Q-functions** are represented almost as p-functions but in inverse
#' fashion. Output data frame has "p" (probabilities) and "x" (values of
#' q-function `f` at "p" elements) columns.
#'     - For "continuous" type, "p" is computed as equidistant grid (with
#'     `n_points` elements) between 0 and 1.
#'     - For "discrete" type, "p" is taken from "cumprob" column of "x_tbl"
#'     metadata.
#' - **R-functions** are represented by generating `n_points` elements from
#' distribution. Output data frame has columns "n" (consecutive point number,
#' basically a row number) and "x" (generated elements).
#'
#' **Note** that the other way to produce points for p-, d-, and q-functions is
#' to manually construct them with [form_regrid()] and [meta_x_tbl()]. However,
#' this method may slightly change function values due to possible
#' renormalization inside `form_regrid()`.
#'
#' @return A data frame with `n_points` (or less, for "discrete" type p-, d-, or
#'   q-function `f`) rows and two columns with names depending on `f`'s class
#'   and type.
#'
#' @seealso [pdqr_approx_error()] for diagnostics of pdqr-function approximation
#' accuracy.
#'
#' [Pdqr methods for `plot()`][methods-plot] for a direct plotting of
#' pdqr-functions.
#'
#' [form_regrid()] to change underlying grid of pdqr-function.
#'
#' @examples
#' d_norm <- as_d(dnorm)
#' head(enpoint(d_norm))
#'
#' # Control number of points with `n_points` argument
#' enpoint(d_norm, n_points = 5)
#'
#' # Different pdqr classes and types produce different column names in output
#' colnames(enpoint(new_p(1:2, "discrete")))
#' colnames(enpoint(new_d(1:2, "discrete")))
#' colnames(enpoint(new_d(1:2, "continuous")))
#' colnames(enpoint(new_q(1:2, "continuous")))
#' colnames(enpoint(new_r(1:2, "continuous")))
#'
#' # Manual way with different output structure
#' df <- meta_x_tbl(form_regrid(d_norm, 5))
#'   # Difference in values due to `form_regrid()` renormalization
#' plot(enpoint(d_norm, 5), type = "l")
#' lines(df[["x"]], df[["y"]], col = "blue")
#'
#' @export
enpoint <- function(f, n_points = 1001) {
  assert_pdqr_fun(f)
  assert_type(
    n_points, is_single_number,
    type_name = "single number more than 1",
    min_val = 1
  )

  switch(
    meta_class(f),
    p = enpoint_p(f, n_points),
    d = enpoint_d(f, n_points),
    q = enpoint_q(f, n_points),
    r = enpoint_r(f, n_points)
  )
}

enpoint_p <- function(f, n_points) {
  if (meta_type(f) == "discrete") {
    x_tbl <- meta_x_tbl(f)

    data.frame(x = x_tbl[["x"]], p = x_tbl[["cumprob"]])
  } else {
    q <- seq_between(meta_support(f), length.out = n_points)

    data.frame(x = q, p = f(q))
  }
}

enpoint_d <- function(f, n_points) {
  if (meta_type(f) == "discrete") {
    x_tbl <- meta_x_tbl(f)

    data.frame(x = x_tbl[["x"]], prob = x_tbl[["prob"]])
  } else {
    x <- seq_between(meta_support(f), length.out = n_points)

    data.frame(x = x, y = f(x))
  }
}

enpoint_q <- function(f, n_points) {
  if (meta_type(f) == "discrete") {
    x_tbl <- meta_x_tbl(f)

    data.frame(p = x_tbl[["cumprob"]], x = x_tbl[["x"]])
  } else {
    p <- seq_between(c(0, 1), length.out = n_points)

    data.frame(p = p, x = f(p))
  }
}

enpoint_r <- function(f, n_points) {
  data.frame(n = seq_len(n_points), x = f(n_points))
}
