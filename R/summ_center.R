#' Summarize distribution with center
#'
#' Functions to compute center of distribution. `summ_center()` is a wrapper for
#' respective `summ_*()` functions (from this page) with default arguments.
#'
#' @param f A pdqr-function representing distribution.
#' @param method Method of center computation. For `summ_center()` is one of
#'   "mean", "median", "mode". For `summ_mode()` is one of "global" or "local".
#'
#' @details `summ_mean()` computes distribution's mean.
#'
#' `summ_median()` computes a smallest `x` value for which cumulative
#' probability is not less than 0.5. Essentially, it is a `as_q(f)(0.5)`. This
#' also means that for pdqr-functions with type "discrete" it always returns an
#' entry of "x" column from `f`'s ["x_tbl" metadata][meta_x_tbl()].
#'
#' `summ_mode(*, method = "global")` computes a smallest `x` (which is an entry
#' of "x" column from `f`'s `x_tbl`) with the highest probability/density.
#' `summ_mode(*, method = "local")` computes all `x` values which represent
#' non-strict **local maxima** of probability mass/density function.
#'
#' @return `summ_center()`, `summ_mean()`, `summ_median()` and `summ_mode(*,
#'   method = "global")` always return a single number representing a center of
#'   distribution. `summ_mode(*, method = "local")` can return a numeric vector
#'   with multiple values representing local maxima.
#'
#' @seealso [summ_spread()] for computing distribution's spread, [summ_moment()]
#'   for general moments.
#'
#' @family summary functions
#'
#' @examples
#' # Type "continuous"
#' d_norm <- as_d(dnorm)
#'   # The same as `summ_center(d_norm, method = "mean")`
#' summ_mean(d_norm)
#' summ_median(d_norm)
#' summ_mode(d_norm)
#'
#' # Type "discrete"
#' d_pois <- as_d(dpois, lambda = 10)
#' summ_mean(d_pois)
#' summ_median(d_pois)
#'   # Returns the smallest `x` with highest probability
#' summ_mode(d_pois)
#'   # Returns all values which are non-strict local maxima
#' summ_mode(d_pois, method = "local")
#'
#' # Details of computing local modes
#' my_d <- new_d(data.frame(x = 11:15, y = c(0, 1, 0, 2, 0)/3), "continuous")
#'   # Several values, which are entries of `x_tbl`, are returned as local modes
#' summ_mode(my_d, method = "local")
#'
#' @name summ_center
NULL

#' @rdname summ_center
#' @export
summ_center <- function(f, method = "mean") {
  # `f` is validated inside `summ_*()` calls
  assert_type(method, is_string)
  assert_in_set(method, c("mean", "median", "mode"))

  switch(
    method,
    mean = summ_mean(f),
    median = summ_median(f),
    mode = summ_mode(f, method = "global")
  )
}

#' @rdname summ_center
#' @export
summ_mean <- function(f) {
  assert_pdqr_fun(f)

  # Not using `raw_moment(f, 1)` for speed reasons
  x_tbl <- meta_x_tbl(f)

  switch(
    meta_type(f),
    discrete = dotprod(x_tbl[["x"]], x_tbl[["prob"]]),
    continuous = summ_mean_con(x_tbl)
  )
}

#' @rdname summ_center
#' @export
summ_median <- function(f) {
  assert_pdqr_fun(f)

  as_q(f)(0.5)
}

#' @rdname summ_center
#' @export
summ_mode <- function(f, method = "global") {
  assert_pdqr_fun(f)
  assert_type(method, is_string)
  assert_in_set(method, c("global", "local"))

  f_x_tbl <- meta_x_tbl(f)
  x <- f_x_tbl[["x"]]
  col_name <- switch(meta_type(f), discrete = "prob", continuous = "y")
  col <- f_x_tbl[[col_name]]

  if (method == "global") {
    # Returns the first (smallest) value if there are more than 1
    x[which.max(col)]
  } else {
    col_left <- col[-length(col)]
    col_right <- col[-1]
    col_geq_right <- c(col_left >= col_right, TRUE)
    col_geq_left <- c(TRUE, col_right >= col_left)

    x[col_geq_right & col_geq_left]
  }
}

summ_mean_con <- function(x_tbl) {
  n <- nrow(x_tbl)
  x_lag <- x_tbl[["x"]][-n]
  x_lead <- x_tbl[["x"]][-1]
  y_lag <- x_tbl[["y"]][-n]
  y_lead <- x_tbl[["y"]][-1]
  y_sum <- y_lag + y_lead
  x_mass <- (x_lag * (y_lag + y_sum) + x_lead * (y_lead + y_sum))/(3 * y_sum)

  x_mass_is_good <- is.finite(x_mass)

  prob <- diff(x_tbl[["cumprob"]])

  dotprod(x_mass[x_mass_is_good], prob[x_mass_is_good])
}
