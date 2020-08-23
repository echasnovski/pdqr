#' Summarize distribution with spread
#'
#' Functions to compute spread (variability, dispersion) of distribution (i.e.
#' "how wide it is spread"). `summ_spread()` is a wrapper for respective
#' `summ_*()` functions (from this page) with default arguments.
#'
#' @inheritParams summ_center
#' @param method Method of spread computation. Should be one of "sd", "var",
#'   "iqr", "mad", "range".
#'
#' @details `summ_sd()` computes distribution's standard deviation.
#'
#' `summ_var()` computes distribution's variance.
#'
#' `summ_iqr()` computes distribution's interquartile range. Essentially, it is
#' a `as_q(f)(0.75) - as_q(f)(0.25)`.
#'
#' `summ_mad()` computes distribution's *median* absolute deviation around the
#' distribution's *median*.
#'
#' `summ_range()` computes range length (difference between maximum and minimum)
#' of "x" values within region of positive probability. **Note** that this might
#' differ from length of [support][meta_support()] because the latter might be
#' affected by tails with zero probability (see Examples).
#'
#' @return All functions return a single number representing a spread of
#'   distribution.
#'
#' @seealso [summ_center()] for computing distribution's center, [summ_moment()]
#'   for general moments.
#'
#' @family summary functions
#'
#' @examples
#' # Type "continuous"
#' d_norm <- as_d(dnorm)
#' ## The same as `summ_spread(d_norm, method = "sd")`
#' summ_sd(d_norm)
#' summ_var(d_norm)
#' summ_iqr(d_norm)
#' summ_mad(d_norm)
#' summ_range(d_norm)
#'
#' # Type "discrete"
#' d_pois <- as_d(dpois, lambda = 10)
#' summ_sd(d_pois)
#' summ_var(d_pois)
#' summ_iqr(d_pois)
#' summ_mad(d_pois)
#' summ_range(d_pois)
#'
#' # Difference of `summ_range(f)` and `diff(meta_support(f))`
#' zero_tails <- new_d(data.frame(x = 1:5, y = c(0, 0, 1, 0, 0)), "continuous")
#' ## This returns difference between 5 and 1
#' diff(meta_support(zero_tails))
#' ## This returns difference between 2 and 4 as there are zero-probability
#' ## tails
#' summ_range(zero_tails)
#' @name summ_spread
NULL

#' @rdname summ_spread
#' @export
summ_spread <- function(f, method = "sd") {
  assert_pdqr_fun(f)
  assert_method(method, methods_spread)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  switch(
    method,
    var = summ_var(f),
    sd = summ_sd(f),
    iqr = summ_iqr(f),
    mad = summ_mad(f),
    range = summ_range(f)
  )
}

methods_spread <- c("var", "sd", "iqr", "mad", "range")

#' @rdname summ_spread
#' @export
summ_sd <- function(f) {
  # `f` is validated inside `summ_var()`
  sqrt(summ_var(f))
}

#' @rdname summ_spread
#' @export
summ_var <- function(f) {
  # `f` is validated inside `summ_mean()`
  # `max(*, 0)` is used to take into account numerical representation accuracy
  max(-summ_mean(f)^2 + raw_moment(f, order = 2), 0)
}

#' @rdname summ_spread
#' @export
summ_iqr <- function(f) {
  assert_pdqr_fun(f)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  quarts <- as_q(f)(c(0.25, 0.75))

  quarts[2] - quarts[1]
}

#' @rdname summ_spread
#' @export
summ_mad <- function(f) {
  # `f` is validated inside `summ_median(f)`
  med <- summ_median(f)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  summ_median(abs(f - med))
}

#' @rdname summ_spread
#' @export
summ_range <- function(f) {
  assert_pdqr_fun(f)

  x_tbl <- meta_x_tbl(f)
  x <- x_tbl[["x"]]
  d_vals <- x_tbl[[get_x_tbl_sec_col(x_tbl)]]

  # Note that this code assumes that "x_tbl" metadata is arranged in ascending
  # order of "x" column
  within_pos_prob <- !is_zero_tail(d_vals, type = meta_type(f))
  x_range <- range(x[within_pos_prob])

  x_range[2] - x_range[1]
}
