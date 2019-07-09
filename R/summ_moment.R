#' Summarize distribution with moment
#'
#' `summ_moment()` computes a moment of distribution. It can be one of eight
#' kinds determined by the combination of `central`, `standard`, and `absolute`
#' boolean features. `summ_skewness()` and `summ_kurtosis()` are wrappers for
#' commonly used kinds of moments: third and forth order central standard ones.
#' **Note** that `summ_kurtosis()` by default computes excess kurtosis, i.e.
#' subtracts 3 from computed forth order moment.
#'
#' @inheritParams summ_mean
#' @param order A single number representing order of a moment. Should be
#'   non-negative number (even fractional).
#' @param central Whether to compute central moment (subtract
#'   [mean][summ_mean()] of distribution).
#' @param standard Whether to compute standard moment (divide by [standar
#'   deviation][summ_sd()] of distribution).
#' @param absolute Whether to compute absolute moment (take absolute value of
#'   random variable created after possible effect of `central` and `standard`).
#' @param excess Whether to compute excess kurtosis (subtract 3 from third order
#'   central standard moment). Default is `TRUE`.
#'
#' @return A single number representing moment. If `summ_sd(f)` is zero and
#'   `standard` is `TRUE`, then it is `Inf`; otherwise - finite number.
#'
#' @seealso [summ_center()] for computing distribution's center, [summ_spread()]
#'   for spread.
#'
#' @family summary functions
#'
#' @examples
#' d_beta <- as_d(dbeta, shape1 = 2, shape2 = 1)
#'
#' # The same as `summ_mean(d_beta)`
#' summ_moment(d_beta, order = 1)
#'
#' # The same as `summ_var(d_beta)`
#' summ_moment(d_beta, order = 2, central = TRUE)
#'
#' # Return the same number
#' summ_moment(d_beta, order = 3, central = TRUE, standard = TRUE)
#' summ_skewness(d_beta)
#'
#' # Return the same number representing non-excess kurtosis
#' summ_moment(d_beta, order = 4, central = TRUE, standard = TRUE)
#' summ_kurtosis(d_beta, excess = FALSE)
#'
#' @name summ_moment
NULL

#' @rdname summ_moment
#' @export
summ_moment <- function(f, order, central = FALSE, standard = FALSE,
                        absolute = FALSE) {
  assert_pdqr_fun(f)
  assert_missing(order, "order of moment")
  assert_type(
    order, is_single_number,
    type_name = "single non-negative number", min_val = 0
  )
  assert_type(central, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(absolute, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(standard, is_truefalse, "`TRUE` or `FALSE`")

  if (central) {
    f <- f - summ_mean(f)
  }

  if (standard) {
    f_sd <- summ_sd(f)
    if (is_zero(f_sd)) {
      return(Inf)
    }

    # This leverages the fact that `sd(f) = sd(f - m)` for any `m`.
    f <- f / f_sd
  }

  if (absolute) {
    f <- abs(f)
  }

  raw_moment(f, order)
}

#' @rdname summ_moment
#' @export
summ_skewness <- function(f) {
  summ_moment(f, order = 3, central = TRUE, standard = TRUE)
}

#' @rdname summ_moment
#' @export
summ_kurtosis <- function(f, excess = TRUE) {
  assert_type(excess, is_truefalse, "`TRUE` or `FALSE`")

  res <- summ_moment(f, order = 4, central = TRUE, standard = TRUE)

  if (excess) {
    res <- res - 3
  }

  res
}
