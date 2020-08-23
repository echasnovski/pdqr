#' Summarize distribution with quantiles
#'
#' Essentially, this is a more strict wrapper of `as_q(f)(probs)`. If any value
#' in `probs` is outside of segment \\[0; 1\\], an error is thrown.
#'
#' @inheritParams summ_mean
#' @param probs Vector of probabilities for which quantiles should be returned.
#'
#' @return A numeric vector of the same length as `probs` representing
#'   corresponding quantiles.
#'
#' @family summary functions
#'
#' @examples
#' d_norm <- as_d(dnorm)
#' probs <- c(0.25, 0.5, 0.75)
#' all.equal(summ_quantile(d_norm, probs), as_q(d_norm)(probs))
#' @export
summ_quantile <- function(f, probs) {
  assert_pdqr_fun(f)
  assert_missing(probs, "quantile probabilities")
  assert_type(probs, is.numeric)
  if (any((probs < 0) | (probs > 1))) {
    stop_collapse("`probs` should have values inside [0; 1].")
  }

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  as_q(f)(probs)
}

#' Summarize boolean distribution with probability
#'
#' Here `summ_prob_false()` returns a probability of 0 and `summ_prob_true()` -
#' complementary probability (one minus `summ_prob_false()` output). Both of
#' them check if their input is a **boolean** pdqr-function: type "discrete"
#' with `x` in `x_tbl` identical to `c(0, 1)`. If it is not, warning is thrown.
#'
#' @inheritParams summ_mean
#'
#' @return A single numeric value representing corresponding probability.
#'
#' @family summary functions
#'
#' @examples
#' d_unif <- as_d(dunif)
#' d_norm <- as_d(dnorm)
#' summ_prob_true(d_unif > d_norm)
#' summ_prob_false(2 * d_norm > d_unif)
#'
#' # When input is "continuous" function or doesn't have 0 as distribution
#' # element, probability of being false is returned as 0.
#' summ_prob_false(d_unif)
#' summ_prob_true(new_d(2, "discrete"))
#' @name summ_prob_true
NULL

#' @rdname summ_prob_true
#' @export
summ_prob_false <- function(f) {
  assert_pdqr_fun(f)
  if (!is_boolean_pdqr_fun(f)) {
    warning_boolean_pdqr_fun(f)
  }

  x_tbl <- meta_x_tbl(f)

  res <- x_tbl[["prob"]][x_tbl[["x"]] == 0]
  if (length(res) == 0) {
    res <- 0
  }

  res
}

#' @rdname summ_prob_true
#' @export
summ_prob_true <- function(f) {
  1 - summ_prob_false(f)
}
