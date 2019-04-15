#' Compute quantiles of distribution
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
#' @examples
#' d_norm <- as_d(dnorm)
#' probs <- c(0.25, 0.5, 0.75)
#' all.equal(summ_quantile(d_norm, probs), as_q(d_norm)(probs))
#'
#' @export
summ_quantile <- function(f, probs) {
  assert_pdqr_fun(f)
  assert_type(probs, is.numeric)
  if (any((probs < 0) | (probs > 1))) {
    stop_collapse("`probs` should have values inside [0; 1].")
  }

  as_q(f)(probs)
}

#' Extract probability from boolean pdqr-function
#'
#' Here `summ_prob_true()` returns a probability of 1 and `summ_prob_false()` -
#' complementary probability (one minus `summ_prob_true()` output). Both of them
#' check if their input is a **boolean** pdqr-function: type "fin" with `x` in
#' `x_tbl` identical to `c(0, 1)`. If it is not, warning is thrown.
#'
#' @inheritParams summ_mean
#'
#' @return A single numeric value representing corresponding probability.
#'
#' @examples
#' d_unif <- as_d(dunif)
#' d_norm <- as_d(dnorm)
#' summ_prob_true(d_unif > d_norm)
#' summ_prob_false(2*d_norm > d_unif)
#'
#' @name summ-prob
NULL

#' @rdname summ-prob
#' @export
summ_prob_true <- function(f) {
  assert_pdqr_fun(f)
  if (!is_boolean_pdqr_fun(f)) {
    warning_collapse(
      '`f` is not a "boolean" pdqr-function (type "fin" with `x` in "x_tbl" ',
      'identical to c(0, 1)). Proceed with caution.'
    )
  }

  x_tbl <- meta_x_tbl(f)

  x_tbl[["prob"]][x_tbl[["x"]] == 1]
}

#' @rdname summ-prob
#' @export
summ_prob_false <- function(f) {
  1 - summ_prob_true(f)
}
