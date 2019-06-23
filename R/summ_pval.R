#' Summarize distribution with p-value
#'
#' `summ_pval()` computes p-value(s) based on supplied distribution and observed
#' value(s). There are several methods of computing p-values ("both", "right",
#' and "left") as well as several types of multiple comparison adjustments
#' (using on [stats::p.adjust()]).
#'
#' @inheritParams summ_mean
#' @param obs Numeric vector of observed values to be used as threshold for
#'   p-value. Can have multiple values, in which case output will be adjusted
#'   for multiple comparisons with [p.adjust()][stats::p.adjust()].
#' @param method Method representing direction of p-value computation. Should be
#'   one of "both", "right", "left".
#' @param adjust Adjustment method as `method` argument to `p.adjust()`.
#'
#' @details Method "both" for each element in `obs` computes two-sided p-value
#' as `min(1, 2 * min(right_p_val, left_p_val))`, where `right_p_val` and
#' `left_p_val` are right and left one-sided p-values (ones which are computed
#' with "right" and "left" methods) of `obs`'s elements correspondingly.
#'
#' Method "right" for each element `x` of `obs` computes probability of `f >= x`
#' being true (more strictly, of random variable, represented by `f`, being not
#' less than `x`). This corresponds to right one-sided p-value.
#'
#' Method "left" for each element `x` of `obs` computes probability of `f <= x`,
#' which is a left one-sided p-value.
#'
#' **Note** that by default multiple p-values in output are adjusted with
#' `p.adjust(*, method = adjust)`. To not do any adjustment, use `adjust =
#' "none"`.
#'
#' @return A numeric vector with the same length as `obs` representing
#' corresponding p-values after possible adjustment for multiple comparisons.
#'
#' @family summary functions
#'
#' @examples
#' # Type "discrete"
#' d_dis <- new_d(data.frame(x = 1:5, prob = c(1, 2, 3, 2, 1) / 9), "discrete")
#' summ_pval(d_dis, 3, method = "both")
#' summ_pval(d_dis, 3, method = "right")
#' summ_pval(d_dis, 3, method = "left")
#'
#' # Type "continuous"
#' d_norm <- as_d(dnorm)
#' summ_pval(d_norm, 2, method = "both")
#' summ_pval(d_norm, 2, method = "right")
#' summ_pval(d_norm, 2, method = "left")
#'
#' # Adjustment is made for multiple observed values
#' summ_pval(d_norm, seq(0, 2, by = 0.1))
#'   # Use `adjust = "none"` for to not do any adjustment
#' summ_pval(d_norm, seq(0, 2, by = 0.1), adjust = "none")
#'
#' @export
summ_pval <- function(f, obs, method = "both", adjust = "holm") {
  assert_pdqr_fun(f)
  assert_missing(obs, "numeric vector of observation(s)")
  assert_type(obs, is.numeric)

  assert_type(method, is_string)
  assert_in_set(method, c("both", "right", "left"))

  assert_type(adjust, is_string)
  assert_in_set(adjust, stats::p.adjust.methods)

  f <- as_p(f)

  res <- switch(
    method,
    both = both_pval(f, obs),
    left = left_pval(f, obs),
    right = right_pval(f, obs)
  )

  stats::p.adjust(res, method = adjust)
}

both_pval <- function(p_f, obs) {
  res <- 2 * pmin(right_pval(p_f, obs), left_pval(p_f, obs))

  pmin(res, 1)
}

right_pval <- function(p_f, obs) {
  if (meta_type(p_f) == "discrete") {
    # This is needed to compute probability of `x >= obs` and not of `x > obs`
    # (which is returned by `1 - p_f(obs)`).
    # Alternative implementation is to sum probabilities directly from "x_tbl"
    # (for every element in `obs). Although this would be faster on target
    # usecase of small number of observations (`as_d` converting here takes some
    # time), it has slower algorithmic (big-O) speed.
    1 - p_f(obs) + as_d(p_f)(obs)
  } else {
    1 - p_f(obs)
  }
}

left_pval <- function(p_f, obs) {
  p_f(obs)
}
