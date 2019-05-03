summ_pval <- function(f, obs, method = "both", adjust = "holm") {
  assert_pdqr_fun(f)
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
  if (meta_type(p_f) == "fin") {
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
