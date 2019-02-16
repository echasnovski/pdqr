summ_pval <- function(f, obs, direction = "right", adjust = "holm") {
  assert_pdqr_fun(f)
  assert_type(obs, is.numeric)

  assert_type(direction, is_string)
  assert_in_set(direction, c("left", "right", "both"))

  assert_type(adjust, is_string)
  assert_in_set(adjust, stats::p.adjust.methods)

  f <- as_p(f)

  res <- switch(
    direction,
    left = left_pval(f, obs),
    right = right_pval(f, obs),
    both = both_pval(f, obs)
  )

  stats::p.adjust(res, method = adjust)
}

left_pval <- function(p_f, obs) {
  p_f(obs)
}

right_pval <- function(p_f, obs, h = 10^(-6)) {
  # Shifting by small value `h` is needed to compute probability of `x >= obs`
  # and not of `x > obs` (which is returned by `1 - p_v(obs)`)
  1 - p_f(obs - h)
}

both_pval <- function(p_f, obs) {
  res <- 2 * pmin(left_pval(p_f, obs), right_pval(p_f, obs))

  pmin(res, 1)
}
