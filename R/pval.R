pdqr_pval <- function(f, obs, direction = "right", adjust = "holm") {
  assert_pdqr_fun(f)
  assert_type(obs, is.numeric)
  assert_type(direction, is_string)
  assert_adjust(adjust)

  f <- as_p(f)

  res <- switch(
    direction,
    left = left_pval(f, obs),
    right = right_pval(f, obs),
    both = both_pval(f, obs),
    stop_collapse(
      '`direction` should be one of "left", "right", or "both", not ',
      '"', direction, '".'
    )
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

assert_adjust <- function(adjust) {
  assert_type(adjust, is_string)

  if (!(adjust %in% stats::p.adjust.methods)) {
    adjust_values <- paste0(
      '"holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", or "none"'
    )
    stop_collapse(
      "`adjust` should be one of ", adjust_values, ', not "', adjust, '".'
    )
  }

  adjust
}
