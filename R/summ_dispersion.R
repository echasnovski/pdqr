summ_var <- function(f) {
  # `f` is validated inside `summ_mean()`
  # `max(*, 0)` is used to take into account numerical representation accuracy
  max(-summ_mean(f)^2 + raw_moment(f, order = 2), 0)
}

summ_sd <- function(f) {
  # `f` is validated inside `summ_var()`
  sqrt(summ_var(f))
}

summ_iqr <- function(f) {
  assert_pdqr_fun(f)

  quarts <- as_q(f)(c(0.25, 0.75))

  quarts[2] - quarts[1]
}
