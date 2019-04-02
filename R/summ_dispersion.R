summ_dispersion <- function(f, method = "sd") {
  # `f` is validated inside `summ_*()` calls
  assert_type(method, is_string)
  assert_in_set(method, c("var", "sd", "iqr", "mad"))

  switch(
    method,
    var = summ_var(f),
    sd = summ_sd(f),
    iqr = summ_iqr(f),
    mad = summ_mad(f)
  )
}

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

summ_mad <- function(f) {
  # `f` is validated inside `summ_median(f)`
  med <- summ_median(f)

  summ_median(abs(f - med))
}
