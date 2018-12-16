as_distr_impl_def <- function(fun_class, f, type, support, adjust_to_support) {
  assert_type(f, is.function)
  assert_distr_type(type)
  assert_support(support)

  f_adj <- adjust_to_support(f, type, support)

  res <- add_meta(remove_meta(f_adj), type = type, support = support)

  add_pdqr_class(res, fun_class)
}

as_distr_impl_r <- function(distr_fun, f, n_sample, ...) {
  assert_type(n_sample, is_single_number, type = "single number")

  distr_fun(x = f(n_sample), type = meta(f, "type"), ...)
}

detect_support_raw <- function(f, support, n = 10000) {
  # Detection is done with trying values with step which is multiple of 10^(-5)
  # This should increase probability of finding actual support
  step <- ceiling(10^5 * diff(support) / n) * 10^(-5)
  x_grid <- seq(support[1], support[2], by = step)

  x_grid[!is_near(f(x_grid), 0)]
}
