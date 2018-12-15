distr_from_meta <- function(f, new_f, ...) {
  new_f(
    x = meta(f, "x"),
    type = meta(f, "type"),
    attach_x = TRUE,
    ...
  )
}

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

  distr_fun(
    x = f(n_sample),
    type = meta(f, "type"),
    # As `x` isn't attached to `f`, as it is checked in `as_*()` functions
    attach_x = FALSE,
    ...
  )
}

warn_conversion_from_p_raw <- function(f, warn_precision, fun_name) {
  if (isTRUE(warn_precision) && inherits(f, "p") &&
      (meta(f, "type") == "raw")) {
    warning_collapse(
      'Converting from cumulative distribution function into ',
      fun_name, ' in case `type` = "raw" and no "x" in metadata is not ',
      'precise around actual raw values. Consider attaching `x` to input.'
    )
  }

  TRUE
}

detect_support_raw <- function(f, support, n = 10000) {
  # Detection is done with trying values with step which is multiple of 10^(-5)
  # This should increase probability of finding actual support
  step <- ceiling(10^5 * diff(support) / n) * 10^(-5)
  x_grid <- seq(support[1], support[2], by = step)

  x_grid[!is_near(f(x_grid), 0)]
}
