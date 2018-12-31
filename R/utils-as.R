as_distr_impl_def <- function(fun_class, f, support, adjust_to_support, ...) {
  assert_type(f, is.function)
  assert_support(support)

  f_partial <- function(t) {
    f(t, ...)
  }
  attributes(f_partial) <- attributes(f)

  f_adj <- adjust_to_support(f_partial, support)

  res <- add_meta(
    remove_meta(f_adj),
    support = support, type = "smooth", x_tbl = NULL
  )

  add_pdqr_class(res, fun_class)
}

as_distr_impl_r <- function(distr_fun, f, n_sample, ...) {
  assert_type(n_sample, is_single_number, type_name = "single number")

  distr_fun(x = f(n_sample), type = meta(f, "type"), ...)
}
