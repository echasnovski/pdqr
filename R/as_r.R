as_r <- function(f, ...) {
  if (inherits(f, "r_fun")) {
    return(f)
  } else if (has_meta_x(f) && has_meta_type(f)) {
    return(distr_from_meta(f, r_fun, ...))
  }

  UseMethod("as_r")
}

as_r.default <- function(f, type, support, extra = NULL, ...) {
  assert_missing_args(
    "r_fun", type = missing(type), support = missing(support)
  )
  assert_support(support)

  as_distr_impl_def("r_fun", f, type, support, extra)
}

as_r.p_fun <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  warn_conversion_from_p_raw(f, warn_precision, "random generation function")
  q_f <- as_q(f, n_grid = n_grid, warn_precision = FALSE, ...)

  as_r_impl(q_f)
}

as_r.d_fun <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  q_f <- as_q(f, n_grid = n_grid, warn_precision = warn_precision, ...)

  as_r_impl(q_f)
}

as_r.q_fun <- function(f, ...) {
  as_r_impl(f)
}

as_r_impl <- function(q_f) {
  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_f(rand_q_vec)
  }
  res <- add_pdqr_class(res, "r_fun")

  copy_meta(res, q_f)
}
