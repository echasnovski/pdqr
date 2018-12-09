as_d <- function(f, ...) {
  if (inherits(f, "d_fun")) {
    return(f)
  } else if (has_meta_x(f) && has_meta_type(f)) {
    return(distr_from_meta(f, d_fun, ...))
  }

  UseMethod("as_d")
}

as_d.default <- function(f, type, support, extra = NULL, ...) {
  assert_missing_args(
    "d_fun", type = missing(type), support = missing(support)
  )

  as_distr_impl_def("d_fun", f, type, support, extra)
}

as_d.p_fun <- function(f, h = 10^(-6), ...) {
  assert_pdqr_fun(f)

  support <- meta(f, "support")
  type <- meta(f, "type")

  if (type == "raw") {
    res <- function(x) {f(x + h) - f(x - h)}
  } else {
    res <- function(x) {
      left_point <- pmax(x - h, support[1])
      right_point <- pmin(x + h, support[2])
      point_len <- right_point - left_point
      (f(right_point) - f(left_point)) / point_len
    }
  }

  res <- add_pdqr_class(res, "d_fun")

  copy_meta(res, f)
}

as_d.q_fun <- function(f, n_grid = 10001, ...) {
  as_d(as_p(f, n_grid = n_grid, ...))
}

as_d.r_fun <- function(f, n_sample = 10000, ...) {
  assert_pdqr_fun(f)

  as_distr_impl_r(d_fun, f, n_sample, ...)
}
