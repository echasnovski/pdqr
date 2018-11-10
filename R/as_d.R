as_d <- function(f, ...) {
  if (inherits(f, "d_fun")) {
    return(f)
  } else if (has_meta(f, "x") && has_meta(f, "type")) {
    return(distr_from_meta(f, d_fun, ...))
  }

  UseMethod("as_d")
}

as_d.default <- function(f, type, support, extra = NULL, ...) {
  assert_missing_args(
    "d_fun", type = missing(type), support = missing(support)
  )
  assert_support(support)

  as_distr_impl_def("d_fun", f, type, support, extra)
}

as_d.p_fun <- function(f, ...) {
  small_h <- (.Machine$double.eps)^(1/3)
  big_h <- switch(
    meta(f, "type"),
    raw = 0.5,
    smooth = small_h,
    stop_collapse('`f` should have "type" metadata equal to "raw" or "smooth".')
  )

  res <- function(x) {
    (f(x + small_h) - f(x - small_h)) / (2 * big_h)
  }
  res <- add_class(res, "d_fun")

  copy_meta(res, f)
}

as_d.q_fun <- function(f, ...) {
  as_d(as_p(f, ...))
}

as_d.r_fun <- function(f, n_sample = 10000, ...) {
  as_distr_impl_r(d_fun, f, n_sample, ...)
}
