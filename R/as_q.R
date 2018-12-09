as_q <- function(f, ...) {
  if (inherits(f, "q_fun")) {
    return(f)
  } else if (has_meta_x(f) && has_meta_type(f)) {
    return(distr_from_meta(f, q_fun, ...))
  }

  UseMethod("as_q")
}

as_q.default <- function(f, type, support, extra = NULL, ...) {
  assert_missing_args(
    "q_fun", type = missing(type), support = missing(support)
  )

  as_distr_impl_def("q_fun", f, type, support, extra)
}

as_q.p_fun <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  assert_pdqr_fun(f)

  support <- meta(f, "support")
  # Stretch support a little bit to ensure opposite signs of `f()` values at end
  # points for correct use in `inversing()`.
  ext_support <- stretch_range(support)
  f_inv <- inversing(
    f, interval = ext_support, f_type = meta(f, "type"),
    n_grid = n_grid
  )

  warn_conversion_from_p_raw(f, isTRUE(warn_precision), "quantile function")

  res <- function(p) {
    out <- numeric(length(p))

    is_more_0 <- p >= 0
    is_less_1 <- p <= 1
    is_inside <- (p > 0) & (p < 1)

    out[is_inside] <- round(f_inv(p[is_inside]), digits = 8)
    out[is_near(p, 0) & is_more_0] <- support[1]
    out[is_near(p, 1) & is_less_1] <- support[2]
    out[!(is_more_0 & is_less_1)] <- NaN

    out
  }
  res <- add_pdqr_class(res, "q_fun")

  copy_meta(res, f)
}

as_q.d_fun <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  # This conversion usually is very slow due to task nature: to compute "q_fun"
  # from "d_fun" one *needs* to compute "p_fun".
  as_q(
    as_p(f, warn_precision = warn_precision, ...),
    n_grid = n_grid, warn_precision = FALSE
  )
}

as_q.r_fun <- function(f, n_sample = 10000, ...) {
  assert_pdqr_fun(f)

  as_distr_impl_r(q_fun, f, n_sample, ...)
}
