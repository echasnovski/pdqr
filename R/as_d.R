as_d <- function(f, ...) {
  if (inherits(f, "d")) {
    return(f)
  }

  UseMethod("as_d")
}

as_d.default <- function(f, support, ...) {
  assert_missing_args("d", support = missing(support))

  as_distr_impl_def("d", f, support, adjust_to_support_d)
}

as_d.p <- function(f, h = 10^(-6), ...) {
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

  res <- add_pdqr_class(res, "d")

  copy_meta(res, f)
}

as_d.q <- function(f, n_grid = 10001, ...) {
  as_d(as_p(f, n_grid = n_grid, ...))
}

as_d.r <- function(f, n_sample = 10000, ...) {
  assert_pdqr_fun(f)

  as_distr_impl_r(new_d, f, n_sample, ...)
}

adjust_to_support_d <- function(f, support) {
  tot_prob <- stats::integrate(f, support[1], support[2])$value
  assert_tot_prob(tot_prob)

  copy_attrs(adjust_d_impl(f, support, tot_prob), f)
}

adjust_d_impl <- function(f, support, tot_prob) {
  function(x) {
    res <- numeric(length(x))
    is_inside <- (x >= support[1]) & (x <= support[2])

    res[is_inside] <- f(x[is_inside]) / tot_prob

    res
  }
}
