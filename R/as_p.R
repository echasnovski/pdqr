as_p <- function(f, ...) {
  if (inherits(f, "p")) {
    return(f)
  } else if (has_meta_type(f) && has_meta_x_tbl(f, meta(f, "type"))) {
    return(new_p(x = meta(f, "x_tbl"), type = meta(f, "type")))
  }

  UseMethod("as_p")
}

as_p.default <- function(f, support, ...) {
  assert_missing_args("p", support = missing(support))

  as_distr_impl_def("p", f, support, adjust_to_support_p, ...)
}

as_p.d <- function(f, n_grid = 10001, ...) {
  assert_pdqr_fun(f)

  support <- meta(f, "support")
  x_dens <- seq(from = support[1], to = support[2], length.out = n_grid)
  y_dens <- f(x_dens)

  res <- p_from_d_points(x_dens, y_dens)
  res <- add_pdqr_class(res, "p")

  copy_meta(res, f)
}

as_p.q <- function(f, n_grid = 10001, ...) {
  assert_pdqr_fun(f)

  support <- meta(f, "support")
  f_inv <- inversing(
    f, interval = c(0, 1), f_type = meta(f, "type"), n_grid = n_grid
  )

  res <- function(q) {
    out <- numeric(length(q))

    is_small_q <- q < support[1]
    is_big_q <- q > support[2]
    is_in_support <- !(is_small_q | is_big_q)

    # Extra cutoffs to respect precision errors
    out[is_in_support] <- pmin(pmax(f_inv(q[is_in_support]), 0), 1)
    out[is_small_q] <- 0
    out[is_big_q] <- 1

    out
  }
  res <- add_pdqr_class(res, "p")

  copy_meta(res, f)
}

as_p.r <- function(f, n_sample = 10000, ...) {
  assert_pdqr_fun(f)

  as_distr_impl_r(new_p, f, n_sample, ...)
}

adjust_to_support_p <- function(f, support) {
  min_p <- f(support[1])

  tot_prob <- f(support[2]) - min_p
  assert_tot_prob(tot_prob)

  res <- function(q) {
    out <- numeric(length(q))
    is_lower <- q < support[1]
    is_higher <- q >= support[2]
    is_inside <- !(is_lower | is_higher)

    out[is_inside] <- (f(q[is_inside]) - min_p) / tot_prob
    out[is_higher] <- 1

    out
  }

  copy_attrs(res, f)
}
