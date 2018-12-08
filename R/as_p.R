as_p <- function(f, ...) {
  if (inherits(f, "p_fun")) {
    return(f)
  } else if (has_meta_x(f) && has_meta_type(f)) {
    return(distr_from_meta(f, p_fun, ...))
  }

  UseMethod("as_p")
}

as_p.default <- function(f, type, support, extra = NULL, ...) {
  assert_missing_args(
    "p_fun", type = missing(type), support = missing(support)
  )
  assert_support(support)

  as_distr_impl_def("p_fun", f, type, support, extra)
}

as_p.d_fun <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  assert_pdqr_fun(f)

  res <- switch(
    meta(f, "type"),
    raw = p_from_d_raw(f, isTRUE(warn_precision)),
    smooth = p_from_d_smooth(f, n_grid = n_grid),
    stop_collapse('`f` should have "type" metadata equal to "raw" or "smooth".')
  )
  res <- add_pdqr_class(res, "p_fun")

  copy_meta(res, f)
}

as_p.q_fun <- function(f, n_grid = 10001, ...) {
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

    out[is_small_q] <- 0
    out[is_big_q] <- 1
    out[is_in_support] <- f_inv(q[is_in_support])

    out
  }
  res <- add_pdqr_class(res, "p_fun")

  copy_meta(res, f)
}

as_p.r_fun <- function(f, n_sample = 10000, ...) {
  assert_pdqr_fun(f)

  as_distr_impl_r(p_fun, f, n_sample, ...)
}

p_from_d_raw <- function(f, warn_precision = TRUE, ...) {
  if (warn_precision) {
    warning_collapse(
      'Converting from density function in case `type` = "raw" and no "x" in ',
      'metadata is not precise. Consider attaching `x` to input.'
    )
  }

  support <- detect_support_raw(f)
  supp_prob <- f(support)
  supp_cumprob <- c(0, cumsum(supp_prob) / sum(supp_prob))

  function(q) {
    q_ind <- findInterval(round(q, digits = 8), support) + 1

    supp_cumprob[q_ind]
  }
}

detect_support_raw <- function(f, n = 10000) {
  support <- meta(f, "support")
  # Detection is done with trying values with step which is multiple of 10^(-5)
  # This should increase probability of finding actual support
  step <- ceiling(10^5 * diff(support) / n) * 10^(-5)
  x_grid <- seq(support[1], support[2], by = step)

  x_grid[!is_near(f(x_grid), 0)]
}

p_from_d_smooth <- function(f, n_grid) {
  support <- meta(f, "support")
  x_dens <- seq(from = support[1], to = support[2], length.out = n_grid)
  y_dens <- f(x_dens)

  p_from_d_points(x_dens, y_dens)
}
