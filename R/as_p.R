as_p <- function(f, ...) {
  if (class(f)[1] == "p_fun") {
    return(f)
  } else if (has_meta(f, "x") && has_meta(f, "type")) {
    return(p_fun(
      x = meta(f, "x"),
      type = meta(f, "type"),
      attach_x = TRUE,
      extra = meta(f, "extra"),
      ...
    ))
  }

  UseMethod("as_p")
}

as_p.default <- function(f, type, domain_in, extra = NULL, ...) {
  assert_domain(domain_in, "domain_in")

  as_distr_impl_def("p_fun", f, type, extra, domain_in = domain_in)
}

as_p.d_fun <- function(f, ...) {
  res <- switch(
    meta(f, "type"),
    raw = p_from_d_raw(f),
    smooth = p_from_d_smooth(f),
    stop_collapse('`f` should have "type" metadata equal to "raw" or "smooth".')
  )
  res <- structure(res, class = c("p_fun", "function"))

  copy_meta(res, f)
}

as_p.q_fun <- function(f, ...) {
  domain_out <- meta(f, "domain_out")
  f_inv <- inverse(f, interval = c(0, 1), tol = sqrt(.Machine$double.eps))

  fun <- function(q) {
    out <- numeric(length(q))

    is_small_q <- q <= domain_out[1]
    is_big_q <- q >= domain_out[2]
    is_in_domain <- !(is_small_q | is_big_q)

    out[is_small_q] <- 0
    out[is_big_q] <- 1
    out[is_in_domain] <- f_inv(q[is_in_domain])

    out
  }

  res <- structure(fun, class = c("p_fun", "function"))
  res <- add_meta(res, type = meta(f, "type"), domain_in = domain_out)

  add_meta_cond(res, is.null(meta(f, "extra")), meta(f, "extra"))
}

as_p.r_fun <- function(f, n_sample = 10000, ...) {
  as_distr_impl_r(p_fun, f, n_sample, ...)
}

p_from_d_raw <- function(f, ...) {
  warning_collapse(
    'Converting from density function in case `type` = "raw" and no "x" in ',
    'meta data is not precise. Consider attaching `x` to input.'
  )

  support <- detect_raw_support(f)
  supp_prob <- f(support)
  supp_cumprob <- c(0, cumsum(supp_prob) / sum(supp_prob))

  function(q) {
    q_ind <- findInterval(round(q, digits = 8), support) + 1

    supp_cumprob[q_ind]
  }
}

detect_raw_support <- function(f, n = 10000) {
  domain <- meta(f, "domain_in")
  # Detection is done with trying values with step which is multiple of 10^(-5)
  # This should increase probability of finding actual support
  step <- ceiling(10^5 * diff(domain) / n) * 10^(-5)
  x_grid <- seq(domain[1], domain[2], by = step)

  x_grid[!is_near(f(x_grid), 0)]
}

p_from_d_smooth <- function(f) {
  domain <- meta(f, "domain_in")

  function(q) {
    out <- numeric(length(q))

    is_q_small <- q <= domain[1]
    out[is_q_small] <- 0
    is_q_large <- q >= domain[2]
    out[is_q_large] <- 1


    is_q_between <- !(is_q_small | is_q_large)
    out_between <- integrate_right(f, q[is_q_between], domain[1])
    # Extra cutoffs to respect floating point precision (~10^(-15))
    out[is_q_between] <- pmin(pmax(out_between, 0), 1)

    out
  }
}

integrate_right <- function(f, at, from) {
  # This "partial" approach should be faster if many `at` values are
  # substantially bigger than `from`
  at_order <- order(at)
  at_sorted <- at[at_order]
  at_grid <- c(from, at_sorted)

  partial_integrals <- vapply(
    seq_along(at),
    function(i) {
      stats::integrate(f, at_grid[i], at_grid[i + 1])[["value"]]
    },
    numeric(1)
  )

  integral_sorted <- cumsum(partial_integrals)

  integral_sorted[order(at_order)]
}
