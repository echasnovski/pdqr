as_q <- function(f, ...) {
  if (class(f)[1] == "q_fun") {
    return(f)
  } else if (has_meta(f, "x") && has_meta(f, "type")) {
    return(distr_from_meta(f, q_fun, ...))
  }

  UseMethod("as_q")
}

as_q.default <- function(f, type, domain_out, extra = NULL, ...) {
  assert_domain(domain_out, "domain_out")

  as_distr_impl_def("q_fun", f, type, extra, domain_out = domain_out)
}

as_q.p_fun <- function(f, warn_precision = TRUE, ...) {
  domain_in <- meta(f, "domain_in")
  # Stretch input domain a little bit to ensure opposite signs of `f()` values
  # at end points for correct use of `uniroot()` during `inverse()` output.
  ext_domain_in <- domain_in + 10^(-6) * c(-1, 1)
  f_inv <- inverse(f, interval = ext_domain_in, tol = sqrt(.Machine$double.eps))

  warn_conversion_from_p_raw(f, isTRUE(warn_precision), "quantile function")

  res <- function(p) {
    out <- numeric(length(p))

    is_more_0 <- p >= 0
    is_less_1 <- p <= 1
    is_inside <- (p > 0) & (p < 1)

    out[is_inside] <- f_inv(p[is_inside])
    out[is_near(p, 0) & is_more_0] <- domain_in[1]
    out[is_near(p, 1) & is_less_1] <- domain_in[2]
    out[!(is_more_0 & is_less_1)] <- NaN

    out
  }
  class(res) <- c("q_fun", "function")

  res <- add_meta(res, type = meta(f, "type"), domain_out = domain_in)

  add_meta_cond(res, !is.null(meta(f, "extra")), meta(f, "extra"))
}

as_q.d_fun <- function(f, warn_precision = TRUE, ...) {
  # This conversion usually is very slow due to task nature: to compute "q_fun"
  # from "d_fun" one *needs* to compute "p_fun".
  as_q(as_p(f, warn_precision = warn_precision, ...), warn_precision = FALSE)
}

as_q.r_fun <- function(f, n_sample = 10000, ...) {
  as_distr_impl_r(q_fun, f, n_sample, ...)
}
