as_r <- function(f, ...) {
  if (class(f)[1] == "r_fun") {
    return(f)
  } else if (has_meta(f, "x") && has_meta(f, "type")) {
    return(distr_from_meta(f, r_fun, ...))
  }

  UseMethod("as_r")
}

as_r.default <- function(f, type, domain_out, extra = NULL, ...) {
  assert_domain(domain_out, "domain_out")

  as_distr_impl_def("r_fun", f, type, extra, domain_out = domain_out)
}

as_r.p_fun <- function(f, warn_precision = TRUE, ...) {
  warn_conversion_from_p_raw(f, warn_precision, "random generation function")
  q_f <- as_q(f, warn_precision = FALSE, ...)

  as_r_impl(q_f)
}

as_r.d_fun <- function(f, warn_precision = TRUE, ...) {
  q_f <- as_q(f, warn_precision = warn_precision, ...)

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
  class(res) <- c("r_fun", "function")

  copy_meta(res, q_f)
}
