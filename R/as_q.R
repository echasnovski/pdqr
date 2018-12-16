as_q <- function(f, ...) {
  if (inherits(f, "q")) {
    return(f)
  }

  UseMethod("as_q")
}

as_q.default <- function(f, type, support, ...) {
  assert_missing_args(
    "q", type = missing(type), support = missing(support)
  )

  as_distr_impl_def("q", f, type, support, adjust_to_support_q)
}

as_q.p <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  assert_pdqr_fun(f)

  support <- meta(f, "support")
  # Stretch support a little bit to ensure opposite signs of `f()` values at end
  # points for correct use in `inversing()`.
  ext_support <- stretch_range(support)
  f_inv <- inversing(
    f, interval = ext_support, f_type = meta(f, "type"),
    n_grid = n_grid
  )

  res <- function(p) {
    out <- numeric(length(p))

    is_inside <- (p >= 0) & (p <= 1)

    # Extra cutoffs to respect precision errors and support boundaries
    out[is_inside] <- pmin(pmax(f_inv(p[is_inside]), support[1]), support[2])
    out[!is_inside] <- NaN

    out
  }
  res <- add_pdqr_class(res, "q")

  copy_meta(res, f)
}

as_q.d <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  as_q(
    as_p(f, warn_precision = warn_precision, ...),
    n_grid = n_grid, warn_precision = FALSE
  )
}

as_q.r <- function(f, n_sample = 10000, ...) {
  assert_pdqr_fun(f)

  as_distr_impl_r(new_q, f, n_sample, ...)
}

adjust_to_support_q <- function(f, type, support) {
  f_inv <- inversing(f, interval = c(0, 1), f_type = type)
  p_f <- function(q) {
    out <- numeric(length(q))

    is_small_q <- q < f(0)
    is_big_q <- q > f(1)
    is_in_support <- !(is_small_q | is_big_q)

    out[is_small_q] <- 0
    out[is_big_q] <- 1
    out[is_in_support] <- f_inv(q[is_in_support])

    out

  }

  new_p <- as_p(p_f, type = type, support = support)

  copy_attrs(as_q(new_p, warn_precision = FALSE), f)
}
