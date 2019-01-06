as_r <- function(f, ...) {
  UseMethod("as_r")
}

as_r.default <- function(f, support, n_grid = 10001, n_sample = 10000, ...) {
  assert_missing_args("r", support = missing(support))
  assert_as_def_args(f, support, n_grid)

  # Create sample
  smpl <- f(n_sample)
  # Create density function from sample
  d_f <- new_d(smpl, type = "smooth", ...)
  # Adjust to supplied support
  d_f <- as_d(unpdqr(d_f), support = support, n_grid = n_grid)

  # Convert to r-function
  as_r(d_f)
}

as_r.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  res <- new_r(x = meta(f, "x_tbl"), type = meta(f, "type"))

  # Ensure that output has maximum available support (usually equal to
  # `meta(f, "support")`)
  ensure_support(res, meta(f, "support"))
}
