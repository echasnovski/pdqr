as_q <- function(f, ...) {
  UseMethod("as_q")
}

as_q.default <- function(f, support, n_grid = 10001, ...) {
  assert_missing_args("q", support = missing(support))
  assert_as_def_args(f, support, n_grid)

  p_f <- inversing(f, c(0, 1), n_grid = n_grid, ...)

  as_q(as_p(p_f, support, n_grid))
}

as_q.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  res <- new_q(x = meta(f, "x_tbl"), type = meta(f, "type"))

  # Ensure that output has maximum available support (usually equal to
  # `meta(f, "support")`)
  ensure_support(res, meta(f, "support"))
}
