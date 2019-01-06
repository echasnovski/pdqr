as_d <- function(f, ...) {
  UseMethod("as_d")
}

as_d.default <- function(f, support, n_grid = 10001, ...) {
  assert_missing_args("d", support = missing(support))
  assert_as_def_args(f, support, n_grid)

  x <- seq(support[1], support[2], length.out = n_grid)
  y <- f(x, ...)
  y <- impute_inf(x, y, '`f` output')

  # This doesn't change output computation results and is needed for correct
  # approximation of q-function in case `as_q()` is called
  x_tbl <- remove_zero_edge_y(data.frame(x = x, y = y))

  res <- new_d(x_tbl, "smooth")

  # Ensure that output has maximum available support (usually equal to
  # `support`)
  ensure_support(res, support)
}

as_d.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  res <- new_d(x = meta(f, "x_tbl"), type = meta(f, "type"))

  # Ensure that output has maximum available support (usually equal to
  # `meta(f, "support")`)
  ensure_support(res, meta(f, "support"))
}
