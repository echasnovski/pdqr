as_p <- function(f, ...) {
  UseMethod("as_p")
}

as_p.default <- function(f, support, n_grid = 10001, ...) {
  assert_missing_args("p", support = missing(support))
  assert_as_def_args(f, support, n_grid)

  x <- seq(support[1], support[2], length.out = n_grid)
  p <- f(x, ...)

  y <- y_from_p_grid(x, p)

  # This doesn't change output computation results and is needed for correct
  # approximation of q-function in case `as_q()` is called
  x_tbl <- remove_zero_edge_y(data.frame(x = x, y = y))

  res <- new_p(x_tbl, "smooth")

  # Ensure that output has maximum available support (usually equal to
  # `support`)
  ensure_support(res, support)
}

as_p.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  res <- new_p(x = meta(f, "x_tbl"), type = meta(f, "type"))

  # Ensure that output has maximum available support (usually equal to
  # `meta(f, "support")`)
  ensure_support(res, meta(f, "support"))
}
