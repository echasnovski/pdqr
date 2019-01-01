as_d <- function(f, ...) {
  UseMethod("as_d")
}

as_d.default <- function(f, support, n_grid = 10001, ...) {
  assert_missing_args("d", support = missing(support))
  assert_as_def_args(f, support, n_grid)

  x <- seq(support[1], support[2], length.out = n_grid)
  y <- f(x, ...)
  y <- impute_inf(x, y, '`f` output')

  new_d(data.frame(x = x, y = y), "smooth")
}

as_d.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  new_d(x = meta(f, "x_tbl"), type = meta(f, "type"))
}
