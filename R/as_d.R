as_d <- function(f, ...) {
  UseMethod("as_d")
}

as_d.default <- function(f, support, n_grid = 10001, ...) {
  assert_missing_args("d", support = missing(support))
  assert_type(f, is.function)
  assert_support(support)
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 2", min_val = 3
  )

  x <- seq(support[1], support[2], length.out = n_grid)
  y <- f(x, ...)
  y <- impute_inf(x, y, '`f` output')

  new_d(data.frame(x = x, y = y), "smooth")
}

as_d.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  new_d(x = meta(f, "x_tbl"), type = meta(f, "type"))
}
