as_p <- function(f, ...) {
  UseMethod("as_p")
}

as_p.default <- function(f, support, n_grid = 10001, ...) {
  assert_missing_args("p", support = missing(support))
  assert_type(f, is.function)
  assert_support(support)
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 2", min_val = 3
  )

  x <- seq(support[1], support[2], length.out = n_grid)
  p <- f(x, ...)

  y <- y_from_p_grid(x, p)

  new_p(data.frame(x = x, y = y), "smooth")
}

as_p.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  new_p(x = meta(f, "x_tbl"), type = meta(f, "type"))
}
