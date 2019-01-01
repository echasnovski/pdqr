as_q <- function(f, ...) {
  UseMethod("as_q")
}

as_q.default <- function(f, support, n_grid = 10001, ...) {
  assert_missing_args("p", support = missing(support))
  assert_as_def_args(f, support, n_grid)

  p_f <- inversing(f, c(0, 1), n_grid = n_grid, ...)
  p_f <- as_p(p_f, support, n_grid)

  # Remove small `x` corresponding to exactly 0 cumulative probability.
  # This is needed for returning `q(0)` based on present values and not on
  # supplied `support`. In other words, when `f` is beta distribution and
  # `support` is c(-1, 1), `q(0)` should be equal to 0 and not to -1.
  p_x_tbl <- meta(p_f, "x_tbl")
  is_cumprob_zero <- p_x_tbl[["cumprob"]] == 0
  if (sum(is_cumprob_zero) > 1) {
    is_small_x <- duplicated.default(is_cumprob_zero, fromLast = TRUE)
    p_x_tbl <- p_x_tbl[!is_small_x | !is_cumprob_zero, ]
  }

  res <- new_q(p_x_tbl, "smooth")

  # Ensure that output has support exactly equal to input one.
  add_meta(res, support = support)
}

as_q.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  new_q(x = meta(f, "x_tbl"), type = meta(f, "type"))
}
