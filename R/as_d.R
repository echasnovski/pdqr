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

impute_inf <- function(x, y, vec_name) {
  is_inf <- is.infinite(y)
  is_fin <- is.finite(y)
  if (!all(is_inf | is_fin)) {
    stop_collapse(
      "All values in ", vec_name, " should be finite or infinite numbers ",
      "(with no `NA`s)."
    )
  }
  if (sum(is_fin) < 3) {
    stop_collapse(vec_name, " should have at least 3 finite values.")
  }
  if (!any(is_inf)) {
    return(y)
  }

  inf_inds <- which(is_inf)
  x_inf <- x[is_inf]

  fin_inds <- which(is_fin)
  x_fin <- x[is_fin]
  y_fin <- y[is_fin]

  l_ind <- findInterval(inf_inds, fin_inds)

  # Extrapolate linearly based on two nearest *from left* points with finite
  # `y`. If there are none, return `-Inf`.
  left_imputation <- impute_linearly(
    ind_1 = l_ind - 1, ind_2 = l_ind,
    x = x_fin, y = y_fin, x_target = x_inf,
    direction = "left"
  )
  # Extrapolate linearly based on two nearest *from right* points with finite
  # `y`. If there are none, return `-Inf`.
  right_imputation <- impute_linearly(
    ind_1 = l_ind + 1, ind_2 = l_ind + 2,
    x = x_fin, y = y_fin, x_target = x_inf,
    direction = "right"
  )

  # Compute maximum from two linear imputations
  y[is_inf] <- pmax(left_imputation, right_imputation)

  y
}

impute_linearly <- function(ind_1, ind_2, x, y, x_target, direction) {
  res <- numeric(length(x_target))
  condition_bad <- switch(
    direction, left = ind_2 < 2, right = ind_1 > length(x) - 1
  )
  res[condition_bad] <- -Inf

  not_bad <- !condition_bad
  res[not_bad] <- extrap_lin(
    x_1 = x[ind_1[not_bad]], x_2 = x[ind_2[not_bad]],
    y_1 = y[ind_1[not_bad]], y_2 = y[ind_2[not_bad]],
    x_target = x_target[not_bad]
  )

  res
}
