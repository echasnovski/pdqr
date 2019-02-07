# Common functionality for `new_*()` --------------------------------------
distr_impl <- function(fun_class, impl_funs, x, type, ...) {
  assert_distr_type(type)

  x_tbl <- impute_x_tbl(x, type, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- switch(
    type,
    fin = impl_funs[["fin"]](x_tbl),
    infin = impl_funs[["infin"]](x_tbl)
  )

  add_pdqr_class(res, fun_class)
}

add_pdqr_class <- function(f, subclass) {
  add_class(f, c(subclass, "pdqr"))
}

unpdqr <- function(f) {
  class(f) <- setdiff(class(f), c("p", "d", "q", "r", "pdqr"))

  f
}

filter_numbers <- function(x) {
  x_is_nan <- is.nan(x)
  if (any(x_is_nan)) {
    warning_collapse("`x` has `NaN`s, which are removed.")
  }
  x <- x[!x_is_nan]

  x_is_na <- is.na(x)
  if (any(x_is_na)) {
    warning_collapse("`x` has `NA`s, which are removed.")
  }
  x <- x[!x_is_na]

  x_is_inf <- is.infinite(x)
  if (any(x_is_inf)) {
    warning_collapse("`x` has infinite values, which are removed.")
  }

  x[!x_is_inf]
}


# Predicates for pdqr-functions -------------------------------------------
is_pdqr_fun <- function(f) {
  tryCatch(assert_pdqr_fun(f), error = function(e) {FALSE})
}

is_distr_type <- function(type) {
  tryCatch(assert_distr_type(type), error = function(e) {FALSE})
}

is_support <- function(supp, allow_na = FALSE) {
  tryCatch(assert_support(supp, allow_na), error = function(e) {FALSE})
}

is_x_tbl <- function(x, type) {
  tryCatch(assert_x_tbl(x, type), error = function(e) {FALSE})
}

is_x_tbl_meta <- function(x, type) {
  tryCatch(assert_x_tbl_meta(x, type), error = function(e) {FALSE})
}

is_pdqr_class <- function(chr) {
  chr %in% c("p", "d", "q", "r")
}

has_meta_type <- function(f) {
  has_meta(f, "type") && is_distr_type(meta_type(f))
}

has_meta_support <- function(f) {
  has_meta(f, "support") && is_support(meta_support(f))
}

has_meta_x_tbl <- function(f, type) {
  has_meta(f, "x_tbl") && is_x_tbl(meta_x_tbl(f), type) &&
    is_x_tbl_meta(meta_x_tbl(f), type)
}


# Piecewise linear density ------------------------------------------------
# Wrapper for `density()` assuming that output points will be the base for
# piecewise linear density function.
density_piecelin <- function(x, ...) {
  dens <- stats::density(x, ...)
  x_dens <- dens[["x"]]
  y_dens <- dens[["y"]]

  tot_integral <- trapez_integral(x_dens, y_dens)

  data.frame(x = x_dens, y = y_dens / tot_integral)
}

trapez_integral <- function(x, y) {
  n <- length(y)
  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  sum((x[-1] - x[-n]) * (y[-1] + y[-n])) / 2
}

trapez_part_integral <- function(x, y) {
  n <- length(y)
  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  c(0, cumsum((x[-1] - x[-n]) * (y[-1] + y[-n])) / 2)
}


# Coefficients of piecewise-linear density --------------------------------
compute_piecelin_density_coeffs <- function(x_dens, y_dens, ind_vec) {
  slope <- (y_dens[ind_vec+1] - y_dens[ind_vec]) /
    (x_dens[ind_vec+1] - x_dens[ind_vec])
  intercept <- y_dens[ind_vec] - slope * x_dens[ind_vec]

  list(slope = slope, intercept = intercept)
}
