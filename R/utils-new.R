# Documentation of `new_*()` ----------------------------------------------
#' Create new pdqr-function
#'
#' Main tools to create new distribution pdqr-functions based on numeric sample
#' or data frame describing distribution.
#'
#' @param x Numeric vector or data frame with appropriate columns (see Details).
#' @param type Type of pdqr-function. One of "fin" or "infin" (see Details).
#' @param ... Extra arguments for [density()][stats::density()].
#'
#' @details Symbol "~" in `print()` output indicates that printed value is an
#' approximation to a true one.
#'
#' @return A pdqr-function of corresponding [class][meta_class()] and
#'   [type][meta_type()].
#'
#' @examples
#' set.seed(101)
#' x <- rnorm(10)
#' my_d_infin <- new_d(x, "infin")
#' my_d_fin <- new_d(x, "fin")
#'
#' my_p_fin <- new_p(data.frame(x = 1:10, prob = rep(1, 10)/10), "fin")
#'
#' # Using bigger bandwidth in `density()`
#' my_d_infin_2 <- new_d(x, "infin", adjust = 2)
#'
#' @name new-pdqr
NULL


# Common functionality for `new_*()` --------------------------------------
distr_impl <- function(pdqr_class, impl_funs, x, type, ...) {
  assert_distr_type(type)

  x_tbl <- impute_x_tbl(x, type, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- switch(
    type,
    fin = impl_funs[["fin"]](x_tbl),
    infin = impl_funs[["infin"]](x_tbl)
  )

  add_pdqr_class(res, pdqr_class)
}

add_pdqr_class <- function(f, pdqr_class) {
  add_class(f, c(pdqr_class, "pdqr"))
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

is_boolean_pdqr_fun <- function(f) {
  (meta_type(f) == "fin") && identical(meta_x_tbl(f)[["x"]], c(0, 1))
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
  sum(trapez_piece_integral(x, y))
}

trapez_piece_integral <- function(x, y) {
  n <- length(y)

  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  (x[-1] - x[-n]) * (y[-1] + y[-n]) / 2
}

trapez_part_integral <- function(x, y) {
  n <- length(y)
  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  c(0, cumsum((x[-1] - x[-n]) * (y[-1] + y[-n])) / 2)
}


# Coefficients of piecewise-linear density --------------------------------
compute_piecelin_density_coeffs <- function(x_tbl, ind_vec) {
  n <- length(ind_vec)
  slope <- numeric(n)
  intercept <- numeric(n)

  x <- x_tbl[["x"]]
  y <- x_tbl[["y"]]

  ind_is_in <- (ind_vec >= 1) & (ind_vec < length(x))
  inds_in <- ind_vec[ind_is_in]

  slope[ind_is_in] <- (y[inds_in+1] - y[inds_in]) / (x[inds_in+1] - x[inds_in])
  intercept[ind_is_in] <- y[inds_in] - slope[ind_is_in] * x[inds_in]

  list(slope = slope, intercept = intercept)
}
