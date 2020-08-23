# Documentation of `new_*()` ----------------------------------------------
#' Create new pdqr-function
#'
#' Functions for creating new pdqr-functions based on numeric sample or data
#' frame describing distribution. They construct appropriate ["x_tbl"
#' metadata][meta_x_tbl()] based on the input and then create pdqr-function (of
#' corresponding [pdqr class][meta_class()]) defined by that "x_tbl".
#'
#' @param x Numeric vector or data frame with appropriate columns (see "Data
#'   frame input" section).
#' @param type [Type][meta_type()] of pdqr-function. Should be one of "discrete"
#'   or "continuous".
#' @param ... Extra arguments for [density()][stats::density()].
#'
#' @details Data frame input `x` is treated as having enough information for
#' creating (including normalization of "y" column) an "x_tbl" metadata. For
#' more details see "Data frame input" section.
#'
#' Numeric input is transformed into data frame which is then used as "x_tbl"
#' metadata (for more details see "Numeric input" section):
#' - If `type` is `"discrete"` then `x` is viewed as sample from distribution
#' that can produce only values from `x`. Input is tabulated and normalized to
#' form "x_tbl" metadata.
#' - If `type` is `"continuous"` then:
#'     - If `x` has 1 element, output distribution represents a **dirac-like**
#'     distribution which is an approximation to singular dirac distribution.
#'     - If `x` has more than 1 element, output distribution represents a
#'     **density estimation** with [density()][stats::density()] treating `x` as
#'     sample.
#'
#' @section Numeric input:
#'
#' If `x` is a numeric vector, it is transformed into a data frame which is then
#' used as ["x_tbl" metadata][meta_x_tbl()] to create pdqr-function of
#' corresponding class.
#'
#' First, all `NaN`, `NA`, and infinite values are removed with warnings. If
#' there are no elements left, error is thrown. Then data frame is created in
#' the way which depends on the `type` argument.
#'
#' **For "discrete" type** elements of filtered `x` are:
#' - Rounded to 10th digit to avoid numerical representation issues (see Note
#' in [`==`]'s help page).
#' - Tabulated (all unique values are counted). Output data frame has three
#' columns: "x" with unique values, "prob" with normalized (divided by sum)
#' counts, "cumprob" with cumulative sum of "prob" column.
#'
#' **For "continuous" type** output data frame has columns "x", "y", "cumprob".
#' Choice of algorithm depends on the number of `x` elements:
#' - If `x` has 1 element, an "x_tbl" metadata describes **dirac-like**
#' "continuous" pdqr-function. It is implemented as triangular peak with center
#' at `x`'s value and width of `2e-8` (see Examples). This is an approximation
#' of singular dirac distribution. Data frame has columns "x" with value
#' `c(x-1e-8, x, x+1e-8)`, "y" with value `c(0, 1e8, 0)` normalized to have
#' total integral of "x"-"y" points of 1, "cumprob" `c(0, 0.5, 1)`.
#' - If `x` has more than 1 element, it serves as input to
#' [density(x, ...)][stats::density()] for density estimation (here arguments in
#' `...` of `new_*()` serve as extra arguments to `density()`). The output's "x"
#' element is used as "x" column in output data frame. Column "y" is taken as
#' "y" element of `density()` output, normalized so that piecewise-linear
#' function passing through "x"-"y" points has total integral of 1. Column
#' "cumprob" has cumulative probability of piecewise-linear d-function.
#'
#' @section Data frame input:
#'
#' If `x` is a data frame, it should have numeric columns appropriate for
#' ["x_tbl" metadata][meta_x_tbl()] of input `type`: "x", "prob" for "discrete"
#' `type` and "x", "y" for "continuous" type ("cumprob" column will be computed
#' inside `new_*()`). To become an appropriate "x_tbl" metadata, input data
#' frame is ordered in increasing order of "x" column and then **imputed** in
#' the way which depends on the `type` argument.
#'
#' **For "discrete" type**:
#' - Values in column "x" are rounded to 10th digit to avoid numerical
#' representation issues (see Note in [`==`]'s help page).
#' - If there are duplicate values in "x" column, they are "squashed" into one
#' having sum of their probability in "prob" column.
#' - Column "prob" is normalized by its sum to have total sum of 1.
#' - Column "cumprob" is computed as cumulative sum of "prob" column.
#'
#' **For "continuous" type** column "y" is normalized so that piecewise-linear
#' function passing through "x"-"y" points has total integral of 1. Column
#' "cumprob" has cumulative probability of piecewise-linear d-function.
#'
#' @return A pdqr-function of corresponding [class][meta_class()] ("p" for
#'   `new_p()`, etc.) and [type][meta_type()].
#'
#' @examples
#' set.seed(101)
#' x <- rnorm(10)
#'
#' # Type "discrete": `x` values are directly tabulated
#' my_d_dis <- new_d(x, "discrete")
#' meta_x_tbl(my_d_dis)
#' plot(my_d_dis)
#'
#' # Type "continuous": `x` serves as input to `density()`
#' my_d_con <- new_d(x, "continuous")
#' head(meta_x_tbl(my_d_con))
#' plot(my_d_con)
#'
#' # Data frame input
#' ## Values in "prob" column will be normalized automatically
#' my_p_dis <- new_p(data.frame(x = 1:4, prob = 1:4), "discrete")
#' ## As are values in "y" column
#' my_p_con <- new_p(data.frame(x = 1:3, y = c(0, 10, 0)), "continuous")
#'
#' # Using bigger bandwidth in `density()`
#' my_d_con_2 <- new_d(x, "continuous", adjust = 2)
#' plot(my_d_con, main = "Comparison of density bandwidths")
#' lines(my_d_con_2, col = "red")
#'
#' # Dirac-like "continuous" pdqr-function is created if `x` is a single number
#' meta_x_tbl(new_d(1, "continuous"))
#' @name new-pdqr
NULL


# Common functionality for `new_*()` --------------------------------------
distr_impl <- function(pdqr_class, impl_funs, x, type, ...) {
  assert_missing(x, "numeric vector or appropriate data frame")
  assert_missing(type, 'pdqr type ("discrete" or "continuous")')
  assert_pdqr_type(type)

  x_tbl <- impute_x_tbl(x, type, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- switch(
    type,
    discrete = impl_funs[["discrete"]](x_tbl),
    continuous = impl_funs[["continuous"]](x_tbl)
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
  # Force performing assertions in order to have chance of getting `FALSE` in
  # case option "pdqr.assert_args" is disabled
  enable_asserting_locally()

  tryCatch(assert_pdqr_fun(f), error = function(e) {
    FALSE
  })
}

is_pdqr_type <- function(type) {
  # Force performing assertions in order to have chance of getting `FALSE` in
  # case option "pdqr.assert_args" is disabled
  enable_asserting_locally()

  tryCatch(assert_pdqr_type(type), error = function(e) {
    FALSE
  })
}

is_support <- function(supp, allow_na = FALSE) {
  # Force performing assertions in order to have chance of getting `FALSE` in
  # case option "pdqr.assert_args" is disabled
  enable_asserting_locally()

  tryCatch(assert_support(supp, allow_na), error = function(e) {
    FALSE
  })
}

is_x_tbl <- function(x, type) {
  # Force performing assertions in order to have chance of getting `FALSE` in
  # case option "pdqr.assert_args" is disabled
  enable_asserting_locally()

  tryCatch(assert_x_tbl(x, type), error = function(e) {
    FALSE
  })
}

is_x_tbl_meta <- function(x, type) {
  # There is no `enable_asserting_locally()` because this is a helper function

  tryCatch(assert_x_tbl_meta(x, type), error = function(e) {
    FALSE
  })
}

is_pdqr_class <- function(chr) {
  chr %in% c("p", "d", "q", "r")
}

is_boolean_pdqr_fun <- function(f) {
  (meta_type(f) == "discrete") && identical(meta_x_tbl(f)[["x"]], c(0, 1))
}

has_meta_type <- function(f) {
  has_meta(f, "type") && is_pdqr_type(meta_type(f))
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

  slope[ind_is_in] <- (y[inds_in + 1] - y[inds_in]) /
    (x[inds_in + 1] - x[inds_in])
  intercept[ind_is_in] <- y[inds_in] - slope[ind_is_in] * x[inds_in]

  list(slope = slope, intercept = intercept)
}
