# Common functionality for `*_fun()` --------------------------------------
distr_impl <- function(fun_class, impl_funs, x, type, attach_x, extra,
                       ...) {
  assert_common_args(x, type, attach_x)
  x <- filter_numbers(x)
  if (length(x) == 0) {
    stop_collapse("`x` shouldn't be empty.")
  }

  fun <- switch(
    type,
    raw = impl_funs[["raw"]](x),
    smooth = impl_funs[["smooth"]](x, ...)
  )

  res <- add_common_meta(
    fun, x = x, type = type, attach_x = attach_x, extra = extra
  )

  add_pdqr_class(res, fun_class)
}

add_pdqr_class <- function(f, subclass) {
  add_class(f, c(subclass, "pdqr_fun"))
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

assert_common_args <- function(x, type, attach_x) {
  assert_type(x, is.numeric)

  assert_distr_type(type)

  assert_type(
    attach_x, function(x) {identical(x, TRUE) || identical(x, FALSE)},
    "`TRUE` or `FALSE`"
  )

  x
}

assert_distr_type <- function(type) {
  assert_type(type, is_string)
  if (!(type %in% c("raw", "smooth"))) {
    stop_collapse(
      '`type` should be one of "raw" or "smooth", not ', type, "."
    )
  }

  type
}

add_common_meta <- function(obj, x, type = "smooth", attach_x = TRUE,
                            extra = NULL) {
  res <- add_meta_cond(obj, attach_x, x = x)
  res <- add_meta_cond(res, !is.null(extra), extra = extra)
  res <- add_meta(res, type = type)

  res
}

is_support <- function(supp) {
  is.numeric(supp) && (length(supp) == 2) &&
    (supp[1] <= supp[2])
}

is_pdqr_fun <- function(obj) {
  is.function(obj) && inherits(obj, "pdqr_fun")
}

is_pdqr_class <- function(chr) {
  chr %in% paste0(c("p", "d", "q", "r"), "_fun")
}


# Piecewise linear density ------------------------------------------------
# Wrapper for `density()` assuming that output points will be the base for
# piecewise linear density function.
density_piecelin <- function(x, ...) {
  dens <- stats::density(x, ...)
  x_dens <- dens[["x"]]
  y_dens <- dens[["y"]]

  tot_integral <- trapez_integral(x_dens, y_dens)

  list(x = x_dens, y = y_dens / tot_integral)
}

trapez_integral <- function(x, y) {
  n <- length(y)
  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  sum(diff(x) * (y[-1] + y[-n])) / 2
}

trapez_part_integral <- function(x, y) {
  n <- length(y)
  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  c(0, cumsum(diff(x) * (y[-1] + y[-n])) / 2)
}


# CDF from points of piecewise-linear density -----------------------------
p_from_d_points <- function(x_dens, y_dens) {
  n <- length(x_dens)
  p_grid <- trapez_part_integral(x_dens, y_dens)
  slope_vec <- diff(y_dens) / diff(x_dens)
  inter_vec <- y_dens[-n] - slope_vec * x_dens[-n]

  res <- function(q) {
    out <- numeric(length(q))

    q_ind <- findInterval(q, x_dens)

    is_q_small <- q_ind == 0
    out[is_q_small] <- 0
    is_q_large <- q_ind == length(x_dens)
    out[is_q_large] <- 1

    is_q_between <- !(is_q_small | is_q_large)
    q_ind_bet <- q_ind[is_q_between]
    q_bet <- q[is_q_between]
    x_bet <- x_dens[q_ind_bet]
    # Exact integration of density linear interpolation
    out_between <- p_grid[q_ind_bet] +
      0.5 * slope_vec[q_ind_bet] * (q_bet * q_bet - x_bet * x_bet) +
      inter_vec[q_ind_bet] * (q_bet - x_bet)
    # Extra cutoffs to respect floating point precision (~10^(-15))
    out[is_q_between] <- pmin(pmax(out_between, 0), 1)

    out
  }

  # For efficient memory management
  rm(list = "y_dens", envir = environment())

  res
}
