# Common functionality for `new_*()` --------------------------------------
distr_impl <- function(fun_class, impl_funs, x, type, ...) {
  assert_common_args(x, type)
  x <- filter_numbers(x)
  if (length(x) == 0) {
    stop_collapse("`x` shouldn't be empty.")
  }

  fun <- switch(
    type,
    raw = impl_funs[["raw"]](x),
    smooth = impl_funs[["smooth"]](x, ...)
  )

  res <- add_meta(fun, type = type)

  add_pdqr_class(res, fun_class)
}

compute_raw_tbl <- function(x, vals = sort(unique(x))) {
  x <- x[!is.na(x)]

  x_val_id <- match(x, vals)
  val_n <- tabulate(x_val_id)
  prob <- val_n / length(x)

  data.frame(x = vals, prob = prob, n = val_n)
}

add_pdqr_class <- function(f, subclass) {
  add_class(f, c(subclass, "pdqr"))
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

assert_common_args <- function(x, type) {
  assert_type(x, is.numeric)
  assert_distr_type(type)

  x
}

is_support <- function(supp) {
  is.numeric(supp) && (length(supp) == 2) &&
    (supp[1] <= supp[2]) && all(is.finite(supp))
}

is_raw_tbl <- function(x) {
  tryCatch(assert_raw_tbl(x), error = function(e) {FALSE})
}

is_pdqr_fun <- function(obj) {
  is.function(obj) && inherits(obj, "pdqr") &&
    has_meta_type(obj) && has_meta_support(obj)
}

is_pdqr_class <- function(chr) {
  chr %in% c("p", "d", "q", "r")
}

has_meta_type <- function(f) {
  has_meta(f, "type") && (meta(f, "type") %in% c("raw", "smooth"))
}

has_meta_support <- function(f) {
  has_meta(f, "support") && is_support(meta(f, "support"))
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
