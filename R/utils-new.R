# Common functionality for `new_*()` --------------------------------------
distr_impl <- function(fun_class, impl_funs, x, type, ...) {
  assert_distr_type(type)

  x_tbl <- impute_x_tbl(x, type, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  fun <- switch(
    type,
    raw = impl_funs[["raw"]](x_tbl),
    smooth = impl_funs[["smooth"]](x_tbl)
  )

  res <- add_meta(fun, type = type)

  add_pdqr_class(res, fun_class)
}

impute_x_tbl <- function(x, type, ...) {
  if (is.numeric(x)) {
    x <- filter_numbers(x)
    if (length(x) == 0) {
      stop_collapse("`x` shouldn't be empty.")
    }

    compute_x_tbl(x, type, ...)
  } else if (is.data.frame(x)) {
    assert_x_tbl(x, type)

    impute_x_tbl_impl(x, type)
  } else {
    stop_collapse("`x` should be numeric vector or data frame.")
  }
}

impute_x_tbl_impl <- function(x_tbl, type) {
  # `if` is used for effecient memory management because calling `[` creates
  # copy of an object
  if (is.unsorted(x_tbl[["x"]])) {
    x_tbl <- x_tbl[order(x_tbl[["x"]]), ]
  }

  if (type == "raw") {
    res <- data.frame(
      x = x_tbl[["x"]],
      prob = impute_prob(x_tbl[["prob"]])
    )
    res[["cumprob"]] <- impute_vec(
      vec = x_tbl[["cumprob"]], new_vec = cumsum(res[["prob"]])
    )
  } else if (type == "smooth") {
    res <- data.frame(
      x = x_tbl[["x"]],
      y = impute_y(x_tbl[["y"]], x_tbl[["x"]])
    )
    res[["cumprob"]] <- impute_vec(
      vec = x_tbl[["cumprob"]],
      new_vec = trapez_part_integral(res[["x"]], res[["y"]])
    )
  } else {
    stop("Wrong `type`.")
  }

  res
}

# Extra property checks are needed to avoid creating unnecessary copies
impute_prob <- function(prob) {
  tot_prob <- sum(prob)

  if (is_near(tot_prob, 1)) {
    prob
  } else {
    prob / tot_prob
  }
}

impute_y <- function(y, x) {
  tot_prob <- trapez_integral(x, y)

  if (is_near(tot_prob, 1)) {y} else {y / tot_prob}
}

impute_vec <- function(vec, new_vec) {
  if (is.null(vec) || !all(is_near(vec, new_vec))) {
    new_vec
  } else {
    vec
  }
}

compute_x_tbl <- function(x, type, ...) {
  switch(
    type,
    raw = compute_x_tbl_raw(x),
    smooth = compute_x_tbl_smooth(x, ...)
  )
}

compute_x_tbl_raw <- function(x, vals = sort(unique(x))) {
  x <- x[!is.na(x)]

  x_val_id <- match(x, vals)
  prob <- tabulate(x_val_id) / length(x)
  cumprob <- cumsum(prob)

  data.frame(x = vals, prob = prob, cumprob = cumprob)
}

compute_x_tbl_smooth <- function(x, ...) {
  res <- density_piecelin(x, ...)
  res[["cumprob"]] <- trapez_part_integral(res[["x"]], res[["y"]])

  res
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


# Predicates for pdqr-functions -------------------------------------------
is_pdqr_fun <- function(f) {
  tryCatch(assert_pdqr_fun(f), error = function(e) {FALSE})
}

is_distr_type <- function(type) {
  tryCatch(assert_distr_type(type), error = function(e) {FALSE})
}

is_support <- function(supp) {
  tryCatch(assert_support(supp), error = function(e) {FALSE})
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
  has_meta(f, "type") && is_distr_type(meta(f, "type"))
}

has_meta_support <- function(f) {
  has_meta(f, "support") && is_support(meta(f, "support"))
}

has_meta_x_tbl <- function(f, type) {
  has_meta(f, "x_tbl") && is_x_tbl(meta(f, "x_tbl"), type) &&
    is_x_tbl_meta(meta(f, "x_tbl"), type)
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
