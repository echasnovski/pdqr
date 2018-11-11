p_fun <- function(x, type = "smooth", attach_x = TRUE, extra = NULL, ...) {
  distr_impl(
    fun_class = "p_fun",
    impl_funs = list(raw = p_fun_raw, smooth = p_fun_smooth),
    x = x, type = type, attach_x = attach_x, extra = extra, ...
  )
}

p_fun_raw <- function(x) {
  distr <- distr_tbl(x)
  distr_cum_prob <- c(0, cumsum(distr[["prob"]]))
  support <- range(x)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(q) {
    q_ind <- findInterval(round(q, digits = 8), distr[["x"]]) + 1

    distr_cum_prob[q_ind]
  }

  add_meta(res, support = support)
}

p_fun_smooth <- function(x, ...) {
  dens <- density_ext(x, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  x_dens <- dens[["x"]]
  y_dens <- dens[["y"]]
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

  add_meta(res, support = range(dens[["x"]]))
}

print.p_fun <- function(x, ...) {
  pdqr_print(x, "Cumulative distribution")
}
