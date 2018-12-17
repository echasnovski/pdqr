new_q <- function(x, type = "smooth", ...) {
  distr_impl(
    fun_class = "q",
    impl_funs = list(raw = new_q_raw, smooth = new_q_smooth),
    x = x, type = type, ...
  )
}

new_q_raw <- function(x) {
  raw_tbl <- compute_raw_tbl(x)
  distr_cum_prob_small <- cumsum(raw_tbl[["prob"]])
  support <- range(x)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(p) {
    out <- numeric(length(p))

    is_prob <- (p >= 0) & (p <= 1)
    p_prob <- round(p[is_prob], digits = 8)
    p_ind <- findInterval(p_prob, distr_cum_prob_small, left.open = TRUE) + 1

    out[is_prob] <- raw_tbl[["x"]][p_ind]
    out[!is_prob] <- NaN

    out
  }

  add_meta(res, support = support, raw_tbl = raw_tbl)
}

new_q_smooth <- function(x, ...) {
  dens <- density_piecelin(x, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  x_dens <- dens[["x"]]
  y_dens <- dens[["y"]]
  n <- length(x_dens)
  support <- range(x_dens)

  p_grid <- trapez_part_integral(x_dens, y_dens)[-n]
  slope_vec <- diff(y_dens) / diff(x_dens)
  inter_vec <- y_dens[-n] - slope_vec * x_dens[-n]

  res <- function(p) {
    out <- numeric(length(p))

    is_inside <- (p > 0) & (p < 1)
    p_prob <- p[is_inside]

    p_ind <- findInterval(p_prob, p_grid)

    out[is_inside] <- find_quant(
      p = p_prob,
      cdf_start = p_grid[p_ind],
      x_start = x_dens[p_ind],
      slope = slope_vec[p_ind],
      intercept = inter_vec[p_ind]
    )

    out[is_near(p, 0) & (p >= 0)] <- support[1]
    out[is_near(p, 1) & (p <= 1)] <- support[2]
    out[(p < 0) | (p > 1)] <- NaN

    out
  }

  add_meta(res, support = support)
}

find_quant <- function(p, cdf_start, x_start, slope, intercept) {
  res <- numeric(length(p))

  is_quadr <- !is_near(slope, 0)
  is_lin <- !(is_quadr | is_near(intercept, 0))
  is_const <- !(is_quadr | is_lin)

  # Case of quadratic CDF curve (density is a line not parallel to x axis)
  a <- 0.5 * slope[is_quadr]
  b <- intercept[is_quadr]
  c <- (cdf_start[is_quadr] - a * x_start[is_quadr] * x_start[is_quadr] -
          b * x_start[is_quadr] - p[is_quadr])
    # Equations have form of a * x^2 + b * x + c = 0
  discr <- b * b - 4 * a * c
  res[is_quadr] <- (-b + sqrt(discr)) / (2 * a)

  # Case of linear CDF curve (density is non-zero constant)
  res[is_lin] <- x_start[is_lin] +
    (p[is_lin] - cdf_start[is_lin]) / intercept[is_lin]

  # Case of plateau in CDF (density equals zero)
  res[is_const] <- x_start[is_const]

  res
}

print.q <- function(x, ...) {
  pdqr_print(x, "Quantile")
}
