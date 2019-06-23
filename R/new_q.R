#' @rdname new-pdqr
#' @export
new_q <- function(x, type, ...) {
  distr_impl(
    pdqr_class = "q",
    impl_funs = list(discrete = new_q_dis, continuous = new_q_con),
    x = x, type = type, ...
  )
}

new_q_dis <- function(x_tbl) {
  type <- "discrete"
  support <- range(x_tbl[["x"]])

  function(p) {
    # Not using `assert_type()` for speed reasons
    if (!is.numeric(p)) {
      stop_collapse("`p` must be 'numeric', not '", get_type(p), "'.")
    }

    res <- numeric(length(p))

    p_not_na <- !is.na(p)
    res[!p_not_na] <- NA
    p <- p[p_not_na]

    is_prob <- (p >= 0) & (p <= 1)
    p_prob <- p[is_prob]
    p_ind <- findInterval(p_prob, x_tbl[["cumprob"]], left.open = TRUE) + 1

    res[p_not_na][is_prob] <- x_tbl[["x"]][p_ind]
    res[p_not_na][!is_prob] <- NaN

    res
  }
}

new_q_con <- function(x_tbl) {
  type <- "continuous"
  support <- range(x_tbl[["x"]])

  function(p) {
    # Not using `assert_type()` for speed reasons
    if (!is.numeric(p)) {
      stop_collapse("`p` must be 'numeric', not '", get_type(p), "'.")
    }

    res <- numeric(length(p))

    p_not_na <- !is.na(p)
    res[!p_not_na] <- NA
    p <- p[p_not_na]

    x <- x_tbl[["x"]]
    y <- x_tbl[["y"]]
    p_grid <- x_tbl[["cumprob"]]

    is_inside <- (p >= 0) & (p <= 1)
    p_prob <- p[is_inside]

    # `left.open = TRUE` and `all.inside = TRUE` ensure that index of interval
    # with **the smallest** left end is returned. This is needed to ensure
    # definition of quantile function: "the smallest value with cumulative
    # probability not exceeding given one".
    p_ind <- findInterval(p_prob, p_grid, left.open = TRUE, all.inside = TRUE)

    coeffs <- compute_piecelin_density_coeffs(x_tbl, p_ind)

    res[p_not_na][is_inside] <- find_quant(
      p = p_prob,
      cdf_start = p_grid[p_ind],
      x_start = x[p_ind],
      slope = coeffs[["slope"]],
      intercept = coeffs[["intercept"]]
    )

    res[p_not_na][(p < 0) | (p > 1)] <- NaN

    res
  }
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
  # Theoretically, `discr` should always be >= 0. However, due to numerical
  # inaccuracies of magnitude ~10^(-15), here call to `pmax()` is needed.
  discr <- pmax(b * b - 4 * a * c, 0)
  res[is_quadr] <- (-b + sqrt(discr)) / (2 * a)

  # Case of linear CDF curve (density is non-zero constant)
  res[is_lin] <- x_start[is_lin] +
    (p[is_lin] - cdf_start[is_lin]) / intercept[is_lin]

  # Case of plateau in CDF (density equals zero)
  res[is_const] <- x_start[is_const]

  res
}

#' @rdname methods-print
#' @export
print.q <- function(x, ...) {
  pdqr_print(x, "Quantile")
}
