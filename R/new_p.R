#' @rdname new-pdqr
#' @export
new_p <- function(x, type, ...) {
  distr_impl(
    pdqr_class = "p",
    impl_funs = list(discrete = new_p_dis, continuous = new_p_con),
    x = x, type = type, ...
  )
}

new_p_dis <- function(x_tbl) {
  type <- "discrete"
  support <- range(x_tbl[["x"]])

  function(q) {
    # Not using `assert_type()` for speed reasons
    if (!is.numeric(q)) {
      stop_collapse("`q` must be 'numeric', not '", get_type(q), "'.")
    }

    res <- numeric(length(q))

    q_not_na <- !is.na(q)
    res[!q_not_na] <- NA
    q <- q[q_not_na]

    q_ind <- findInterval(round(q, digits = 10), x_tbl[["x"]])
    # Among `q_ind` there might be `0`s, in which case `[q_ind]` subset returns
    # vector for only non-zero elements. That is why `[q_ind != 0]` is needed.
    res[q_not_na][q_ind != 0] <- x_tbl[["cumprob"]][q_ind]

    res
  }
}

new_p_con <- function(x_tbl) {
  type <- "continuous"
  support <- range(x_tbl[["x"]])

  function(q) {
    # Not using `assert_type()` for speed reasons
    if (!is.numeric(q)) {
      stop_collapse("`q` must be 'numeric', not '", get_type(q), "'.")
    }

    res <- numeric(length(q))

    q_not_na <- !is.na(q)
    res[!q_not_na] <- NA
    q <- q[q_not_na]

    x <- x_tbl[["x"]]
    y <- x_tbl[["y"]]
    p_grid <- x_tbl[["cumprob"]]

    q_ind <- findInterval(q, x)

    is_q_small <- q_ind == 0
    res[q_not_na][is_q_small] <- 0
    is_q_large <- q_ind == length(x)
    res[q_not_na][is_q_large] <- 1

    is_q_between <- !(is_q_small | is_q_large)
    q_ind_bet <- q_ind[is_q_between]
    q_bet <- q[is_q_between]
    x_bet <- x[q_ind_bet]

    # Exact integration of density linear interpolation
    coeffs <- compute_piecelin_density_coeffs(x_tbl, q_ind_bet)

    res_between <- p_grid[q_ind_bet] +
      # Using (q+x)*(q-x) instead of (q^2-x^2) helps to deal with very small
      # `q-x` values in case of dirac-like functions
      0.5 * coeffs[["slope"]] * (q_bet + x_bet) * (q_bet - x_bet) +
      coeffs[["intercept"]] * (q_bet - x_bet)
    # Extra cutoffs to respect floating point precision (~10^(-15))
    res[q_not_na][is_q_between] <- pmin(pmax(res_between, 0), 1)

    res
  }
}

#' @rdname methods-print
#' @include print.R
#' @export
print.p <- function(x, ...) {
  pdqr_print(x, "Cumulative distribution")
}
