#' @rdname new-pdqr
#' @export
new_p <- function(x, type = "infin", ...) {
  distr_impl(
    fun_class = "p",
    impl_funs = list(fin = new_p_fin, infin = new_p_infin),
    x = x, type = type, ...
  )
}

new_p_fin <- function(x_tbl) {
  type <- "fin"
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

new_p_infin <- function(x_tbl) {
  type <- "infin"
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
      0.5 * coeffs[["slope"]] * (q_bet * q_bet - x_bet * x_bet) +
      coeffs[["intercept"]] * (q_bet - x_bet)
    # Extra cutoffs to respect floating point precision (~10^(-15))
    res[q_not_na][is_q_between] <- pmin(pmax(res_between, 0), 1)

    res
  }
}

print.p <- function(x, ...) {
  pdqr_print(x, "Cumulative distribution")
}
