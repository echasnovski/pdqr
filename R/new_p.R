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
    res <- numeric(length(q))

    q_ind <- findInterval(q, x_tbl[["x"]])
    q_ind_isnt_zero <- q_ind != 0

    res[q_ind_isnt_zero] <- x_tbl[["cumprob"]][q_ind[q_ind_isnt_zero]]
    res[!q_ind_isnt_zero] <- 0

    res
  }
}

new_p_infin <- function(x_tbl) {
  type <- "infin"
  support <- range(x_tbl[["x"]])

  function(q) {
    x <- x_tbl[["x"]]
    y <- x_tbl[["y"]]
    p_grid <- x_tbl[["cumprob"]]

    out <- numeric(length(q))

    q_ind <- findInterval(q, x)

    is_q_small <- q_ind == 0
    out[is_q_small] <- 0
    is_q_large <- q_ind == length(x)
    out[is_q_large] <- 1

    is_q_between <- !(is_q_small | is_q_large)
    q_ind_bet <- q_ind[is_q_between]
    q_bet <- q[is_q_between]
    x_bet <- x[q_ind_bet]

    # Exact integration of density linear interpolation
    coeffs <- compute_piecelin_density_coeffs(x, y, q_ind_bet)

    out_between <- p_grid[q_ind_bet] +
      0.5 * coeffs[["slope"]] * (q_bet * q_bet - x_bet * x_bet) +
      coeffs[["intercept"]] * (q_bet - x_bet)
    # Extra cutoffs to respect floating point precision (~10^(-15))
    out[is_q_between] <- pmin(pmax(out_between, 0), 1)

    out
  }
}

print.p <- function(x, ...) {
  pdqr_print(x, "Cumulative distribution")
}
