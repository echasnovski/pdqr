new_p <- function(x, type = "smooth", ...) {
  distr_impl(
    fun_class = "p",
    impl_funs = list(raw = new_p_raw, smooth = new_p_smooth),
    x = x, type = type, ...
  )
}

new_p_raw <- function(x_tbl) {
  support <- range(x_tbl[["x"]])

  res <- function(q) {
    res <- numeric(length(q))

    q_ind <- findInterval(round(q, digits = 8), x_tbl[["x"]])
    q_ind_isnt_zero <- q_ind != 0

    res[q_ind_isnt_zero] <- x_tbl[["cumprob"]][q_ind[q_ind_isnt_zero]]
    res[!q_ind_isnt_zero] <- 0

    res
  }

  add_meta(res, support = support, x_tbl = x_tbl)
}

new_p_smooth <- function(x_tbl) {
  support <- range(x_tbl[["x"]])

  res <- p_from_d_points(
    x_tbl[["x"]], x_tbl[["y"]], cumprob = x_tbl[["cumprob"]]
  )

  add_meta(res, support = support, x_tbl = x_tbl)
}

print.p <- function(x, ...) {
  pdqr_print(x, "Cumulative distribution")
}
