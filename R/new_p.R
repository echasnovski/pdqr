new_p <- function(x, type = "smooth", ...) {
  distr_impl(
    fun_class = "p",
    impl_funs = list(raw = new_p_raw, smooth = new_p_smooth),
    x = x, type = type, ...
  )
}

new_p_raw <- function(x_tbl) {
  distr_cum_prob <- c(0, cumsum(x_tbl[["prob"]]))
  support <- range(x_tbl[["x"]])

  res <- function(q) {
    q_ind <- findInterval(round(q, digits = 8), x_tbl[["x"]]) + 1

    distr_cum_prob[q_ind]
  }

  add_meta(res, support = support, x_tbl = x_tbl)
}

new_p_smooth <- function(x_tbl) {
  support <- range(x_tbl[["x"]])

  res <- p_from_d_points(x_tbl[["x"]], x_tbl[["y"]])

  add_meta(res, support = support, x_tbl = x_tbl)
}

print.p <- function(x, ...) {
  pdqr_print(x, "Cumulative distribution")
}
