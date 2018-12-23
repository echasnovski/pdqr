new_p <- function(x, type = "smooth", ...) {
  distr_impl(
    fun_class = "p",
    impl_funs = list(raw = new_p_raw, smooth = new_p_smooth),
    x = x, type = type, ...
  )
}

new_p_raw <- function(x) {
  raw_tbl <- compute_raw_tbl(x)
  distr_cum_prob <- c(0, cumsum(raw_tbl[["prob"]]))
  support <- range(x)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(q) {
    q_ind <- findInterval(round(q, digits = 8), raw_tbl[["x"]]) + 1

    distr_cum_prob[q_ind]
  }

  add_meta(res, support = support, raw_tbl = raw_tbl)
}

new_p_smooth <- function(x, ...) {
  smooth_tbl <- density_piecelin(x, ...)
  support <- range(smooth_tbl[["x"]])

  # For efficient memory management
  rm(list = c("x"), envir = environment())

  res <- p_from_d_points(smooth_tbl[["x"]], smooth_tbl[["y"]])

  add_meta(res, support = support, smooth_tbl = smooth_tbl)
}

print.p <- function(x, ...) {
  pdqr_print(x, "Cumulative distribution")
}
