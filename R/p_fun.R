p_fun <- function(x, type = "smooth", attach_x = identical(type, "raw"),
                  extra = NULL, ...) {
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
  dens <- density_piecelin(x, ...)
  support <- range(dens[["x"]])

  res <- p_from_d_points(dens[["x"]], dens[["y"]])

  # For efficient memory management
  rm(list = c("x", "dens"), envir = environment())

  add_meta(res, support = support)
}

print.p_fun <- function(x, ...) {
  pdqr_print(x, "Cumulative distribution")
}
