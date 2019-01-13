new_d <- function(x, type = "infin", ...) {
  distr_impl(
    fun_class = "d",
    impl_funs = list(fin = new_d_fin, infin = new_d_infin),
    x = x, type = type, ...
  )
}

new_d_fin <- function(x_tbl) {
  support <- range(x_tbl[["x"]])

  res <- function(x) {
    x_ind <- match(round(x, digits = 8), x_tbl[["x"]], nomatch = NA)

    ifelse(is.na(x_ind), 0, x_tbl[["prob"]][x_ind])
  }

  add_meta(res, support = support, x_tbl = x_tbl)
}

new_d_infin <- function(x_tbl) {
  # Using custom `approx_lin()` instead of `stats::approxfun()` to avoid
  # creating copies of `x_tbl[["x"]]` and `x_tbl[["y"]]`. It is slower but at
  # acceptable level.
  res <- approx_lin(x_tbl[["x"]], x_tbl[["y"]])

  # A better solution which doesn't pass R CMD CHECK:
  # res <- function(v) {
  #   stats:::.approxfun(
  #     x = x_tbl[["x"]], y = x_tbl[["y"]], v = v,
  #     method = 1, yleft = 0, yright = 0, f = 0
  #   )
  # }

  add_meta(res, support = range(x_tbl[["x"]]), x_tbl = x_tbl)
}

print.d <- function(x, ...) {
  pdqr_print(x, "Density")
}
