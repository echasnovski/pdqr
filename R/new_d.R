new_d <- function(x, type = "smooth", ...) {
  distr_impl(
    fun_class = "d",
    impl_funs = list(raw = new_d_raw, smooth = new_d_smooth),
    x = x, type = type, ...
  )
}

new_d_raw <- function(x) {
  raw_tbl <- compute_raw_tbl(x)
  support <- range(x)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(x) {
    x_ind <- match(round(x, digits = 8), raw_tbl[["x"]], nomatch = NA)

    ifelse(is.na(x_ind), 0, raw_tbl[["prob"]][x_ind])
  }

  add_meta(res, support = support, raw_tbl = raw_tbl)
}

new_d_smooth <- function(x, ...) {
  smooth_tbl <- density_piecelin(x, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- stats::approxfun(
    x = smooth_tbl[["x"]], y = smooth_tbl[["y"]], method = "linear",
    yleft = 0, yright = 0, rule = 2
  )

  add_meta(res, support = range(smooth_tbl[["x"]]), smooth_tbl = smooth_tbl)
}

print.d <- function(x, ...) {
  pdqr_print(x, "Density")
}
