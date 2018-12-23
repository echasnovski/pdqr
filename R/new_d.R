new_d <- function(x, type = "smooth", ...) {
  distr_impl(
    fun_class = "d",
    impl_funs = list(raw = new_d_raw, smooth = new_d_smooth),
    x = x, type = type, ...
  )
}

new_d_raw <- function(x) {
  x_tbl <- compute_x_tbl(x, "raw")
  support <- range(x)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(x) {
    x_ind <- match(round(x, digits = 8), x_tbl[["x"]], nomatch = NA)

    ifelse(is.na(x_ind), 0, x_tbl[["prob"]][x_ind])
  }

  add_meta(res, support = support, x_tbl = x_tbl)
}

new_d_smooth <- function(x, ...) {
  x_tbl <- compute_x_tbl(x, "smooth", ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- stats::approxfun(
    x = x_tbl[["x"]], y = x_tbl[["y"]], method = "linear",
    yleft = 0, yright = 0, rule = 2
  )

  add_meta(res, support = range(x_tbl[["x"]]), x_tbl = x_tbl)
}

print.d <- function(x, ...) {
  pdqr_print(x, "Density")
}
