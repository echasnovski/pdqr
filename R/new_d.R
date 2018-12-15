new_d <- function(x, type = "smooth", attach_x = identical(type, "raw"), ...) {
  distr_impl(
    fun_class = "d",
    impl_funs = list(raw = new_d_raw, smooth = new_d_smooth),
    x = x, type = type, attach_x = attach_x, ...
  )
}

new_d_raw <- function(x) {
  distr <- vec_summ_distr_tbl(x)
  support <- range(x)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(x) {
    x_ind <- match(round(x, digits = 8), distr[["x"]], nomatch = NA)

    ifelse(is.na(x_ind), 0, distr[["prob"]][x_ind])
  }

  add_meta(res, support = support)
}

new_d_smooth <- function(x, ...) {
  dens <- density_piecelin(x, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- stats::approxfun(
    x = dens[["x"]], y = dens[["y"]], method = "linear",
    yleft = 0, yright = 0, rule = 2
  )

  add_meta(res, support = range(dens[["x"]]))
}

print.d <- function(x, ...) {
  pdqr_print(x, "Density")
}
