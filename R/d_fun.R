d_fun <- function(x, type = "smooth", attach_sample = FALSE, extra = NULL,
                  ...) {
  distr_impl(
    fun_class = "d_fun",
    impl_funs = list(raw = d_fun_raw, smooth = d_fun_smooth),
    x = x, type = type, attach_sample = attach_sample, extra = extra, ...
  )
}

d_fun_raw <- function(x) {
  distr_tbl <- compute_distr_tbl(x)
  domain_in <- range(x)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(x) {
    x_ind <- match(round(x, digits = 8), distr_tbl[["x"]], nomatch = NA)

    ifelse(is.na(x_ind), 0, distr_tbl[["prob"]][x_ind])
  }

  add_meta(res, distr_tbl = distr_tbl, domain_in = domain_in)
}

d_fun_smooth <- function(x, ...) {
  dens <- density_ext(x, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- stats::approxfun(
    x = dens[["x"]], y = dens[["y"]], method = "linear",
    yleft = 0, yright = 0, rule = 2
  )

  add_meta(res, domain_in = range(dens[["x"]]))
}

print.d_fun <- function(x, ...) {
  distr_print("Density function", x, ...)
}
