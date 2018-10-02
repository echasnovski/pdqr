d_fun <- function(x, type = "smooth", attach_sample = FALSE, extra = NULL,
                  ...) {
  assert_common_args(x, type, attach_sample)

  fun <- switch(
    type,
    raw = d_fun_raw(x),
    smooth = d_fun_smooth(x, ...)
  )

  res <- add_common_meta(
    fun, sample = x, type = type, attach_sample = attach_sample, extra = extra
  )

  structure(res, class = c("d_fun", "function"))
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
  meta_names <- glue::glue_collapse(names(meta(x)), sep = ", ")

  cat(glue_null(
    'Density function based on {meta(x, "type")} input\n',
    'Meta data has following elements: {meta_names}'
  ))
  cat("\n")
  attributes(x) <- NULL

  print(x, ...)
}
