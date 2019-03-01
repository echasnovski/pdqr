.onLoad <- function(libname, pkgname) {
  #nocov start
  default_options <- list(
    pdqr.group_gen.n_sample = 10000,
    pdqr.group_gen.args_new = list()
  )
  op <- options()
  toset <- !(names(default_options) %in% names(op))
  if (any(toset)) {
    options(default_options[toset])
  }

  invisible()
  #nocov end
}
