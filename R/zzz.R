.onLoad <- function(libname, pkgname) {
  #nocov start
  default_options <- list(pdqr.transform.n_sample = 10000)
  op <- options()
  toset <- !(names(default_options) %in% names(op))
  if (any(toset)) {
    options(default_options[toset])
  }

  invisible()
  #nocov end
}
