#nocov start
.onLoad <- function(libname, pkgname) {
  # Set default options
  default_options <- list(
    pdqr.group_gen.n_sample = 10000,
    pdqr.group_gen.args_new = list(),
    pdqr.group_gen.repair_supp_method = "reflect"
  )
  op <- options()
  toset <- !(names(default_options) %in% names(op))
  if (any(toset)) {
    options(default_options[toset])
  }

  # Register external methods
  if (requireNamespace("pillar", quietly = TRUE)) {
    register_s3_method("pillar", "type_sum", "pdqr", fun = type_sum.pdqr)
  }

  invisible()
}

# Pretty printing of pdqr-functions as elements of list-columns in tibble
# (tidyverse package).
# Only one method for "pdqr" class is implemented (instead of four more quick
# methods for every class separately) because of possible collision with classes
# "p", "d", "q", "r" from some other package. This implmementation has not very
# big overhead (around 4 microseconds).
type_sum.pdqr <- function(x) {
  paste0(meta_class(x), "-fun")
}

# Adapted from github.com/tidyverse/googledrive ('dplyr-compat.R')
register_s3_method <- function(pkg, generic, class, fun) {
  envir <- asNamespace(pkg)

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
}
#nocov end
