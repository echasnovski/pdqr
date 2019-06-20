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
    # Pretty printing of pdqr-functions inside tibble list-columns. Not
    # exporting because it would mean listing 'pillar' in 'Imports' field of
    # 'DESCRIPTION', which is unnecessary with this trick.
  if (requireNamespace("pillar", quietly = TRUE)) {
    register_s3_method("pillar", "type_sum", "pdqr", fun = type_sum.pdqr)
  }

    # Methods for ordering list of pdqr-functions. Not exporting because it is
    # for internal use only.
  register_s3_method("base", "[",  "pdqr_list", fun = `[.pdqr_list`)
  register_s3_method("base", ">",  "pdqr_list", fun = `>.pdqr_list`)
  register_s3_method("base", "==", "pdqr_list", fun = `==.pdqr_list`)

  invisible()
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
