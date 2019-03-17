#' Get metadata of pdqr-functions
#'
#' @param f A "proper" pdqr-function.
#'
#' @return `meta_all()` returns a list of all metadata. `meta_type()`,
#'   `meta_support`, and `meta_x_tbl()` return corresponding metadata.
#'
#' @examples
#' d_unif <- as_d(dunif)
#'
#' meta_all(d_unif)
#' meta_type(d_unif)
#' meta_support(d_unif)
#' head(meta_x_tbl(d_unif))
#'
#' @name meta
NULL

#' @rdname meta
#' @export
meta_all <- function(f) {
  check_f_envir(f)

  meta_names <- c("type", "support", "x_tbl")

  # Usage of `get0()` ensures that `NULL` is returned if (for some reason) an
  # object isn't found in environement
  res <- lapply(meta_names, get0, envir = environment(f), inherits = FALSE)
  names(res) <- meta_names

  res
}

#' @rdname meta
#' @export
meta_type <- function(f) {
  check_f_envir(f)

  get0("type", envir = environment(f), inherits = FALSE)
}

#' @rdname meta
#' @export
meta_support <- function(f) {
  check_f_envir(f)

  get0("support", envir = environment(f), inherits = FALSE)
}

#' @rdname meta
#' @export
meta_x_tbl <- function(f) {
  check_f_envir(f)

  get0("x_tbl", envir = environment(f), inherits = FALSE)
}

has_meta <- function(f, elem) {
  !is.null(meta_all(f)[[elem]])
}

check_f_envir <- function(f) {
  f_env <- environment(f)

  if (is.null(f_env)) {
    stop_collapse("`f` should have enclosing environment.")
  }
  if (identical(f_env, globalenv())) {
    stop_collapse("`f` should not be created in Global environment.")
  }

  TRUE
}
