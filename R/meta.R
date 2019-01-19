meta <- function(f) {
  check_f_envir(f)

  meta_names <- c("support", "type", "x_tbl")

  # Usage of `get0()` ensures that `NULL` is returned if (for some reason) an
  # object isn't found in environement
  res <- lapply(meta_names, get0, envir = environment(f), inherits = FALSE)
  names(res) <- meta_names

  res
}

has_meta <- function(f, elem) {
  !is.null(meta(f)[[elem]])
}

pdqr_type <- function(f) {
  check_f_envir(f)

  get0("type", envir = environment(f), inherits = FALSE)
}

pdqr_support <- function(f) {
  check_f_envir(f)

  get0("support", envir = environment(f), inherits = FALSE)
}

pdqr_x_tbl <- function(f) {
  check_f_envir(f)

  get0("x_tbl", envir = environment(f), inherits = FALSE)
}

check_f_envir <- function(f) {
  if (is.null(environment(f))) {
    stop_collapse("`f` should have enclosing environment.")
  }

  TRUE
}
