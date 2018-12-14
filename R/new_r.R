new_r <- function(x, type = "smooth", attach_x = identical(type, "raw"),
                  extra = NULL, ...) {
  distr_impl(
    fun_class = "r",
    impl_funs = list(raw = new_r_raw, smooth = new_r_smooth),
    x = x, type = type, attach_x = attach_x, extra = extra, ...
  )
}

new_r_raw <- function(x) {
  q_raw <- new_q(x, type = "raw", attach_x = FALSE)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_raw(rand_q_vec)
  }

  add_meta(res, support = meta(q_raw, "support"))
}

new_r_smooth <- function(x, ...) {
  q_smooth <- new_q(x, type = "smooth", attach_x = FALSE, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_smooth(rand_q_vec)
  }

  add_meta(res, support = meta(q_smooth, "support"))
}

print.r <- function(x, ...) {
  pdqr_print(x, "Random generation")
}
