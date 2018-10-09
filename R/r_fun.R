r_fun <- function(x, type = "smooth", attach_x = FALSE, extra = NULL, ...) {
  distr_impl(
    fun_class = "r_fun",
    impl_funs = list(raw = r_fun_raw, smooth = r_fun_smooth),
    x = x, type = type, attach_x = attach_x, extra = extra, ...
  )
}

r_fun_raw <- function(x) {
  q_raw <- q_fun(x, type = "raw", attach_x = FALSE)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_raw(rand_q_vec)
  }

  add_meta(
    res,
    distr_tbl = meta(q_raw, "distr_tbl"),
    domain_out = meta(q_raw, "domain_out")
  )
}

r_fun_smooth <- function(x, ...) {
  q_smooth <- q_fun(x, type = "smooth", attach_x = FALSE, ...)

  # For efficient memory management
  rm(list = "x", envir = environment())

  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_smooth(rand_q_vec)
  }

  add_meta(res, domain_out = meta(q_smooth, "domain_out"))
}

print.r_fun <- function(x, ...) {
  distr_print("Random generation function", x, ...)
}
