new_r <- function(x, type = "smooth", ...) {
  distr_impl(
    fun_class = "r",
    impl_funs = list(fin = new_r_fin, smooth = new_r_smooth),
    x = x, type = type, ...
  )
}

new_r_fin <- function(x_tbl) {
  q_fin <- new_q(x_tbl, type = "fin")

  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_fin(rand_q_vec)
  }

  add_meta(
    res, support = meta(q_fin, "support"), x_tbl = meta(q_fin, "x_tbl")
  )
}

new_r_smooth <- function(x_tbl) {
  q_smooth <- new_q(x_tbl, type = "smooth")

  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_smooth(rand_q_vec)
  }

  add_meta(
    res, support = meta(q_smooth, "support"), x_tbl = meta(q_smooth, "x_tbl")
  )
}

print.r <- function(x, ...) {
  pdqr_print(x, "Random generation")
}
