new_r <- function(x, type = "infin", ...) {
  distr_impl(
    fun_class = "r",
    impl_funs = list(fin = new_r_fin, infin = new_r_infin),
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
    res, support = pdqr_support(q_fin), x_tbl = meta(q_fin, "x_tbl")
  )
}

new_r_infin <- function(x_tbl) {
  q_infin <- new_q(x_tbl, type = "infin")

  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_infin(rand_q_vec)
  }

  add_meta(
    res, support = pdqr_support(q_infin), x_tbl = meta(q_infin, "x_tbl")
  )
}

print.r <- function(x, ...) {
  pdqr_print(x, "Random generation")
}
