#' @rdname new-pdqr
#' @export
new_r <- function(x, type, ...) {
  distr_impl(
    pdqr_class = "r",
    impl_funs = list(fin = new_r_fin, continuous = new_r_con),
    x = x, type = type, ...
  )
}

new_r_fin <- function(x_tbl) {
  q_fin <- new_q(x_tbl, type = "fin")

  type <- "fin"
  support <- meta_support(q_fin)
  x_tbl <- meta_x_tbl(q_fin)

  function(n) {
    # Not using `assert_type()` for speed reasons
    if (!(is_single_number(n, min_val = 0) || (length(n) == 0))) {
      stop_collapse(
        "`n` must be 'single non-negative number', not '", get_type(n), "'."
      )
    }

    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_fin(rand_q_vec)
  }
}

new_r_con <- function(x_tbl) {
  q_con <- new_q(x_tbl, type = "continuous")

  type <- "continuous"
  support <- meta_support(q_con)
  x_tbl <- meta_x_tbl(q_con)

  function(n) {
    # Not using `assert_type()` for speed reasons
    if (!(is_single_number(n, min_val = 0) || (length(n) == 0))) {
      stop_collapse(
        "`n` must be 'single non-negative number', not '", get_type(n), "'."
      )
    }

    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_con(rand_q_vec)
  }
}

#' @rdname methods-print
#' @export
print.r <- function(x, ...) {
  pdqr_print(x, "Random generation")
}
