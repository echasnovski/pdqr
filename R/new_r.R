#' @rdname new-pdqr
#' @export
new_r <- function(x, type, ...) {
  distr_impl(
    pdqr_class = "r",
    impl_funs = list(fin = new_r_fin, infin = new_r_infin),
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

new_r_infin <- function(x_tbl) {
  q_infin <- new_q(x_tbl, type = "infin")

  type <- "infin"
  support <- meta_support(q_infin)
  x_tbl <- meta_x_tbl(q_infin)

  function(n) {
    # Not using `assert_type()` for speed reasons
    if (!(is_single_number(n, min_val = 0) || (length(n) == 0))) {
      stop_collapse(
        "`n` must be 'single non-negative number', not '", get_type(n), "'."
      )
    }

    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_infin(rand_q_vec)
  }
}

#' @export
print.r <- function(x, ...) {
  pdqr_print(x, "Random generation")
}
