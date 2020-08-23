#' @rdname as-pdqr
#' @export
as_q <- function(f, ...) {
  UseMethod("as_q")
}

#' @rdname as-pdqr
#' @export
as_q.default <- function(f, support = NULL, ..., n_grid = 10001) {
  assert_as_def_args(f, support, n_grid)

  # Attempt to detect "honored" distribution
  f_name <- deparse(substitute(f))
  honored_distr <- as_honored_distr(
    "q", f_name, f, support, ..., n_grid = n_grid
  )
  if (!is.null(honored_distr)) {
    return(honored_distr)
  }

  # Treate `f` as unknown q-function
  q_f <- function(p) {
    f(p, ...)
  }

  # Detect support
  support <- detect_support_q(q_f, format_support(support))

  p_f <- inversing(q_f, c(0, 1), n_grid = n_grid)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  as_q(as_p(p_f, support, n_grid = n_grid))
}

#' @rdname as-pdqr
#' @export
as_q.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  new_q(x = meta_x_tbl(f), type = meta_type(f))
}

detect_support_q <- function(q_f, supp) {
  if (is.na(supp[1])) {
    q_f_0 <- q_f(0)
    if (is.finite(q_f_0)) {
      supp[1] <- q_f_0
    } else {
      supp[1] <- q_f(1e-6)
    }
  }

  if (is.na(supp[2])) {
    q_f_1 <- q_f(1)
    if (is.finite(q_f_1)) {
      supp[2] <- q_f_1
    } else {
      supp[2] <- q_f(1 - 1e-6)
    }
  }

  if (!is_support(supp) || (supp[1] == supp[2])) {
    stop_collapse(
      "Detected support isn't proper. Usually this is because input quantile ",
      "function isn't proper."
    )
  }

  supp
}
