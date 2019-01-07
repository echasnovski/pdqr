as_q <- function(f, ...) {
  UseMethod("as_q")
}

as_q.default <- function(f, support = NULL, n_grid = 10001, ...) {
  assert_as_def_args(f, support, n_grid)

  # Detect support
  support <- detect_support_q(f, format_support(support))

  p_f <- inversing(f, c(0, 1), n_grid = n_grid, ...)

  as_q(as_p(p_f, support, n_grid))
}

as_q.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  res <- new_q(x = meta(f, "x_tbl"), type = meta(f, "type"))

  # Ensure that output has maximum available support (usually equal to
  # `meta(f, "support")`)
  ensure_support(res, meta(f, "support"))
}

detect_support_q <- function(q_f, supp) {
  if (is.na(supp[1])) {
    q_f_0 <- q_f(0)
    if (is.finite(q_f_0)) {
      supp[1] <- q_f_0
    } else {
      supp[1] <- q_f(1e-8)
    }
  }

  if (is.na(supp[2])) {
    q_f_1 <- q_f(1)
    if (is.finite(q_f_1)) {
      supp[2] <- q_f_1
    } else {
      supp[2] <- q_f(1 - 1e-8)
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
