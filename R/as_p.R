as_p <- function(f, ...) {
  UseMethod("as_p")
}

as_p.default <- function(f, support = NULL, n_grid = 10001, ...) {
  assert_as_def_args(f, support, n_grid)

  # Format support as vector with length two where `NA` indicates value to be
  # detected
  supp <- format_support(support)

  # Detect support
  support <- detect_support_p(f, supp)

  # Compute `y`
  x <- seq(support[1], support[2], length.out = n_grid)
  p <- f(x, ...)

  y <- y_from_p_grid(x, p)

  assert_tot_prob(sum(y, na.rm = TRUE))

  # This doesn't change output computation results and is needed for correct
  # approximation of q-function in case `as_q()` is called
  x_tbl <- remove_zero_edge_y(data.frame(x = x, y = y))

  res <- new_p(x_tbl, "smooth")

  # Make detected support more precise: if initial edge was `NA` then use the
  # one stored in `x_tbl` (which is more precise than in `support` because of
  # removing edge zero `y`)
  supp <- coalesce_pair(supp, range(x_tbl[["x"]]))

  # Ensure that output has maximum available support (usually equal to
  # `support`)
  ensure_support(res, supp)
}

as_p.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  res <- new_p(x = meta(f, "x_tbl"), type = meta(f, "type"))

  # Ensure that output has maximum available support (usually equal to
  # `meta(f, "support")`)
  ensure_support(res, meta(f, "support"))
}

detect_support_p <- function(p_f, supp) {
  if (is.na(supp[1])) {
    supp[1] <- solve_for_quan(p_f, 1e-8)
  }
  if (is.na(supp[2])) {
    supp[2] <- solve_for_quan(p_f, 1 - 1e-8)
  }

  if (!is_support(supp)) {
    stop_collapse(
      "Detected support isn't proper. Usually this is because supplied edge ",
      "is not compatible with actual support (left too high or right too low)"
    )
  }

  supp
}

solve_for_quan <- function(p_f, quan) {
  tryCatch(
    # Solve equation on interval (-10^100; 10^100)
    stats::uniroot(
      function(q) {p_f(q) - quan}, 1e100 * c(-1, 1)
    )[["root"]],
    error = function(e) {
      stop_collapse("Can't find quantile ", quan, " during support detection.")
    }
  )
}
