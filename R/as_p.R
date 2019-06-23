#' @rdname as-pdqr
#' @export
as_p <- function(f, ...) {
  UseMethod("as_p")
}

#' @rdname as-pdqr
#' @export
as_p.default <- function(f, support = NULL, ..., n_grid = 10001) {
  assert_as_def_args(f, support, n_grid)

  # Attempt to detect "honored" distribution
  f_name <- deparse(substitute(f))
  honored_distr <- as_honored_distr(
    "p", f_name, f, support, ..., n_grid = n_grid
  )
  if (!is.null(honored_distr)) {
    return(honored_distr)
  }

  # Treate `f` as unknown p-function
  p_f <- function(q) {f(q, ...)}

  # Format support as vector with length two where `NA` indicates value to be
  # detected
  supp <- format_support(support)

  # Detect support
  support <- detect_support_p(p_f, supp)

  # Compute `y`
  x <- seq_between(support, length.out = n_grid)
  p <- p_f(x)

  # Adjust p grid to support ("cut-and-normalize" method)
  p_norm <- (p - p[1]) / (p[length(p)] - p[1])

  y <- y_from_p_grid(x, p_norm)

  assert_tot_prob(sum(y, na.rm = TRUE))

  # This doesn't change output computation results and is needed for correct
  # approximation of q-function in case `as_q()` is called
  x_tbl <- remove_zero_edge_y(data.frame(x = x, y = y))

  new_p(x_tbl, "continuous")
}

#' @rdname as-pdqr
#' @export
as_p.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  new_p(x = meta_x_tbl(f), type = meta_type(f))
}

detect_support_p <- function(p_f, supp) {
  if (is.na(supp[1])) {
    supp[1] <- solve_for_quan(p_f, 1e-6)
  }
  if (is.na(supp[2])) {
    supp[2] <- solve_for_quan(p_f, 1 - 1e-6)
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
