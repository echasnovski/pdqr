#' @rdname as-pdqr
#' @include as_p.R
#' @export
as_d <- function(f, ...) {
  UseMethod("as_d")
}

#' @rdname as-pdqr
#' @export
as_d.default <- function(f, support = NULL, ..., n_grid = 10001) {
  assert_as_def_args(f, support, n_grid)

  # Attempt to detect "honored" distribution
  f_name <- deparse(substitute(f))
  honored_distr <- as_honored_distr(
    "d", f_name, f, support, ..., n_grid = n_grid
  )
  if (!is.null(honored_distr)) {
    return(honored_distr)
  }

  # Treate `f` as unknown d-function
  d_f <- function(x) {f(x, ...)}

  # Format support as vector with length two where `NA` indicates value to be
  # detected
  supp <- format_support(support)

  # Detect support
  support <- detect_support_d(d_f, supp)

  # Compute `y`
  x <- seq_between(support, length.out = n_grid)
  y <- d_f(x)
  y <- impute_inf(x, y, '`f` output')

  assert_tot_prob(sum(y, na.rm = TRUE))

  # This doesn't change output computation results and is needed for correct
  # approximation of q-function in case `as_q()` is called
  x_tbl <- remove_zero_edge_y(data.frame(x = x, y = y))

  new_d(x_tbl, "continuous")
}

#' @rdname as-pdqr
#' @export
as_d.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  new_d(x = meta_x_tbl(f), type = meta_type(f))
}

detect_support_d <- function(d_f, supp) {
  is_supp_na <- is.na(supp)
  if (!any(is_supp_na)) {
    return(supp)
  }

  # Initial values of `x` (the smallest and the biggest one could find)
  init_x <- detect_d_init_x(d_f)

  # Construct p-function from `d_f`
  p_f <- construct_p_f(d_f, init_x[1])

  if (is_supp_na[1]) {
    supp[1] <- optim_for_quan(p_f, 1e-6, init_x[1])
  }
  if (is_supp_na[2]) {
    supp[2] <- optim_for_quan(p_f, 1 - 1e-6, init_x[2])
  }

  if (!is_support(supp) || is_near(supp[1], supp[2])) {
    stop_collapse(
      "Detected support isn't proper. Usually this is because supplied edge ",
      "is not compatible with actual support (left too high or right too low) ",
      "or input density has particularly hard to detect support."
    )
  }

  supp
}

construct_p_f <- function(d_f, init) {
  # p-function is not with `stats::integrate(d_f, -Inf, q)` because it will have
  # worse numerical accuracy
  init_p <- tryCatch(
    stats::integrate(d_f, -Inf, init)[["value"]],
    error = function(e) {
      stop_collapse("Can't find initial point in `as_d.default()`.")
    }
  )

  function(q) {
    init_p +
      vapply(q, function(t) {integrate_safely(d_f, init, t)}, numeric(1))
  }
}

optim_for_quan <- function(p_f, quan, init) {
  tryCatch(
    stats::optim(
      par = init, fn = function(q) {abs(p_f(q) - quan)},
      method = "Nelder-Mead",
      control = list(warn.1d.NelderMead = FALSE)
    )[["par"]][1],
    error = function(e) {
      stop_collapse(
        "Can't optimize for quantile ", quan, " during support detection."
      )
    }
  )
}

detect_d_init_x <- function(d_f) {
  # Reproducible sequence of "try" points
  powers <- seq(from = -5, to = 7, length.out = 5000)
  modules <- 10^powers
    # Using `rev()` to ensure that `try_points` is ordered increasingly, which
    # is important to return the smallest and the biggest points with good
    # values of `d_f`
  try_points <- c(-rev(modules), 0, modules)

  # Values of `d_f` at "try" points
  d_vals <- d_f(try_points)

  # Data about good values of `d_f` at "try" points
  d_good_vals_inds <- which(is.finite(d_vals) & (d_vals > 0))
  n_d_good_vals <- length(d_good_vals_inds)
  if (n_d_good_vals == 0) {
    stop_collapse("Can't find initial point in `as_d.default()`.")
  }

  # Take the smallest and the biggest points with good values of `d_f`
  init_x_inds <- d_good_vals_inds[c(1, n_d_good_vals)]

  try_points[init_x_inds]
}
