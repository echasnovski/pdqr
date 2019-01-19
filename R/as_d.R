as_d <- function(f, ...) {
  UseMethod("as_d")
}

as_d.default <- function(f, support = NULL, n_grid = 10001, ...) {
  assert_as_def_args(f, support, n_grid)

  d_f <- function(x) {f(x, ...)}

  # Format support as vector with length two where `NA` indicates value to be
  # detected
  supp <- format_support(support)

  # Detect support
  support <- detect_support_d(d_f, supp)

  # Compute `y`
  x <- seq(support[1], support[2], length.out = n_grid)
  y <- d_f(x)
  y <- impute_inf(x, y, '`f` output')

  assert_tot_prob(sum(y, na.rm = TRUE))

  # This doesn't change output computation results and is needed for correct
  # approximation of q-function in case `as_q()` is called
  x_tbl <- remove_zero_edge_y(data.frame(x = x, y = y))

  new_d(x_tbl, "infin")
}

as_d.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  new_d(x = pdqr_x_tbl(f), type = pdqr_type(f))
}

detect_support_d <- function(d_f, supp) {
  is_supp_na <- is.na(supp)
  if (!any(is_supp_na)) {
    return(supp)
  }

  # Initial value
  init_x <- detect_d_init_x(d_f)

  # Construct p-function from `d_f`
  p_f <- construct_p_f(d_f, init_x)

  if (is_supp_na[1]) {
    supp[1] <- optim_for_quan(p_f, 1e-6, init_x)
  }
  if (is_supp_na[2]) {
    supp[2] <- optim_for_quan(p_f, 1 - 1e-6, init_x)
  }

  if (!is_support(supp) || (supp[1] == supp[2])) {
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
    error = function(e) {stop_collapse("Can't find initial point.")}
  )

  function(q) {
    init_p +
      vapply(q, function(t) {integrate_safely(d_f, init, t)}, numeric(1))
  }
}

optim_for_quan <- function(p_f, quan, init) {
  tryCatch(
    # Solve optimization problem. It is formally two-dimensional in order ot use
    # method "Nelder-Mead" without warnings
    stats::optim(
      par = c(init, 0), fn = function(q) {abs(p_f(q[1]) - quan)},
      method = "Nelder-Mead", control = list(abstol = 1e-3)
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
  set.seed(1)
  smpl <- stats::rcauchy(10000, scale = 1000)
  d_max_ind <- which.max(d_f(smpl))

  smpl[d_max_ind]
}
