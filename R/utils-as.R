# Documentation of `as_*()` -----------------------------------------------
#' Convert to pdqr-function
#'
#' Convert some function to be a proper pdqr-function of type "infin".
#'
#' @param f Appropriate function to be converted (see Details).
#' @param support Support of output. If `NULL` or any value is `NA`, it is
#'   detected using specific algorithms (see Details).
#' @param ... Extra arguments for `f`.
#' @param n_grid Number of grid points at which `f` will be evaluated (see
#'   Details).
#'
#' @return A pdqr-function of corresponding [class][meta_class()].
#'
#' @examples
#' # Convert existing "proper" pdqr-function
#' set.seed(101)
#' x <- rnorm(10)
#' my_d <- new_d(x, "infin")
#'
#' my_p <- as_p(my_d)
#'
#' # Convert "honored" function to be a "proper" pdqr-function
#' p_unif <- as_p(punif)
#' r_beta <- as_r(rbeta, shape1 = 2, shape2 = 2)
#' d_pois <- as_d(dpois, lambda = 5)
#'
#' # Convert some other function to be a "proper" pdqr-function
#' my_d_quadr <- as_d(function(x) {0.75*(1 - x^2)}, support = c(-1, 1))
#'
#' @name as-pdqr
NULL


# Honored distributions ---------------------------------------------------
as_honored_distr <- function(pdqr_class, f_name, f, support, ..., n_grid) {
  distr_info <- honored_distr_info(f_name, f)
  if (is.null(distr_info)) {
    return(NULL)
  }

  f_class <- distr_info[["class"]]
  if (pdqr_class != f_class) {
    warning_collapse(
      "You supplied a ", f_class, "-function `", f_name, "` to `as_",
      pdqr_class, "()`. ",
      "A ", pdqr_class, "-function will be created."
    )
  }

  distr_supp <- honored_distr_supp(
    distr = distr_info[["distr"]], q_fun = distr_info[["q_fun"]], ...
  )
  supp <- coalesce_pair(format_support(support), distr_supp)

  if (distr_info[["type"]] == "fin") {
    # This approach assumes that honored "fin" pdqr-functions have only integer
    # "x" values. If in future it is not true, some (serious) refactoring should
    # be made here.
    x_vec <- supp[1]:supp[2]
    x_tbl <- data.frame(x = x_vec, prob = distr_info[["d_fun"]](x_vec, ...))

    res <- new_d(x_tbl, type = "fin")
  } else {
    # This is a shallow recursion for `as_d.default()`. However, during this run
    # there will be no match for honored distribtuion (there shouldn't be any
    # name `distr_info[["d_fun"]]` as distribution function) and there will be
    # no support detection as `supp` shouldn't have any `NA`s.
    res <- as_d.default(
      f = distr_info[["d_fun"]], support = supp, ..., n_grid = n_grid
    )
  }

  as_pdqr_by_class(pdqr_class)(res)
}

honored_distr_info <- function(f_name, f) {
  # Allow call with `package::` prefix. Also using `[1]` indexing because input
  # `f_name` can be a vector in case of anonymous function like
  # `function(x) {dunif(x)}`.
  f_name <- sub("^.*::", "", f_name[1])
  f_env <- environment(f)
  f_package <- environmentName(f_env)

  is_honored <- (f_name == honored_distrs[["fun"]]) &
    (f_package == honored_distrs[["package"]])

  if (sum(is_honored) == 0) {
    return(NULL)
  }

  list(
    distr = honored_distrs[["distr"]][is_honored],
    type  = honored_distrs[["type"]][is_honored],
    class = honored_distrs[["class"]][is_honored],
    d_fun = get(honored_distrs[["d_fun"]][is_honored], envir = f_env),
    q_fun = get(honored_distrs[["q_fun"]][is_honored], envir = f_env)
  )
}

honored_distr_supp <- function(distr, q_fun, ..., p = 1e-6) {
  if (distr %in% c("binom", "hyper", "unif")) {
    distr_edge_quantiles <- c(0, 1)
  } else if (distr %in% c("geom", "exp")) {
    distr_edge_quantiles <- c(0, 1 - p)
  } else if (
    distr %in% c(
      # "fin"
      "nbinom", "pois",
      # "infin"
      "beta", "chisq", "f", "gamma", "lnorm", "norm", "t", "weibull"
    )
  ) {
    # Putting "beta" distribution here because with large `shape1` and/or
    # `shape2` parameters most probability lies far away from 0 and/or 1.
    # In cases of distributions "chisq", "f", "gamma", "lnorm", "weibull" `p` is
    # used for the left edge quantile instead of zero because there are
    # parameters with which most probability lies far away from zero.
    distr_edge_quantiles <- c(p, 1 - p)
  } else if (distr %in% c("cauchy")) {
    # Cutting more tails from Cauchy distribution because they are long and
    # heavy.
    big_p <- 1e3*p
    distr_edge_quantiles <- c(big_p, 1 - big_p)
  } else {
    stop_collapse('Distribution "', distr, '" is not honored properly.')
  }

  # `q_fun` represents quantile function of distribution. It should take
  # quantiles as first argument and `...` as others
  supp <- q_fun(distr_edge_quantiles, ...)

  # Stretching is needed for "beauty" purpose and to correctly handle density
  # going to infinity. In latter case 'pdqr' "infinity imputation" works better
  # than stepping a little bit aside from edge.
  stretch_to_total_supp(supp, total_supp = q_fun(c(0, 1), ...))
}

stretch_to_total_supp <- function(supp, total_supp, h = 1e-2) {
  delta <- h * diff(supp)
  if (supp[1] - total_supp[1] <= delta) {
    supp[1] <- total_supp[1]
  }
  if (total_supp[2] - supp[2] <= delta) {
    supp[2] <- total_supp[2]
  }

  supp
}


# Other -------------------------------------------------------------------
y_from_p_grid <- function(x, p) {
  # It is assumed that `x` has no duplicates and sorted increasingly
  n <- length(x)

  # `y_inter_sum` is vector: (y_1 + y_2; y_2 + y_3; ...; y_{n-1} + y_n)
  y_inter_sum <- 2 * diff(p) / diff(x)
  n_inter_sum <- length(y_inter_sum)

  # To solve this system of algebraic equations for y_1, ..., y_n precisely one
  # needs to introduce extra condition to complete the set. However, more stable
  # approach is to solve it approximately. Thus this function's output are
  # density values that "approximate" and not "interpolate" CDF values.
  # Approximation is also needed because there might be points of discontinuity
  # which should be approximated with piecewise-linear continuous density.

  # Used approximation assumes that "locally" y_{i-1}, y_{i}, and y_{i+1} lie
  # on the same straight line. This is used to estimate y_{i} (i from 2 to n-1).
  slopes <- diff(y_inter_sum) / (x[3:n] - x[1:(n-2)])
  intercepts <- (y_inter_sum[1:(n-2)] - slopes * (x[1:(n-2)] + x[2:(n-1)])) / 2
  y_2_to_nm1 <- slopes * x[2:(n-1)] + intercepts

  # Both y_1 and y_n are computed from sums directly
  y <- c(
    y_inter_sum[1] - y_2_to_nm1[1],
    y_2_to_nm1,
    y_inter_sum[n_inter_sum] - y_2_to_nm1[n_inter_sum - 1]
  )

  # Extra cutoffs to ensure positive `y`. Sometimes it is needed when `y` is
  # small near edges.
  y <- pmax(y, 0)

  # As `y` is not exact solution, total integral will not equal 1 so
  # renormalization should be done.
  y / trapez_integral(x, y)
}

assert_as_def_args <- function(f, support, n_grid) {
  assert_type(f, is.function)
  # `support` in `as_*.default()` is allowed to be `NULL` or have `NA`s
  if (!is.null(support)) {
    assert_support(support, allow_na = TRUE)
  }
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 2", min_val = 3
  )

  TRUE
}

assert_tot_prob <- function(tot_prob) {
  if (tot_prob <= 0) {
    stop_collapse("Total probability on supplied `support` isn't positive.")
  }

  TRUE
}

# Removes redundant rows from `x_tbl` corresponding to zero `y` values near the
# edges (beginning or end of data frame) except the most "center" ones. It
# doesn't affect the computation of future p- and d-functions.
# This is needed to ensure that q-function, created based on the `x_tbl`,
# returns not extreme results (withing `as_*.default()` functions), as it
# returns **the smallest** value with cumulative probability not more than
# input. For example, if first two rows of `x_tbl` have `x = c(-100, 0)` with
# zero `y` than future q-function `q()` would give `q(0) = -100`, which is not
# desirable because that `-100` is usually a result of too wide input `support`.
remove_zero_edge_y <- function(x_tbl) {
  n <- nrow(x_tbl)
  y <- x_tbl[["y"]]
  is_y_zero <- y == 0

  # `left_y_zero` is `TRUE` at all places in the beginning where `y == 0` except
  # for last (the most center) one
  left_y_zero <- cumsum(is_y_zero) == 1:n
  left_y_zero <- left_y_zero & duplicated.default(left_y_zero, fromLast = TRUE)

  # `right_y_zero` is `TRUE` at all places in the end where `y == 0` except for
  # first (the most center) one
  right_y_zero <- (cumsum(is_y_zero[n:1]) == 1:n)[n:1]
  right_y_zero <- right_y_zero & duplicated.default(right_y_zero)

  is_to_remove <- left_y_zero | right_y_zero
  if (any(is_to_remove)) {
    x_tbl <- x_tbl[!is_to_remove, ]
  }

  x_tbl
}

format_support <- function(support) {
  if (is.null(support)) {
    c(NA_real_, NA_real_)
  } else {
    support
  }
}
