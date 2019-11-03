# Documentation of `as_*()` -----------------------------------------------
#' Convert to pdqr-function
#'
#' Convert some function to be a proper pdqr-function of specific
#' [class][meta_class()], i.e. a function describing distribution with finite
#' support and finite values of probability/density.
#'
#' @param f Appropriate function to be converted (see Details).
#' @param support Numeric vector with two increasing elements describing desired
#'   support of output. If `NULL` or any its value is `NA`, detection is done
#'   using specific algorithms (see Details).
#' @param ... Extra arguments to `f`.
#' @param n_grid Number of grid points at which `f` will be evaluated (see
#'   Details). Bigger values lead to better approximation precision, but worse
#'   memory usage and evaluation speed (direct and in `summ_*()` functions).
#' @param n_sample Number of points to sample from `f` inside `as_r()`.
#' @param args_new List of extra arguments for [new_d()] to control [density()]
#'   inside `as_r()`.
#'
#' @details General purpose of `as_*()` functions is to create a proper
#' pdqr-function of desired class from input which doesn't satisfy these
#' conditions. Here is described sequence of steps which are taken to achieve
#' that goal.
#'
#' If **`f` is already a pdqr-function**, `as_*()` functions properly update it
#' to have specific class. They take input's ["x_tbl" metadata][meta_x_tbl()]
#' and [type][meta_type()] to use with corresponding [new_*()][new-pdqr]
#' function. For example, `as_p(f)` in case of pdqr-function `f` is essentially
#' the same as `new_p(x = meta_x_tbl(f), type = meta_type(f))`.
#'
#' If **`f` is a function describing "honored" distribution**, it is detected
#' and output is created in predefined way taking into account extra arguments
#' in `...`. For more details see "Honored distributions" section.
#'
#' If **`f` is some other unknown function**, `as_*()` functions use heuristics
#' for approximating input distribution with a "proper" pdqr-function. Outputs
#' of `as_*()` can be only pdqr-functions of type "continuous" (because of
#' issues with support detection). It is assumed that `f` returns values
#' appropriate for desired output class of `as_*()` function and output type
#' "continuous". For example, input for `as_p()` should return values of some
#' continuous cumulative distribution function (monotonically non-increasing
#' values from 0 to 1). To manually create function of type "discrete", supply
#' data frame input describing it to appropriate `new_*()` function.
#'
#' General algorithm of how `as_*()` functions work for unknown function is as
#' follows:
#' - **Detect support**. See "Support detection" section for more details.
#' - **Create data frame input for `new_*()`**. The exact process differs:
#'     - In `as_p()` equidistant grid of `n_grid` points is created inside
#'     detected support. After that, input's values at the grid is taken as
#'     reference points of cumulative distribution function used to
#'     *approximate* density at that same grid. This method showed to work more
#'     reliably in case density goes to infinity. That grid and density values
#'     are used as "x" and "y" columns of data frame input for `new_p()`.
#'     - In `as_d()` "x" column of data frame is the same equidistant grid is
#'     taken as in `as_p()`. "y" column is taken as input's values at this grid
#'     after possibly imputing infinity values. This imputation is done by
#'     taking maximum from left and right linear extrapolations on mentioned
#'     grid.
#'     - In `as_q()`, at first inverse of input `f` function is computed on \[0;
#'     1\] interval. It is done by approximating it with piecewise-linear
#'     function on \[0; 1\] equidistant grid with `n_grid` points, imputing
#'     infinity values (which ensures finite support), and computing inverse of
#'     approximation. This inverse of `f` is used to create data frame input
#'     with `as_p()`.
#'     - In `as_r()` at first d-function with `new_d()` is created based on the
#'     same sample used for support detection and extra arguments supplied as
#'     list in `args_new` argument. In other words, density estimation is done
#'     based on sample, generated from input `f`. After that, its values are
#'     used to create data frame with `as_d()`.
#' - **Use appropriate `new_*()` function** with data frame from previous step
#' and `type = "continuous"`. This step implies that all tails outside detected
#' support are trimmed and data frame is normalized to represent proper
#' piecewise-linear density.
#'
#' @section Honored distributions:
#'
#' For efficient workflow, some commonly used distributions are recognized as
#' special ("honored"). Those receive different treatment in `as_*()` functions.
#'
#' Basically, there is a manually selected list of "honored" distributions with
#' all their information enough to detect them. Currently that list has all
#' common univariate distributions [from 'stats' package][stats::Distributions],
#' i.e. all except multinomial and "less common distributions of test
#' statistics".
#'
#' "Honored" distribution is **recognized only if `f` is one of `p*()`, `d*()`,
#' `q*()`, or `r*()` function describing honored distribution and is supplied as
#' variable with original name**. For example, `as_d(dunif)` will be treated as
#' "honored" distribution but `as_d(function(x) {dunif(x)})` will not.
#'
#' After it is recognized that input `f` represents "honored" distribution,
#' **its support is computed based on predefined rules**. Those take into
#' account special features of distribution (like infinite support or infinite
#' density values) and supplied extra arguments in `...`. Usually output support
#' "loses" only around `1e-6` probability on each infinite tail.
#'
#' After that, for "discrete" type output `new_d()` is used for appropriate data
#' frame input and for "continuous" - `as_d()` with appropriate `d*()` function
#' and support. D-function is then converted to desired class with `as_*()`.
#'
#' @section Support detection:
#'
#' In case input is a function without any extra information, `as_*()` functions
#' must know which finite support its output should have. User can supply
#' desired support directly with `support` argument, which can also be `NULL`
#' (mean automatic detection of both edges) or have `NA` to detect only those
#' edges.
#'
#' Support is detected in order to preserve as much information as practically
#' reasonable. Exact methods differ:
#' - In `as_p()` support is detected as values at which input function is equal
#' to `1e-6` (left edge detection) and `1 - 1e-6` (right edge), which means
#' "losing" `1e-6` probability on each tail. **Note** that those values are
#' searched inside \[-10^100; 10^100\] interval.
#' - In `as_d()`, at first an attempt at finding one point of non-zero density
#' is made by probing 10000 points spread across wide range of real line
#' (approximately from `-1e7` to `1e7`). If input's value at all of them is
#' zero, error is thrown. After finding such point, cumulative distribution
#' function is made by integrating input with [integrate()][stats::integrate()]
#' using found point as reference (without this there will be poor accuracy of
#' `integrate()`). Created CDF function is used to find `1e-6` and `1 - 1e-6`
#' quantiles as in `as_p()`, which serve as detected support.
#' - In `as_q()` quantiles for 0 and 1 are probed for being infinite. If they
#' are, `1e-6` and `1 - 1e-6` quantiles are used respectively instead of
#' infinite values to form detected support.
#' - In `as_r()` sample of size `n_sample` is generated and detected support is
#' its range stretched by mean difference of sorted points (to account for
#' possible tails at which points were not generated). **Note** that this means
#' that original input `f` "demonstrates its randomness" only once inside
#' `as_r()`, with output then used for approximation of "original randomness".
#'
#' @return A pdqr-function of corresponding [class][meta_class()].
#'
#' @seealso [pdqr_approx_error()] for computing approximation errors compared to
#' some reference function (usually input to `as_*()` family).
#'
#' @examples
#' # Convert existing "proper" pdqr-function
#' set.seed(101)
#' x <- rnorm(10)
#' my_d <- new_d(x, "continuous")
#'
#' my_p <- as_p(my_d)
#'
#' # Convert "honored" function to be a proper pdqr-function. To use this
#' # option, supply originally named function.
#' p_unif <- as_p(punif)
#' r_beta <- as_r(rbeta, shape1 = 2, shape2 = 2)
#' d_pois <- as_d(dpois, lambda = 5)
#'
#'   # `pdqr_approx_error()` computes pdqr approximation error
#' summary(pdqr_approx_error(as_d(dnorm), dnorm))
#'
#'   # This will work as if input is unkonw function because of unsupported
#'   # variable name
#' my_runif <- function(n) {runif(n)}
#' r_unif_2 <- as_r(my_runif)
#' plot(as_d(r_unif_2))
#'
#' # Convert some other function to be a "proper" pdqr-function
#' my_d_quadr <- as_d(function(x) {0.75*(1 - x^2)}, support = c(-1, 1))
#'
#' # Support detection
#' unknown <- function(x) {dnorm(x, mean = 1)}
#'   # Completely automatic support detection
#' as_d(unknown)
#'   # Semi-automatic support detection
#' as_d(unknown, support = c(-4, NA))
#' as_d(unknown, support = c(NA, 5))
#'
#'   # If support is very small and very distant from zero, it probably won't
#'   # get detected in `as_d()` (throwing a relevant error)
#' \dontrun{
#' as_d(function(x) {dnorm(x, mean = 10000, sd = 0.1)})
#' }
#'
#' # Using different level of granularity
#' as_d(unknown, n_grid = 1001)
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

  if (distr_info[["type"]] == "discrete") {
    # This approach assumes that honored "discrete" pdqr-functions have only
    # integer "x" values. If in future it is not true, some (serious)
    # refactoring should be made here.
    x_vec <- supp[1]:supp[2]
    x_tbl <- data.frame(x = x_vec, prob = distr_info[["d_fun"]](x_vec, ...))

    res <- new_d(x_tbl, type = "discrete")
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
      # "discrete"
      "nbinom", "pois",
      # "continuous"
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
  assert_missing(f, "distribution function")
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
# edges (beginning or end of data frame) in a way depending on type of "x_tbl"'s
# pdqr-function. It doesn't affect the computation of future p- and d-functions.
# This is needed to ensure that q-function, created based on the `x_tbl`,
# returns not extreme results (withing `as_*.default()` functions), as it
# returns **the smallest** value with cumulative probability not more than
# input. For example, if first two rows of `x_tbl` have `x = c(-100, 0)` with
# zero `y` than future q-function `q()` would give `q(0) = -100`, which is not
# desirable because that `-100` is usually a result of too wide input `support`.
remove_zero_edge_y <- function(x_tbl) {
  d_vals <- x_tbl[[get_x_tbl_sec_col(x_tbl)]]
  type <- get_type_from_x_tbl(x_tbl)

  is_to_remove <- is_zero_tail(d_vals, type)

  # Check is done for performance reasons
  if (any(is_to_remove)) {
    x_tbl <- x_tbl[!is_to_remove, ]
  }

  x_tbl
}

#' Check if element belongs to zero probability tail
#'
#' This is a utility function for detecting zero probability tails. Its input
#' `vec` is supposed to be one of "prob" or "y" column of "x_tbl" metadata
#' (depending on `type`). For "discrete" type element belongs to zero
#' probability tail if it is a part of group of zeros at the beginning or end of
#' input vector `vec`. For "continuous" the most center elements of groups
#' doesn't belong to zero probability tail because it is used to define positive
#' probability interval (due to piecewise-linear nature of density). For
#' example, for `vec = c(0, 0, 1, 0, 0)`:
#' - In case of "discrete" `type` elements 1, 2, 4, 5 are within zero
#' probability tails. And so output will be `c(TRUE, TRUE, FALSE, TRUE, TRUE)`.
#' - In case of "continuous" `type` only elements 1 and 5 are within zero
#' probability tails. It defines a triangular density from second to fourth
#' element. Output will be `c(TRUE, FALSE, FALSE, FALSE, TRUE)`.
#'
#' @param vec A numeric vector of either "prob" or "y" column of "x_tbl"
#'   metadata.
#' @param type Pdqr type. Should be one of "discrete" or "continuous".
#'
#' @return A logical vector indicating if element belonds to zero probability
#'   tail.
#'
#' @noRd
is_zero_tail <- function(vec, type) {
  n <- length(vec)
  is_zero <- vec == 0

  # Check for first element being zero is made for performance reasons
  if (is_zero[1]) {
    left_is_zero <- cumsum(is_zero) == 1:n

    if (type == "continuous") {
      # Exclude the "most center" zero
      left_is_zero <- left_is_zero &
        duplicated.default(left_is_zero, fromLast = TRUE)
    }
  } else {
    left_is_zero <- rep(FALSE, n)
  }

  # Check for last element being zero is made for performance reasons
  if (is_zero[n]) {
    right_is_zero <- (cumsum(is_zero[n:1]) == 1:n)[n:1]

    if (type == "continuous") {
      # Exclude the "most center" zero
      right_is_zero <- right_is_zero & duplicated.default(right_is_zero)
    }
  } else {
    right_is_zero <- rep(FALSE, n)
  }

  left_is_zero | right_is_zero
}

format_support <- function(support) {
  if (is.null(support)) {
    c(NA_real_, NA_real_)
  } else {
    support
  }
}
