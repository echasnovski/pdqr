# form_mix ----------------------------------------------------------------
#' Form mixture of distributions
#'
#' Based on a list of pdqr-functions and vector of weights form a pdqr-function
#' for corresponding mixture distribution.
#'
#' @param f_list List of pdqr-functions. Can have different
#'   [classes][meta_class()] and [types][meta_type()] (see Details).
#' @param weights Numeric vector of weights or `NULL` (default; corresponds to
#'   equal weights). Should be non-negative numbers with positive sum.
#'
#' @details **Type of output mixture** is determined by the following algorithm:
#' - If `f_list` consists only from pdqr-functions of "fin" type, then output
#' will have "fin" type.
#' - If `f_list` has at least one pdqr-function of type "infin", then output
#' will have "infin" type. In this case all "fin" pdqr-functions in `f_list` are
#' approximated with corresponding dirac-like "infin" functions (with
#' [form_retype(*, method = "dirac")][form_retype()]). **Note** that this
#' approximation has consequences during computation of comparisons. For
#' example, if original "fin" function `f` is for distribution with one element
#' `x`, then probability of `f >= x` being true is 1. After retyping to
#' dirac-like function, this probability will be 0.5, because of symmetrical
#' dirac-like approximation. Using a little nudge to `x` of `1e-7` magnitude in
#' the correct direction (`f >= x - 1e-7` in this case) will have expected
#' output.
#'
#' **Class of output mixture** is determined by the class of the first element
#' of `f_list`. To change output class, use one of `as_*()` functions to change
#' class of first element in `f_list` or class of output.
#'
#' **Note** that if output "infin" pdqr-function for mixture distribution (in
#' theory) should have discontinuous density, it is approximated continuously:
#' discontinuities are represented as intervals in ["x_tbl"][meta_x_tbl()] with
#' extreme slopes (see Examples).
#'
#' @return A pdqr-function for mixture distribution of certain
#'   [type][meta_type()] and [class][meta_class()] (see Details).
#'
#' @examples
#' # All "fin"
#' d_binom <- as_d(dbinom, size = 10, prob = 0.5)
#' r_pois <- as_r(rpois, lambda = 1)
#' fin_mix <- form_mix(list(d_binom, r_pois))
#' plot(fin_mix)
#'
#' # All "infin"
#' p_norm <- as_p(pnorm)
#' d_unif <- as_d(dunif)
#'
#' infin_mix <- form_mix(list(p_norm, d_unif), weights = c(0.7, 0.3))
#'   # Output is a p-function, as is first element of `f_list`
#' infin_mix
#' plot(infin_mix)
#'
#'   # Use `as_*()` functions to change class
#' d_infin_mix <- as_d(infin_mix)
#'
#'   # Theoretical output density should be discontinuous, but here it is
#'   # approximated with continuous function
#' infin_x_tbl <- meta_x_tbl(infin_mix)
#' infin_x_tbl[(infin_x_tbl$x >= -1e-4) & (infin_x_tbl$x <= 1e-4), ]
#'
#' # Some "fin", some "infin"
#' all_mix <- form_mix(list(d_binom, d_unif))
#' plot(all_mix)
#' all_x_tbl <- meta_x_tbl(all_mix)
#'
#'   # What dirac-like approximation looks like
#' all_x_tbl[(all_x_tbl$x >= 1.5) & (all_x_tbl$x <= 2.5), ]
#'
#' @export
form_mix <- function(f_list, weights = NULL) {
  assert_type(f_list, is.list)
  assert_f_list(f_list, allow_numbers = FALSE)

  assert_type(weights, is.numeric, allow_null = TRUE)
  weights <- impute_weights(weights, length(f_list))

  f_list_meta <- compute_f_list_meta(f_list)
  res_type <- f_list_meta[["type"]]

  sec_col <- if (res_type == "fin") {"prob"} else {"y"}

  x_tbl_list <- lapply(seq_along(f_list), function(i) {
    f_typed <- form_retype(f_list[[i]], res_type, method = "dirac")

    x_tbl <- meta_x_tbl(f_typed)
    # Do weighting of distributions
    x_tbl[[sec_col]] <- x_tbl[[sec_col]] * weights[i]

    x_tbl
  })

  x_tbl <- stack_x_tbl(x_tbl_list)

  new_pdqr_by_class(f_list_meta[["class"]])(x_tbl, res_type)
}

impute_weights <- function(weights, n) {
  if (is.null(weights)) {
    weights <- rep(1, n) / n
  } else {
    weights <- recycle_vec(weights, n)
  }

  if (any(weights < 0)) {
    stop_collapse("`weights` should not have negative elements")
  }
  if (sum(weights) <= 0) {
    stop_collapse("`weights` should have positive sum.")
  }

  weights / sum(weights)
}


# form_smooth -------------------------------------------------------------
#' Smooth pdqr-function
#'
#' Smooth pdqr-function using random sampling and corresponding
#' [new_*()][new-pdqr] function.
#'
#' @inheritParams form_trans_self
#'
#' @details General idea of smoothing is to preserve "sampling randomness" as
#' much as reasonably possible while creating more "smooth" probability mass or
#' density function.
#'
#' At first step, sample of size `n_sample` is generated from distribution
#' represented by `f`. Then, based on the sample, "infin" d-function is created
#' with `new_d()` and arguments from `args_new` list. To account for
#' [density()][stats::density()]'s default behavior of "stretching range" by
#' adding small tails, [support][meta_support()] of d-function is forced to be
#' equal to `f`'s support (this is done with [form_resupport()] and method
#' "reflect"). Output represents a "smooth" version of `f` as d-function.
#'
#' Final output is computed by modifying "y" or "prob" column of `f`'s ["x_tbl"
#' metadata][meta_x_tbl()] to be proportional to values of "smooth" output at
#' corresponding points from "x" column. This way output distribution has
#' exactly the same "x" grid as `f` but "more smooth" nature.
#'
#' @return A smoothed version of `f` with the same [class][meta_class()] and
#'   [type][meta_type()].
#'
#' @examples
#' set.seed(101)
#'
#' # Type "fin"
#' bad_fin <- new_d(data.frame(x = sort(runif(100)), prob = runif(100)), "fin")
#' smoothed_fin <- form_smooth(bad_fin)
#' plot(bad_fin)
#' lines(smoothed_fin, col = "blue")
#'
#' # Type "infin"
#' bad_infin <- new_d(data.frame(x = sort(runif(100)), y = runif(100)), "infin")
#' smoothed_infin <- form_smooth(bad_infin)
#' plot(bad_infin)
#' lines(smoothed_infin, col = "blue")
#'
#' @export
form_smooth <- function(f, n_sample = 10000, args_new = list()) {
  assert_pdqr_fun(f)
  assert_type(
    n_sample, is_single_number,
    type_name = "single number more than 1",
    min_val = 2
  )
  assert_type(args_new, is.list)

  f_x_tbl <- meta_x_tbl(f)
  pdqr_fun <- new_pdqr_by_ref(f)

  # Handle edge case of single point input (which is possible only if type of
  # `f` is "fin")
  if (nrow(f_x_tbl) == 1) {
    return(pdqr_fun(f_x_tbl[["x"]][1], "fin"))
  }

  smpl <- as_r(f)(n_sample)

  # Smooth with `density()`
  call_args <- c_dedupl(list(x = smpl, type = "infin"), args_new)
  infin_d <- do.call(new_d, call_args)

  # Account for extra tails that appeared after using `density()`
  infin_d <- form_resupport(
    infin_d, support = meta_support(f), method = "reflect"
  )

  # Output probabilities (or densities) are proportional to smoothed "infin"
  # density. The logic behind this is that "smoothing data" basically means
  # reducing the amount of "jumps" between close data points. In other words,
  # the closer the points the smaller should be difference in
  # probabilities/densities. This also results into reducing variance of
  # probabilities if "x"s are relatively dense.
  f_x_tbl[[get_x_tbl_sec_col(f_x_tbl)]] <- infin_d(f_x_tbl[["x"]])

  pdqr_fun(f_x_tbl, type = meta_type(f))
}


# form_estimate -----------------------------------------------------------
#' Create a pdqr-function for distribution of sample estimate
#'
#' Based on pdqr-function, estimate function, and sample size describe the
#' distribution of sample estimate. This might be useful for statistical
#' inference.
#'
#' @param f A pdqr-function.
#' @param estimate Estimate function. Should be able to accept numeric vector of
#'   size `sample_size` and return single numeric or logical output.
#' @param sample_size Size of sample for which distribution of sample estimate
#'   is needed.
#' @param ... Other arguments for `estimate`.
#' @param n_sample Number of elements to generate from distribution of sample
#'   estimate.
#' @param args_new List of extra arguments for [new_*()][new-pdqr] function to
#'   control [density()].
#'
#' @details General idea is to create a sample from target distribution by
#' generating `n_sample` samples of size `sample_size` and compute for each of
#' them its estimate by calling input `estimate` function. If created sample is
#' logical, **boolean** pdqr-function (type "fin" with elements being exactly 0
#' and 1) is created with probability of being true estimated as share of `TRUE`
#' values (after removing possible `NA`). If sample is numeric, it is used as
#' input to `new_*()` of appropriate class with `type` equal to type of `f` (if
#' not forced otherwise in `args_new`).
#'
#' **Notes**:
#' - This function may be very time consuming for large values of `n_sample` and
#' `sample_size`, as total of `n_sample*sample_size` numbers are generated and
#' `estimate` function is called `n_sample` times.
#' - Output distribution might have a bias compared to true distribution of
#' sample estimate. One useful technique for bias correction: compute mean value
#' of estimate using big `sample_size` (with `mean(as_r(f)(sample_size))`) and
#' then recenter distribution to actually have that as a mean.
#'
#' @return A pdqr-function of the same [class][meta_class()] and
#'   [type][meta_type()] (if not forced otherwise in `args_new`) as `f`.
#'
#' @examples
#' set.seed(101)
#'
#' # Type "fin"
#' d_fin <- new_d(data.frame(x = 1:4, prob = 1:4/10), "fin")
#'   # Estimate of distribution of mean
#' form_estimate(d_fin, estimate = mean, sample_size = 10)
#'   # To change type of output, supply it in `args_new`
#' form_estimate(
#'   d_fin, estimate = mean, sample_size = 10,
#'   args_new = list(type = "infin")
#' )
#'
#' # Type "infin"
#' d_unif <- as_d(dunif)
#'   # Supply extra named arguments for `estimate` in `...`
#' plot(form_estimate(d_unif, estimate = mean, sample_size = 10, trim = 0.1))
#' lines(
#'   form_estimate(d_unif, estimate = mean, sample_size = 10, trim = 0.3),
#'   col = "red"
#' )
#' lines(
#'   form_estimate(d_unif, estimate = median, sample_size = 10),
#'   col = "blue"
#' )
#'
#' # Estimate can return single logical value
#' d_norm <- as_d(dnorm)
#' all_positive <- function(x) {all(x > 0)}
#'   # Probability of being true should be around 0.5^5
#' form_estimate(d_norm, estimate = all_positive, sample_size = 5)
#'
#' @export
form_estimate <- function(f, estimate, sample_size, ...,
                          n_sample = 10000, args_new = list()) {
  assert_pdqr_fun(f)
  assert_type(estimate, is.function)
  assert_type(
    sample_size, is_single_number,
    type_name = "single positive number", min_val = 1
  )
  assert_type(
    n_sample, is_single_number,
    type_name = "single positive number", min_val = 1
  )
  assert_type(args_new, is.list)

  # Producing sample of estimates
  r_f <- as_r(f)
  est_smpl <- lapply(seq_len(n_sample), function(i) {
    estimate(r_f(sample_size), ...)
  })

  # Check outputs of `estimate`
  est_smpl_is_number <- vapply(est_smpl, is_single_number, logical(1))
  est_smpl_is_bool <- vapply(
    # Not using `is_truefalse()` here because `NA` output is allowed
    est_smpl, function(x) {is.logical(x) && (length(x) == 1)}, logical(1)
  )
  if (!all(est_smpl_is_number | est_smpl_is_bool)) {
    stop_collapse(
      "All outputs of `estimate` should be single numeric or logical values."
    )
  }
  est_smpl <- unlist(est_smpl)

  # Return boolean pdqr-function if all outputs are logical
  if (is.logical(est_smpl)) {
    prob_true <- mean(est_smpl, na.rm = TRUE)

    return(boolean_pdqr(prob_true, meta_class(f)))
  }

  # Creating output pdqr-function for numeric estimate distribution
  call_args <- c_dedupl(
    list(x = est_smpl), args_new, list(type = meta_type(f))
  )

  do.call(new_pdqr_by_ref(f), call_args)
}
