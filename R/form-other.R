# form_mix ----------------------------------------------------------------
# Notes in docs. If `f_list` has both "infin" and "fin" pdqr-functions, then
# "fin" ones are **approximated** with "infin" ones in dirac-like fashion. This
# has some major consequences during computation of comparisons with mix of
# distributions. See example:
#
# my_mix <- form_mix(
#   list(new_d(0.5, "fin"), new_d(data.frame(x = 0:1, y = c(1, 1)), "infin"))
# )
#
# Output of the next code block should be equal to the sum of `0.5 * 1` (share
# of probability from first function) and `0.5 * 0.5` (share from second). Total
# is 0.75. However, because of "simmetrical" dirac-like approximation, only half
# of first probability is used (`0.5 * 0.5`) and the output is 0.5
#
# (my_mix >= 0.5)(1)
#
# However, little nudges help in this situation (note approximation):
#
# (my_mix >= 0.5 - 1e-4)(1)

#' Construct mixture of distributions
#'
#' Based on a list of pdqr-functions and vector of weights construct a
#' pdqr-function of corresponding mixture distribution.
#'
#' @param f_list List of pdqr-functions.
#' @param weights Numeric vector of weights.
#'
#' @return A pdqr-function for mixture distribution.
#'
#' @examples
#' d_unif <- as_d(dunif)
#' p_norm <- as_p(pnorm)
#'
#' my_mix <- form_mix(list(d_unif, p_norm), weights = c(0.7, 0.3))
#' plot(my_mix)
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
#' [new_*()][new_d()] function.
#'
#' @param f Pdqr-function.
#' @inheritParams form_trans
#'
#' @return A smoothed version of `f`.
#'
#' @examples
#' set.seed(101)
#'
#' # Pdqr-functions of type "infin"
#' bad_infin <- new_d(data.frame(x = sort(runif(100)), y = runif(100)), "infin")
#' smoothed_infin <- form_smooth(bad_infin)
#' plot(bad_infin)
#' lines(smoothed_infin, col = "blue")
#'
#' # Pdqr-functions of type "fin"
#' bad_fin <- new_d(data.frame(x = sort(runif(100)), prob = runif(100)), "fin")
#' smoothed_fin <- form_smooth(bad_fin)
#' plot(bad_fin)
#' lines(smoothed_fin, col = "blue")
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

  r_f <- as_r.pdqr(f)
  smpl <- r_f(n_sample)

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
  if (!all(est_smpl_is_number)) {
    stop_collapse("All outputs of `estimate` should be single numbers.")
  }
  est_smpl <- unlist(est_smpl)

  # Creating output pdqr-function for estimate distribution
  call_args <- c_dedupl(
    list(x = est_smpl), args_new, list(type = meta_type(f))
  )

  do.call(new_pdqr_by_ref(f), call_args)
}
