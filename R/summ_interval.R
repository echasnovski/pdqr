#' Summarize distribution with interval
#'
#' These functions summarize distribution with one interval based on method of
#' choice.
#'
#' @inheritParams summ_mean
#' @param level A number between 0 and 1 representing a coverage degree of
#'   interval. Interpretation depends on `method` but the bigger is number, the
#'   wider is interval.
#' @param method Method of interval computation. Should be on of "minwidth",
#'   "percentile", "sigma".
#' @param n_grid Number of grid elements to be used for "minwidth" method (see
#'   Details).
#'
#' @details Method "minwidth" searches for an interval with total probability of
#' `level` that has minimum width. This is done with grid search: `n_grid`
#' possible intervals with `level` total probability are computed and the one
#' with minimum width is returned (if there are several, the one with the
#' smallest left end). Left ends of computed set of intervals are created as a
#' grid from `0` to `1-level` quantiles with `n_grid` number of elements. Right
#' ends are computed so that intervals have `level` total probability.
#'
#' Method "percentile" returns an interval with edges being `0.5*(1-level)` and
#' `1 - 0.5*(1-level)` quantiles. Output has total probability equal to `level`.
#'
#' Method "sigma" computes an interval symmetrically centered at
#' [mean][summ_mean()] of distribution. Left and right edges are distant from
#' center by the amount of [standard deviation][summ_sd()] multiplied by
#' `level`'s critical value. Critical value is computed using [normal
#' distribution][stats::qnorm()] as `qnorm(1 - 0.5*(1-level))`, which
#' corresponds to a way of computing sample confidence interval with known
#' standard deviation. The final output interval is possibly cut so that not to
#' be out of `f`'s [support][meta_support()].
#'
#' **Note** that supported methods correspond to different ways of computing
#' [distribution's center][summ_center()]. This idea is supported by the fact
#' that when `level` is 0, "minwidth" method returns zero width interval at
#' distribution's [global mode][summ_mode()], "percentile" method -
#' [median][summ_median()], "sigma" - [mean][summ_mean()].
#'
#' @return A [region][region] with one row. That is a data frame with one row
#'   and the following columns:
#' - **left** <dbl> : Left end of interval.
#' - **right** <dbl> : Right end of interval.
#'
#' To return a simple numeric vector, call [unlist()][base::unlist()] on
#' `summ_interval()`'s output (see Examples).
#'
#' @seealso [summ_hdr()] for computing of Highest Density Region, which can
#'   summarize distribution with multiple intervals.
#'
#' [region_*()][region] family of functions for working with `summ_interval()`
#' output.
#'
#' @family summary functions
#'
#' @examples
#' # Type "discrete"
#' d_dis <- new_d(data.frame(x = 1:6, prob = c(3:1, 0:2)/9), "discrete")
#' summ_interval(d_dis, level = 0.5, method = "minwidth")
#' summ_interval(d_dis, level = 0.5, method = "percentile")
#' summ_interval(d_dis, level = 0.5, method = "sigma")
#'
#'   # Visual difference between methods
#' plot(d_dis)
#' region_draw(summ_interval(d_dis, 0.5, method = "minwidth"), col = "blue")
#' region_draw(summ_interval(d_dis, 0.5, method = "percentile"), col = "red")
#' region_draw(summ_interval(d_dis, 0.5, method = "sigma"), col = "green")
#'
#' # Type "continuous"
#' d_con <- form_mix(
#'   list(as_d(dnorm), as_d(dnorm, mean = 5)),
#'   weights = c(0.25, 0.75)
#' )
#' summ_interval(d_con, level = 0.5, method = "minwidth")
#' summ_interval(d_con, level = 0.5, method = "percentile")
#' summ_interval(d_con, level = 0.5, method = "sigma")
#'
#'   # Visual difference between methods
#' plot(d_con)
#' region_draw(summ_interval(d_con, 0.5, method = "minwidth"), col = "blue")
#' region_draw(summ_interval(d_con, 0.5, method = "percentile"), col = "red")
#' region_draw(summ_interval(d_con, 0.5, method = "sigma"), col = "green")
#'
#' # Output interval is always inside input's support. Formally, next code
#' # should return interval from `-Inf` to `Inf`, but output is cut to be inside
#' # support.
#' summ_interval(d_con, level = 1, method = "sigma")
#'
#' # To get vector output, use `unlist()`
#' unlist(summ_interval(d_con))
#'
#' @export
summ_interval <- function(f, level = 0.95, method = "minwidth",
                          n_grid = 10001) {
  assert_pdqr_fun(f)
  assert_type(
    level, is_single_number,
    type_name = "single number between 0 and 1",
    min_val = 0, max_val = 1
  )
  assert_type(method, is_string)
  assert_in_set(method, c("minwidth", "percentile", "sigma"))
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 1",
    min_val = 1
  )

  edge_probs <- 0.5 * c(1-level, 1+level)
  res <- switch(
    method,
    minwidth = interval_minwidth(f, level, n_grid),
    percentile = as_q(f)(edge_probs),
    sigma = summ_mean(f) + stats::qnorm(edge_probs) * summ_sd(f)
  )
  f_supp <- meta_support(f)

  region_new(left = max(f_supp[1], res[1]), right = min(f_supp[2], res[2]))
}

interval_minwidth <- function(f, level = 0.95, n_grid = 10001) {
  if (level == 0) {
    mode <- summ_mode(f, method = "global")

    return(c(mode, mode))
  }

  n_seq <- seq_len(n_grid)

  left_prob_seq <- seq(0, 1-level, length.out = n_grid)
  prob_seq <- c(left_prob_seq, left_prob_seq + level)
  quants <- as_q(f)(prob_seq)

  width_vec <- quants[n_seq + n_grid] - quants[n_seq]
  # Rounding is needed to overcome numerical storage precision issues
  minwidth_ind <- which.min(round(width_vec, digits = 10))

  quants[minwidth_ind + c(0, n_grid)]
}
