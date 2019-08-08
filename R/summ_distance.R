# summ_distance() ---------------------------------------------------------
#' Summarize pair of distributions with distance
#'
#' This function computes distance between two distributions represented by
#' pdqr-functions. Here "distance" is used in a broad sense: a single
#' non-negative number representing how much two distributions differ from one
#' another. Bigger values indicate bigger difference. Zero value means that
#' input distributions are equivalent based on the method used. The notion of
#' "distance" is useful for doing statistical inference about similarity of two
#' groups of numbers.
#'
#' @param f A pdqr-function of any [type][meta_type()] and
#'   [class][meta_class()].
#' @param g A pdqr-function of any type and class.
#' @param method Method for computing distance. Should be one of "KS", "totvar",
#'   "compare", "wass", "cramer", "align", "entropy".
#'
#' @details Methods can be separated into three categories: probability based,
#' metric based, and entropy based.
#'
#' **Probability based** methods return a number between 0 and 1 which is
#' computed in the way that mostly based on probability:
#' - *Method "KS"* (short for Kolmogorov-Smirnov) computes the supremum of
#' absolute difference between p-functions corresponding to `f` and `g` (`|F -
#' G|`). Here "supremum" is meant to describe the fact that if input functions
#' have different [types][meta_type()], there can be no point at which "KS"
#' distance is achieved. Instead, there might be a sequence of points from left
#' to right with `|F - G|` values tending to the result (see Examples).
#' - *Method "totvar"* (short for "total variation") computes a biggest absolute
#' difference of probabilities for any subset of real line. In other words,
#' there is a set of points for "discrete" type and intervals for "continuous",
#' total probability of which under `f` and `g` differs the most. **Note** that
#' if `f` and `g` have different types, output is always 1. The set of interest
#' consists from all "x" values of "discrete" pdqr-function: probability under
#' "discrete" distribution is 1 and under "continuous" is 0.
#' - *Method "compare"* represents a value computed based on probabilities of
#' one distribution being bigger than the other (see [pdqr methods for "Ops"
#' group generic family][methods-group-generic] for more details on comparing
#' pdqr-functions). It is computed as
#' `2*max(P(F > G), P(F < G)) + 0.5*P(F = G) - 1` (here `P(F > G)` is basically
#' `summ_prob_true(f > g)`). This is maximum of two values (`P(F > G) + 0.5*P(F
#' = G)` and `P(F < G) + 0.5*P(F = G)`), normalized to return values from 0
#' to 1. Other way to look at this measure is that it computes (before
#' normalization) two [ROC AUC][summ_rocauc()] values with method `"expected"`
#' for two possible ordering (`f, g`, and `g, f`) and takes their maximum.
#'
#' **Metric based** methods compute "how far" two distributions are apart on the
#' real line:
#' - *Method "wass"* (short for "Wasserstein") computes a 1-Wasserstein
#' distance: "minimum cost of 'moving' one density into another", or "average
#' path density point should go while transforming from one into another". It is
#' computed as integral of `|F - G|` (absolute difference between p-functions).
#' If any of `f` and `g` has "continuous" type, [stats::integrate()] is used, so
#' relatively small numerical errors can happen.
#' - *Method "cramer"* computes Cramer distance: integral of `(F - G)^2`. This
#' somewhat relates to "wass" method as [variance][summ_var()] relates to [first
#' central absolute moment][summ_moment()]. Relatively small numerical errors
#' can happen.
#' - *Method "align"* computes an absolute value of shift `d` (possibly
#' negative) that should be added to `f` to achieve both `P(f+d >= g) >= 0.5`
#' and `P(f+d <= g) >= 0.5` (in other words, align `f+d` and `g`) as close as
#' reasonably possible. Solution is found numerically with [stats::uniroot()],
#' so relatively small numerical errors can happen. Also **note** that this
#' method is somewhat slow (compared to all others). To increase speed, use less
#' elements in ["x_tbl" metadata][meta_x_tbl()]. For example, with
#' [form_retype()] or smaller `n_grid` argument in [as_*()][as_p()] functions.
#'
#' **Entropy based** methods compute output based on entropy characteristics:
#' - *Method "entropy"* computes sum of two Kullback-Leibler divergences:
#' `KL(f, g) + KL(g, f)`, which are outputs of [summ_entropy2()] with method
#' "relative". **Notes**:
#'     - If `f` and `g` don't have the same support, distance can be very high.
#'     - Error is thrown if `f` and `g` have different types (the same as in
#'     `summ_entropy2()`).
#'
#' @return A single non-negative number representing distance between pair of
#'   distributions. For methods "KS", "totvar", and "compare" it is not bigger
#'   than 1.
#'
#' @seealso [summ_separation()] for computation of optimal threshold separating
#'   pair of distributions.
#'
#' @family summary functions
#'
#' @examples
#' d_unif <- as_d(dunif, max = 2)
#' d_norm <- as_d(dnorm, mean = 1)
#'
#' vapply(
#'   c("KS", "totvar", "compare", "wass", "cramer", "align", "entropy"),
#'   function(meth) {summ_distance(d_unif, d_norm, method = meth)},
#'   numeric(1)
#' )
#'
#' # "Supremum" quality of "KS" distance
#' d_dis <- new_d(2, "discrete")
#'   # Distance is 1, which is a limit of |F - G| at points which tend to 2 from
#'   # left
#' summ_distance(d_dis, d_unif, method = "KS")
#'
#' @export
summ_distance <- function(f, g, method = "KS") {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(method, is_string)
  assert_in_set(
    method, c("KS", "totvar", "compare", "wass", "cramer", "align", "entropy")
  )

  switch(
    method,
    KS = distance_ks(f, g),
    totvar = distance_totvar(f, g),
    compare = distance_compare(f, g),
    wass = distance_wass(f, g),
    cramer = distance_cramer(f, g),
    align = distance_align(f, g),
    entropy = distance_entropy(f, g)
  )
}


# Method "KS" -------------------------------------------------------------
distance_ks <- function(f, g) {
  p_f <- as_p(f)
  p_g <- as_p(g)

  f_type <- meta_type(f)
  g_type <- meta_type(g)

  if (f_type == "discrete") {
    if (g_type == "discrete") {
      distance_ks_two_dis(p_f, p_g)
    } else {
      distance_ks_mixed(p_dis = p_f, p_con = p_g)
    }
  } else {
    if (g_type == "discrete") {
      distance_ks_mixed(p_dis = p_g, p_con = p_f)
    } else {
      distance_ks_two_con(p_f, p_g)
    }
  }
}

distance_ks_two_dis <- function(p_f, p_g) {
  ks_sep <- separation_ks_two_dis(p_f, p_g)

  abs(p_f(ks_sep) - p_g(ks_sep))
}

distance_ks_mixed <- function(p_dis, p_con) {
  # Not using `separation_ks_mixed()` because of possible "limit" nature of K-S
  # distance which is a "supremum" and not "maximum". Its output might be
  # misleading because supremum distance might be achieved as left limit at the
  # point. See also commentary in `separation_ks_mixed()`.
  x_test <- meta_x_tbl(p_dis)[["x"]]

  p_con_cumprob <- p_con(x_test)

  p_dis_cumprob <- meta_x_tbl(p_dis)[["cumprob"]]
  p_dis_left_cumprob <- c(0, p_dis_cumprob[-length(p_dis_cumprob)])

  max(
    abs(p_con_cumprob - p_dis_cumprob),
    abs(p_con_cumprob - p_dis_left_cumprob)
  )
}

distance_ks_two_con <- function(p_f, p_g) {
  ks_sep <- separation_ks_two_con(p_f, p_g)

  abs(p_f(ks_sep) - p_g(ks_sep))
}


# Method "totvar" ---------------------------------------------------------
# **Notes**. Set (of finite values for "discrete" and of intervals for
# "continuous"), at which total variation distance is achieved, can be expressed
# as `A = {x | f(x) > g(x)}` (`f` and `g` are d-functions) or `B = {x | f(x) <
# g(x)}`. However, absolute differences in probabilities for `A` and `B` are
# equal. This is because:
# `0 = 1 - 1 = (P_f(A) + P_f(B) + P_f(C)) - (P_g(A) + P_g(B) + P_g(C))`, where
# `P_f` and `P_g` are probability measures of `f` and `g`;
# `C = {x | f(x) = g(x)}`.
# By definitions: `abs(P_f(A) - P_g(A)) = P_f(A) - P_g(A)`;
# `abs(P_f(B) - P_g(B)) = P_g(B) - P_f(B)`; `P_f(C) = P_g(C)`.
# Therefore: `abs(P_f(A) - P_g(A)) = abs(P_f(B) - P_g(B))`.
distance_totvar <- function(f, g) {
  d_f <- as_d(f)
  d_g <- as_d(g)

  num_dis <- (meta_type(f) == "discrete") + (meta_type(g) == "discrete")

  switch(
    as.character(num_dis),
    `0` = distance_totvar_two_con(d_f, d_g),
    # A target set is all `x` values of "discrete" pdqr-function. Its
    # probability under "discrete" is 1 and under "continuous" is zero because
    # it is countable.
    `1` = 1,
    `2` = distance_totvar_two_dis(d_f, d_g)
  )
}

distance_totvar_two_con <- function(d_f, d_g) {
  # `{x | d_f(x) > d_g(x)}` is a union of intervals where `d_f(x) - d_g(x)` has
  # constant positive sign. `d_f(x) - d_g(x)` can change sign in two cases:
  # - When `d_f` and `d_g` intersect.
  # - When either `d_f` or `d_g` shows discontinuity on edges.
  x_inters <- compute_density_crossings(d_f, d_g)

  # This might introduce duplicate elements on the edges (if `d_f` and `d_g`
  # intersect on any support edge) but they will introduce "interval" with zero
  # "sign" which will later be ignored.
  x_lim <- sort(c(x_inters, meta_support(d_f), meta_support(d_g)))

  interval_center <- (x_lim[-1] + x_lim[-length(x_lim)]) / 2
  pos_sign_inds <- which(d_f(interval_center) > d_g(interval_center))

  # Note: if `pos_sign_inds` is empty, then `f` and `g` are identical. In that
  # case both `x_lim_left` and `x_lim_right` are empty and `sum()` later will
  # return 0, which is correct answer.
  x_lim_left <- x_lim[pos_sign_inds]
  x_lim_right <- x_lim[pos_sign_inds+1]

  p_f <- as_p(d_f)
  p_g <- as_p(d_g)

  # Output distance is total difference in probabilities of intervals where `f`
  # is greater than `g`.
  sum(
    (p_f(x_lim_right) - p_f(x_lim_left)) - (p_g(x_lim_right) - p_g(x_lim_left))
  )
}

distance_totvar_two_dis <- function(d_f, d_g) {
  union_x <- union_x(d_f, d_g)
  prob_diff <- d_f(union_x) - d_g(union_x)

  sum(prob_diff[prob_diff > 0])
}


# Method "compare" --------------------------------------------------------
# This is basically `max(P(f > g) + 0.5*P(f == g), P(g > f) + 0.5*P(f == g))`,
# normalized to return values from 0 to 1 (`P(x)` is `summ_prob_true(x)`).
# Addition of `0.5*P(f == g)` is to ensure that 0.5 is returned when `f` and `g`
# are identical (useful to think about this as maximum between two "symmetric"
# ROCAUCs computed with "expected" method). This also means zero distance for
# identical inputs.
# Here equation `prob_geq()` is used for performance reasons and based on the
# following equation: `max(P(f>g), P(f<g)) + 0.5*P(f==g) =
# max(P(f>=g), P(f<=g)) - P(f==g) + 0.5*P(f==g)`. After `y = 2*x-1`
# normalization, this is the output.
distance_compare <- function(f, g) {
  f_eq_g <- prob_equal(f, g)
  f_geq_g <- prob_geq(f, g)

  # prob_geq(g, f) = 1 - prob_geq(f, g) + prob_equal(f, g)
  2 * max(f_geq_g, 1 - f_geq_g + f_eq_g) - prob_equal(f, g) - 1
}


# Method "wass" -----------------------------------------------------------
distance_wass <- function(f, g) {
  integrate_cdf_absdiff(p_f = as_p(f), p_g = as_p(g), power = 1)
}


# Method "cramer" ---------------------------------------------------------
distance_cramer <- function(f, g) {
  integrate_cdf_absdiff(p_f = as_p(f), p_g = as_p(g), power = 2)
}


# distance_align() --------------------------------------------------------
distance_align <- function(f, g) {
  f_supp <- meta_support(f)
  g_supp <- meta_support(g)

  f_geq_g <- prob_geq(f, g) >= 0.5
  g_geq_f <- prob_geq(g, f) >= 0.5

  # Handle edge case of identical "discrete" pdqr-functions
  if (f_geq_g && g_geq_f) {
    return(0)
  }

  if (f_geq_g) {
    # Moving `f` to the left
    search_interval <- c(g_supp[1] - f_supp[2], 0)
  } else {
    # Moving `f` to the right
    search_interval <- c(0, g_supp[2] - f_supp[1])
  }

  target_fun <- function(delta) {
    prob_geq(f + delta, g) - 0.5
  }

  res <- stats::uniroot(
    target_fun, interval = search_interval, extendInt = "yes"
  )[["root"]]

  abs(res)
}


# Method "entropy" --------------------------------------------------------
distance_entropy <- function(f, g) {
  # This is mostly the same as sum of `summ_entropy2(*, *, method = "relative")`
  # but without extra `assert_*()` checks. **Note** that default value of `clip`
  # argument here should be the same as default value of `summ_entropy2()`.
  res <- cross_entropy(f, g) - cross_entropy(f, f) +
    cross_entropy(g, f) - cross_entropy(g, g)

  # Account for numerical representation issues
  max(res, 0)
}


# Helpers -----------------------------------------------------------------
integrate_cdf_absdiff <- function(p_f, p_g, power) {
  if ((meta_type(p_f) == "discrete") && (meta_type(p_g) == "discrete")) {
    union_x <- union_x(p_f, p_g)
    abs_diff_cumprob <- abs(p_f(union_x) - p_g(union_x))

    sum(diff(union_x) * abs_diff_cumprob[-length(union_x)]^power)
  } else {
    integr_range <- union_support(p_f, p_g)

    stats::integrate(
      f = function(x) {abs(p_f(x) - p_g(x))^power},
      lower = integr_range[1],
      upper = integr_range[2],
      subdivisions = 1e3
    )[["value"]]
  }
}
