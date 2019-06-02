# summ_distance() ---------------------------------------------------------
summ_distance <- function(f, g, method = "KS") {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(method, is_string)
  assert_in_set(method, c("KS", "totvar", "wass", "cramer", "entropy"))

  switch(
    method,
    KS = distance_ks(f, g),
    totvar = distance_totvar(f, g),
    wass = distance_wass(f, g),
    cramer = distance_cramer(f, g),
    entropy = distance_entropy(f, g)
  )
}


# Method "KS" -------------------------------------------------------------
# **Notes in docs**. Distance is equal to supremum of |F - G|, where F and G are
# corresponding p-functions.
distance_ks <- function(f, g) {
  p_f <- as_p(f)
  p_g <- as_p(g)

  f_type <- meta_type(f)
  g_type <- meta_type(g)

  if (f_type == "fin") {
    if (g_type == "fin") {
      distance_ks_two_fin(p_f, p_g)
    } else {
      distance_ks_mixed(p_fin = p_f, p_infin = p_g)
    }
  } else {
    if (g_type == "fin") {
      distance_ks_mixed(p_fin = p_g, p_infin = p_f)
    } else {
      distance_ks_two_infin(p_f, p_g)
    }
  }
}

distance_ks_two_fin <- function(p_f, p_g) {
  ks_sep <- separation_ks_two_fin(p_f, p_g)

  abs(p_f(ks_sep) - p_g(ks_sep))
}

distance_ks_mixed <- function(p_fin, p_infin) {
  # Not using `separation_ks_mixed()` because of possible "limit" nature of K-S
  # distance which is a "supremum" and not "maximum". Its output might be
  # misleading because supremum distance might be achieved as left limit at the
  # point. See also commentary in `separation_ks_mixed()`.
  x_test <- meta_x_tbl(p_fin)[["x"]]

  p_infin_cumprob <- p_infin(x_test)

  p_fin_cumprob <- meta_x_tbl(p_fin)[["cumprob"]]
  p_fin_left_cumprob <- c(0, p_fin_cumprob[-length(p_fin_cumprob)])

  max(
    abs(p_infin_cumprob - p_fin_cumprob),
    abs(p_infin_cumprob - p_fin_left_cumprob)
  )
}

distance_ks_two_infin <- function(p_f, p_g) {
  ks_sep <- separation_ks_two_infin(p_f, p_g)

  abs(p_f(ks_sep) - p_g(ks_sep))
}


# Method "totvar" ---------------------------------------------------------
# **Notes in docs**. Maximum absolute difference in probabilities across all
# possible sets. In other words, there should be some subset of real line (or a
# set of those) probabilities of (more formally, limit of) which under `f` and
# `g` differ the most. This set (of finite values for "fin" and of intervals for
# "infin") can be expressed as `A = {x | f(x) > g(x)}` (`f` and `g` are
# d-functions) or `B = {x | f(x) < g(x)}`.
# However, absolute differences in probabilities for `A` and `B` are equal. This
# is because:
# `0 = 1 - 1 = (P_f(A) + P_f(B) + P_f(C)) - (P_g(A) + P_g(B) + P_g(C))`, where
# `P_f` and `P_g` are probability measures of `f` and `g`;
# `C = {x | f(x) = g(x)}`.
# By definitions: `abs(P_f(A) - P_g(A)) = P_f(A) - P_g(A)`;
# `abs(P_f(B) - P_g(B)) = P_g(B) - P_f(B)`; `P_f(C) = P_g(C)`.
# Therefore: `abs(P_f(A) - P_g(A)) = abs(P_f(B) - P_g(B))`.
distance_totvar <- function(f, g) {
  d_f <- as_d(f)
  d_g <- as_d(g)

  num_fin <- (meta_type(f) == "fin") + (meta_type(g) == "fin")

  switch(
    as.character(num_fin),
    `0` = distance_totvar_two_infin(d_f, d_g),
    # A target set is all `x` values of "fin" pdqr-function. Its probability
    # under "fin" is 1 and under "infin" is zero because it is countable.
    `1` = 1,
    `2` = distance_totvar_two_fin(d_f, d_g)
  )
}

distance_totvar_two_infin <- function(d_f, d_g) {
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

distance_totvar_two_fin <- function(d_f, d_g) {
  union_x <- union_x(d_f, d_g)
  prob_diff <- d_f(union_x) - d_g(union_x)

  sum(prob_diff[prob_diff > 0])
}


# Method "wass" -----------------------------------------------------------
# **Notes in docs**. "Minimum cost of 'moving' one density into another". Here
# 1-Wasserstein distance is computed as integral of |F - G| over union support.
distance_wass <- function(f, g) {
  integrate_cdf_absdiff(p_f = as_p(f), p_g = as_p(g), power = 1)
}


# Method "cramer" ---------------------------------------------------------
# **Notes in docs**. Integral of (F - G)^2 over union support. Roughly, this
# is related to 1-Wasserstein distance in the same way as mean absolute
# deviation around the mean (absolute central first moment) is related to
# variance (central second moment).
distance_cramer <- function(f, g) {
  integrate_cdf_absdiff(p_f = as_p(f), p_g = as_p(g), power = 2)
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
  if ((meta_type(p_f) == "fin") && (meta_type(p_g) == "fin")) {
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
