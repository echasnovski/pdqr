#' Summarize distributions with separation threshold
#'
#' Compute for pair of pdqr-functions the optimal threshold that separates
#' distributions they represent. In other words, `summ_separation()` solves a
#' binary classification problem with one-dimensional linear classifier: values
#' not more than some threshold are classified as one class, and more than
#' threshold - as another. Order of input functions doesn't matter.
#'
#' @param f A pdqr-function of any [type][meta_type()] and
#'   [class][meta_class()]. Represents "true" distribution of "negative" values.
#' @param g A pdqr-function of any type and class. Represents "true"
#'   distribution of "positive" values.
#' @param method Separation method. Should be one of "KS" (Kolmogorov-Smirnov),
#'   "GM", "OP", "F1", "MCC" (all four are methods for computing classification
#'   metric in [summ_classmetric()]).
#' @param n_grid Number of grid points to be used during optimization.
#'
#' @details All methods:
#' - Return middle point of nearest support edges in case of non-overlapping or
#' "touching" supports of `f` and `g`.
#' - Return the smallest optimal solution in case of several candidates.
#'
#' Method "KS" computes "x" value at which corresponding p-functions of `f` and
#' `g` achieve supremum of their absolute difference (so input order of `f` and
#' `g` doesn't matter). If input pdqr-functions have the same
#' [type][meta_type()], then result is a point of maximum absolute difference.
#' If inputs have different types, then absolute difference of p-functions at
#' the result point can be not the biggest. In that case output represents a
#' left limit of points at which target supremum is reached (see Examples).
#'
#' Methods "GM", "OP", "F1", "MCC" compute threshold which maximizes
#' corresponding [classification metric][summ_classmetric()] for best suited
#' classification setup. They evaluate metrics at equidistant grid (with
#' `n_grid` elements) for both directions (`summ_classmetric(f, g, *)` and
#' `summ_classmetric(g, f, *)`) and return threshold which results into maximum
#' of both setups. **Note** that other `summ_classmetric()` methods are either
#' useless here (always return one of the edges) or are equivalent to ones
#' already present.
#'
#' @return A single number representing optimal separation threshold.
#'
#' @seealso [summ_roc()] for computing ROC curve related summaries.
#'
#' [summ_classmetric()] for computing of classification metric for ordered
#' classification setup.
#'
#' @family summary functions
#'
#' @examples
#' d_norm_1 <- as_d(dnorm)
#' d_unif <- as_d(dunif)
#' summ_separation(d_norm_1, d_unif, method = "KS")
#' summ_separation(d_norm_1, d_unif, method = "OP")
#'
#' # Mixed types for "KS" method
#' p_dis <- new_p(1, "discrete")
#' p_unif <- as_p(punif)
#' thres <- summ_separation(p_dis, p_unif)
#' abs(p_dis(thres) - p_unif(thres))
#'   # Actual difference at `thres` is 0. However, supremum (equal to 1) as
#'   # limit value is # reached there.
#' x_grid <- seq(0, 1, by = 1e-3)
#' plot(x_grid, abs(p_dis(x_grid) - p_unif(x_grid)), type = "b")
#'
#' # Handling of non-overlapping supports
#' summ_separation(new_d(2, "discrete"), new_d(3, "discrete"))
#'
#' # The smallest "x" value is returned in case of several optimal thresholds
#' summ_separation(d_norm_1, d_norm_1) == meta_support(d_norm_1)[1]
#'
#' @export
summ_separation <- function(f, g, method = "KS", n_grid = 10001) {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(method, is_string)
  assert_in_set(method, c("KS", "GM", "OP", "F1", "MCC"))
  assert_type(n_grid, is_single_number, type_name = "single number")

  # Early returns in cases of non-overlapping supports
  f_supp <- meta_support(f)
  g_supp <- meta_support(g)

  if (g_supp[1] >= f_supp[2]) {
    return((g_supp[1] + f_supp[2]) / 2)
  }
  if (f_supp[1] >= g_supp[2]) {
    return((f_supp[1] + g_supp[2]) / 2)
  }

  # Main cases
  switch(
    method,
    KS = separation_ks(f, g),
    separation_classmetric(f, g, method, n_grid)
  )
}

separation_ks <- function(f, g) {
  p_f <- as_p(f)
  p_g <- as_p(g)

  f_type <- meta_type(f)
  g_type <- meta_type(g)

  if (f_type == "discrete") {
    if (g_type == "discrete") {
      separation_ks_two_dis(p_f, p_g)
    } else {
      separation_ks_mixed(p_dis = p_f, p_con = p_g)
    }
  } else {
    if (g_type == "discrete") {
      separation_ks_mixed(p_dis = p_g, p_con = p_f)
    } else {
      separation_ks_two_con(p_f, p_g)
    }
  }
}

separation_ks_two_dis <- function(p_f, p_g) {
  x_test <- union_x(p_f, p_g)
  max_ind <- which.max(abs(p_f(x_test) - p_g(x_test)))

  x_test[max_ind]
}

separation_ks_mixed <- function(p_dis, p_con) {
  # Supremum of |F - G| can be found only by inspecting "x" elements of
  # "discrete" pdqr-function. However, it can be also located on one of "x"
  # elements of "continuous" pdqr-function, and this fact should be accounted
  # for, because of obligation to return 'the smallest "x" value on which
  # supremum of |F-G| is located'.
  dis_test <- meta_x_tbl(p_dis)[["x"]]

  p_con_cumprob <- p_con(dis_test)

  p_dis_cumprob <- meta_x_tbl(p_dis)[["cumprob"]]
  p_dis_left_cumprob <- c(0, p_dis_cumprob[-length(p_dis_cumprob)])

  # Using `alternate()` here to ensure that the smallest "x" value is returned
  # if there are several candidates.
  # Also, `round()` is used to ensure that numerical representation issues don't
  # affect the output (especially in case of several output candidates and the
  # need to return the smallest one)
  cdf_absdiff_dis <- round(alternate(
    abs(p_con_cumprob - p_dis_cumprob),
    # Testing against "left cumulative probabilities" (which are left limits
    # of "discrete" type CDF at points of discontinuity) is needed because K-S
    # distance is a **supremum** of absolute differences between two CDFs. It
    # is a way to account for discontinuity. Consider example in which K-S
    # distance should be equal to 1 but without using "left cumprob" it is
    # equal to 0:
    #   f <- new_p(data.frame(x = 0:1, y = c(1, 1)), "continuous")
    #   g <- new_p(1, "discrete")
    abs(p_con_cumprob - p_dis_left_cumprob)
  ), digits = 12)

  # Compute result `dis_test` element taking into account their "double usage"
  # in `cdf_absdiff`
  max_ind_dis <- which.max(cdf_absdiff_dis)
  sep_dis <- dis_test[ceiling(max_ind_dis / 2)]
  sep_dis_absdiff <- cdf_absdiff_dis[max_ind_dis]

  # Take into account CDF values at "x" elements of "continuous" pdqr-function
  con_x_tbl <- meta_x_tbl(p_con)
  con_x <- con_x_tbl[["x"]]
    # No need taking into account "left limits" because if they matter, they are
    # already accounted for in previous step
  cdf_absdiff_con <- round(
    abs(con_x_tbl[["cumprob"]] - p_dis(con_x)),
    digits = 12
  )

  max_ind_con <- which.max(cdf_absdiff_con)
  sep_con <- con_x[max_ind_con]
  sep_con_absdiff <- cdf_absdiff_con[max_ind_con]

  if (sep_con_absdiff >= sep_dis_absdiff) {
    min(sep_dis, sep_con)
  } else {
    sep_dis
  }
}

separation_ks_two_con <- function(p_f, p_g) {
  # In extremum points of `|F(x) - G(x)|` its derivative should be zero. So,
  # `sign(F(x) - G(x)) * (f(x) - g(x))` (`f` and `g` are derivatives of CDF, i.e
  # density functions) should be equal to zero. If `sign(*) = 0` then K-S
  # distance is zero which is (non-interesting) minimum. So the big part of `x`
  # candidates are the coordinates of density crossings (but not all of them).
  # They can be found straightforwardly because `f` and `g` are linear inside
  # common linearity intervals.
  x_inters <- compute_density_crossings(p_f, p_g)

  # As this is an optimization problem with constraints (x should be inside
  # union of supports), optimal values can be also located on constraint
  # boundaries.
  # Also not to mention the case when densities can have common support
  # and no intersections (think about couple of different uniform
  # distributions).
  # Here `sort()` is needed to ensure that the smallest "x" value is returned
  # in case several points have maximum difference of CDFs
  x_test <- sort(c(meta_support(p_f), meta_support(p_g), x_inters))

  max_ind <- which.max(abs(p_f(x_test) - p_g(x_test)))

  x_test[max_ind]
}

#' **Notes** about method choice:
#' - "Acc" and "YI"  are equivalent to "KS" separation in terms of output.
#' - Minimizing "ER" is equivalent to maximizing "Acc", which is equivalent to
#' "KS" separation.
#' - "DOR" and "Simple" metrics are useless because always return edge of some
#' support.
#' - "Jaccard" seems to be equivalent to "F1" (although, not exactly sure why).
#' - "MK" seems to demonstrate poor fit to the "separation" task.
#'
#' @noRd
separation_classmetric <- function(f, g, method, n_grid = 10001) {
  t_grid <- seq_between(union_support(f, g), length.out = n_grid)

  p_f_t <- as_p(f)(t_grid)
  p_g_t <- as_p(g)(t_grid)
  classmetric_vals_1 <- classmetric(p_f_t, p_g_t, method = method)
  classmetric_vals_2 <- classmetric(p_g_t, p_f_t, method = method)

  # `alternate()` is used to ensure that the smallest value is returned in case
  # of several alternatives
  metric <- alternate(classmetric_vals_1, classmetric_vals_2)

  target_ind <- which.max(metric)

  t_grid[ceiling(target_ind / 2)]
}
