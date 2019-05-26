#' Summarize distributions with separation threshold
#'
#' Compute for pair of pdqr-functions the optimal threshold that separates
#' distributions they represent. In other words, `summ_separation()` solves a
#' classification problem with two classes (represented by pdqr-functions) and
#' one-dimensional linear classifier: values not more than some threshold are
#' classified as "negative", and more than threshold - as "positive".
#'
#' @param f A pdqr-function of any [type][meta_type()] and
#'   [class][meta_class()]. Represents "true" distribution of "negative" values.
#' @param g A pdqr-function of any type and class. Represents "true"
#'   distribution of "positive" values.
#' @param method Separation method. Should be one of "KS" (Kolmogorov-Smirnov).
#'
#' @details All methods in case of several optimal solutions return the smallest
#' one.
#'
#' Method "KS" computes "x" value at which corresponding p-functions of `f` and
#' `g` achieve supremum of their absolute difference (so input order of `f` and
#' `g` doesn't matter). If input pdqr-functions have the same
#' [type][meta_type()], then result is a point of maximum absolute difference.
#' If inputs have different types, then absolute difference of p-functions at
#' the result point can be not the biggest. In that case output represents a
#' left limit of points at which target supremum is reached (see Examples).
#'
#' @return A single number representing optimal separation threshold.
#'
#' @family summary functions
#'
#' @examples
#' d_norm_1 <- as_d(dnorm)
#' d_norm_2 <- as_d(dnorm, mean = 2)
#' d_pois <- as_d(dpois, lambda = 2)
#' summ_separation(d_norm_1, d_norm_2, method = "KS")
#' summ_separation(d_norm_2, d_pois, method = "KS")
#'
#' # Mixed types for "KS" method
#' p_fin <- new_p(1, "fin")
#' p_unif <- as_p(punif)
#' thres <- summ_separation(p_fin, p_unif)
#' abs(p_fin(thres) - p_unif(thres))
#'   # Actual difference at `thres` is 0. However, supremum (equal to 1) as
#'   # limit value is # reached there.
#' x_grid <- seq(0, 1, by = 1e-3)
#' plot(x_grid, abs(p_fin(x_grid) - p_unif(x_grid)), type = "b")
#'
#' # The smallest "x" value is returned in case of several optimal thresholds
#' summ_separation(d_norm_1, d_norm_1) == meta_support(d_norm_1)[1]
#'
#' @export
summ_separation <- function(f, g, method = "KS") {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(method, is_string)
  assert_in_set(method, "KS")

  switch(
    method,
    KS = separation_ks(f, g)
  )
}

separation_ks <- function(f, g) {
  # Early returns in cases of non-overlapping supports
  f_supp <- meta_support(f)
  g_supp <- meta_support(g)

  if (f_supp[1] > g_supp[2]) {
    return((f_supp[1] + g_supp[2]) / 2)
  }
  if (g_supp[1] > f_supp[2]) {
    return((g_supp[1] + f_supp[2]) / 2)
  }

  # Main case
  p_f <- as_p(f)
  p_g <- as_p(g)

  f_type <- meta_type(f)
  g_type <- meta_type(g)

  if (f_type == "fin") {
    if (g_type == "fin") {
      separation_ks_two_fin(p_f, p_g)
    } else {
      separation_ks_mixed(p_fin = p_f, p_infin = p_g)
    }
  } else {
    if (g_type == "fin") {
      separation_ks_mixed(p_fin = p_g, p_infin = p_f)
    } else {
      separation_ks_two_infin(p_f, p_g)
    }
  }
}

separation_ks_two_fin <- function(p_f, p_g) {
  x_test <- union_x(p_f, p_g)
  max_ind <- which.max(abs(p_f(x_test) - p_g(x_test)))

  x_test[max_ind]
}

separation_ks_mixed <- function(p_fin, p_infin) {
  # Supremum of |F - G| can be found only by inspecting "x" elements of "fin"
  # pdqr-function. However, it can be also located on one of "x" elements of
  # "infin" pdqr-function, and this fact should be accounted for, because of
  # obligation to return 'the smallest "x" value on which supremum of |F-G| is
  # located'.
  fin_test <- meta_x_tbl(p_fin)[["x"]]

  p_infin_cumprob <- p_infin(fin_test)

  p_fin_cumprob <- meta_x_tbl(p_fin)[["cumprob"]]
  p_fin_left_cumprob <- c(0, p_fin_cumprob[-length(p_fin_cumprob)])

  # Using `alternate()` here to ensure that the smallest "x" value is returned
  # if there are several candidates.
  # Also, `round()` is used to ensure that numerical representation issues don't
  # affect the output (especially in case of several output candidates and the
  # need to return the smallest one)
  cdf_absdiff_fin <- round(alternate(
    abs(p_infin_cumprob - p_fin_cumprob),
    # Testing against "left cumulative probabilities" (which are left limits
    # of "fin" type CDF at points of discontinuity) is needed because K-S
    # distance is a **supremum** of absolute differences between two CDFs. It
    # is a way to account for discontinuity. Consider example in which K-S
    # distance should be equal to 1 but without using "left cumprob" it is
    # equal to 0:
    #   f <- new_p(data.frame(x = 0:1, y = c(1, 1)), "infin")
    #   g <- new_p(1, "fin")
    abs(p_infin_cumprob - p_fin_left_cumprob)
  ), digits = 12)

  # Compute result `fin_test` element taking into account their "double usage"
  # in `cdf_absdiff`
  max_ind_fin <- which.max(cdf_absdiff_fin)
  sep_fin <- fin_test[ceiling(max_ind_fin / 2)]
  sep_fin_absdiff <- cdf_absdiff_fin[max_ind_fin]

  # Take into account CDF values at "x" elements of "infin" pdqr-function
  infin_x_tbl <- meta_x_tbl(p_infin)
  infin_x <- infin_x_tbl[["x"]]
    # No need taking into account "left limits" because if they matter, they are
    # already accounted for in previous step
  cdf_absdiff_infin <- round(
    abs(infin_x_tbl[["cumprob"]] - p_fin(infin_x)),
    digits = 12
  )

  max_ind_infin <- which.max(cdf_absdiff_infin)
  sep_infin <- infin_x[max_ind_infin]
  sep_infin_absdiff <- cdf_absdiff_infin[max_ind_infin]

  if (sep_infin_absdiff >= sep_fin_absdiff) {
    min(sep_fin, sep_infin)
  } else {
    sep_fin
  }
}

separation_ks_two_infin <- function(p_f, p_g) {
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
