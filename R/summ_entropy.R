#' Summarize distribution with entropy
#'
#' `summ_entropy()` computes entropy of single distribution while
#' `summ_entropy2()` - for a pair of distributions. For "discrete"
#' pdqr-functions a classic formula `-sum(p * log(p))` (in nats) is used. In
#' "continuous" case a differential entropy is computed.
#'
#' @inheritParams summ_mean
#' @param g A pdqr-function. Should be the same type as `f`.
#' @param method Entropy method for pair of distributions. One of "relative"
#'   (Kullback–Leibler divergence) or "cross" (for cross-entropy).
#' @param clip Value to be used instead of 0 during `log()` computation.
#'   `-log(clip)` represents the maximum value of output entropy.
#'
#' @details **Note** that due to [pdqr approximation error][pdqr_approx_error()]
#' there can be a rather big error in entropy estimation in case original
#' density goes to infinity.
#'
#' @return A single number representing entropy. If `clip` is strictly positive,
#' then it will be finite.
#'
#' @family summary functions
#'
#' @examples
#' d_norm <- as_d(dnorm)
#' d_norm_2 <- as_d(dnorm, mean = 2, sd = 0.5)
#'
#' summ_entropy(d_norm)
#' summ_entropy2(d_norm, d_norm_2)
#' summ_entropy2(d_norm, d_norm_2, method = "cross")
#'
#' # Increasing `clip` leads to decreasing maximum output value
#' d_1 <- new_d(1:10, "discrete")
#' d_2 <- new_d(20:21, "discrete")
#'
#' ## Formally, output isn't clearly defined because functions don't have the
#' ## same support. Direct use of entropy formulas gives infinity output, but
#' ## here maximum value is `-log(clip)`.
#' summ_entropy2(d_1, d_2, method = "cross")
#' summ_entropy2(d_1, d_2, method = "cross", clip = exp(-10))
#' summ_entropy2(d_1, d_2, method = "cross", clip = 0)
#' @name summ_entropy
NULL

#' @rdname summ_entropy
#' @export
summ_entropy <- function(f) {
  assert_pdqr_fun(f)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  cross_entropy(f, f)
}

#' @rdname summ_entropy
#' @export
summ_entropy2 <- function(f, g, method = "relative", clip = exp(-20)) {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_method(method, methods_entropy2)
  assert_type(clip, is_single_number, "single non-negative number", min_val = 0)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  switch(
    method,
    relative = cross_entropy(f, g, clip) - cross_entropy(f, f, clip),
    cross = cross_entropy(f, g, clip)
  )
}

methods_entropy2 <- c("relative", "cross")

cross_entropy <- function(f, g, clip = exp(-20)) {
  d_f <- as_d(f)
  d_g <- as_d(g)

  num_con <- (meta_type(f) == "continuous") + (meta_type(g) == "continuous")

  switch(
    as.character(num_con),
    `0` = cross_entropy_dis(d_f, d_g, clip),
    `1` = stop_collapse("`f` and `g` should have the same type."),
    `2` = cross_entropy_con(d_f, d_g, clip)
  )
}

cross_entropy_dis <- function(d_f, d_g, clip = exp(-20)) {
  x <- meta_x_tbl(d_f)[["x"]]

  prob_g <- pmax(d_g(x), clip)

  -sum(d_f(x) * log(prob_g))
}

# Not using `stats::integrate()` here because of possible dirac-like intervals
# in `d_f` and/or `d_g`. Except these cases, `integrate()` works pretty good
# considering both numerical and speed reasons.
cross_entropy_con <- function(d_f, d_g, clip) {
  # Entropy will be computed over `d_f`'s support. However, computation is non
  # trivial only on intersection support. Influence of intervals from exactly
  # one support will be used at the end (`outside_entropy`).
  inters_x <- intersection_x(d_f, d_g)

  # Handling case of no intersection support. In that case entropy is equal to
  # `-integral{d_f(x) * log(clip)}dx` over `d_f`'s support (`clip` is constant)
  if (length(inters_x) <= 1) {
    return(-log(clip))
  }

  # Entropy on intersection support is computed as sum of piece integrals on
  # intervals between consecutive `inters_x` elements. There integrals of
  # interest have form `-integral{(a*x+b) * log(A*x+B)}dx` (`a`, `A` - slopes
  # and `b`, `B` - intercepts of `d_f` and `d_g`). The way this integral is
  # computed depends on whether `A` is zero. If yes, then it is straightforward
  # integral because `log(B)` is a constant; if not - a complicated one.

  # Prepare data for integrals
  n <- length(inters_x)
  inters_left <- inters_x[-n]
  inters_right <- inters_x[-1]
  d <- inters_right - inters_left

  inters_mid <- (inters_left + inters_right) / 2
  f_x_tbl <- meta_x_tbl(d_f)
  sl_f <- compute_piecelin_density_coeffs(
    x_tbl = f_x_tbl, ind_vec = findInterval(inters_mid, f_x_tbl[["x"]])
  )[["slope"]]
  g_x_tbl <- meta_x_tbl(d_g)
  sl_g <- compute_piecelin_density_coeffs(
    x_tbl = g_x_tbl, ind_vec = findInterval(inters_mid, g_x_tbl[["x"]])
  )[["slope"]]

  f_y_l <- d_f(inters_x)[-n]

  g_y <- d_g(inters_x)
  g_y_l <- g_y[-n]
  g_y_l_clipped <- pmax(g_y_l, clip)
  g_y_r <- g_y[-1]
  g_y_r_clipped <- pmax(g_y_r, clip)

  # Compute integrals. Here using `g_y_r_clipped` instead of `g_y_r` ensures
  # that no `NaN` happens because of `log(0)` (they can happen, though, if
  # `sl_g == 0`).
  # styler: off
  piece_entropy_with_log <- -(
    log(g_y_r_clipped) *
      (0.5*sl_f*d^2 + d*f_y_l + f_y_l*g_y_l/sl_g - 0.5*sl_f*(g_y_l/sl_g)^2) +
      log(g_y_l_clipped) *
      (-f_y_l*g_y_l/sl_g + 0.5*sl_f*(g_y_l/sl_g)^2) +
      0.5*sl_f*d*g_y_l / sl_g - 0.25*sl_f*d^2 - d*f_y_l
    # styler: on
  )
  piece_entropy_no_log <- -log(g_y_l_clipped) * (0.5 * sl_f * d^2 + f_y_l * d)

  piece_entropy <- numeric(n - 1)
  # Usage of `is_zero()` instead of `sl_g != 0` is crucial as latter introduced
  # severe errors because `sl_g` could end up being ~1e-17 because of numeric
  # representation issues
  int_has_log <- !is_zero(sl_g)
  piece_entropy[int_has_log] <- piece_entropy_with_log[int_has_log]
  piece_entropy[!int_has_log] <- piece_entropy_no_log[!int_has_log]

  inters_entropy <- sum(piece_entropy)

  # Handling outside of intersection support. There either `d_f` is zero (then
  # probabilities will be zero) or `d_g` is zero (then it will be "replaced"
  # with constant `clip` value and integral will be just `d_f`'s probability of
  # outside region multiplied by `-log(clip)`.
  f_supp <- meta_support(d_f)
  p_f <- as_p(d_f)
  outside_entropy <- -log(clip) * (
    (p_f(inters_x[1]) - p_f(f_supp[1])) + (p_f(f_supp[2]) - p_f(inters_x[n]))
  )

  # Using `sum(*, na.rm = TRUE)` to account for possible `NaN`
  sum(inters_entropy, outside_entropy, na.rm = TRUE)
}
