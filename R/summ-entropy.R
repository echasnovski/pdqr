# Note in docs that pdqr approximation error can introduce rather big error in
# entropy estimation in case original density goes to infinity
summ_entropy <- function(f) {
  assert_pdqr_fun(f)

  cross_entropy(f, f)
}

# Note in docs that `method = "relative"` is Kullback–Leibler divergence
summ_entropy2 <- function(f, g, method = "relative", clip = exp(-20)) {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(method, is_string)
  assert_in_set(method, c("relative", "cross"))
  assert_type(clip, is_single_number, "single non-negative number", min_val = 0)

  switch(
    method,
    relative = cross_entropy(f, g, clip) - cross_entropy(f, f, clip),
    cross = cross_entropy(f, g, clip)
  )
}

cross_entropy <- function(f, g, clip = exp(-20)) {
  d_f <- as_d(f)
  d_g <- as_d(g)

  num_infin <- (meta_type(f) == "infin") + (meta_type(g) == "infin")

  switch(
    as.character(num_infin),
    `0` = cross_entropy_fin(d_f, d_g, clip),
    `1` = stop_collapse("`f` and `g` should have the same type."),
    `2` = cross_entropy_infin(d_f, d_g, clip)
  )
}

cross_entropy_fin <- function(d_f, d_g, clip = exp(-20)) {
  x <- meta_x_tbl(d_f)[["x"]]

  prob_g <- pmax(d_g(x), clip)

  -sum(d_f(x) * log(prob_g))
}

cross_entropy_infin <- function(d_f, d_g, clip) {
  # Entropy will be computed over `d_f`'s support. However, computation is non
  # trivial only on common support. Influence of intervals from exactly one
  # support will be used at the end (`outside_entropy`).
  comm_x <- common_x(d_f, d_g)

  # Handling case of no common support. In that case entropy is equal to
  # `-integral{d_f(x) * log(clip)}dx` over `d_f`'s support (`clip` is constant)
  if (length(comm_x) <= 1) {
    return(-log(clip))
  }

  # Entropy on common support is computed as sum of piece integrals on intervals
  # between consecutive `comm_x` elements. There integrals of interest have form
  # `-integral{(a*x+b) * log(A*x+B)}dx` (`a`, `A` - slopes and `b`, `B` -
  # intercepts of `d_f` and `d_g`). The way this integral is computed depends on
  # whether `A` is zero. If yes, then it is straightforward integral because
  # `log(B)` is a constant; if not - a complicated one.

  # Prepare data for integrals
  n <- length(comm_x)
  comm_left <- comm_x[-n]
  comm_right <- comm_x[-1]
  d <- comm_right - comm_left

  comm_mid <- (comm_left + comm_right)/2
  f_x_tbl <- meta_x_tbl(d_f)
  sl_f <- compute_piecelin_density_coeffs(
    x_tbl = f_x_tbl, ind_vec = findInterval(comm_mid, f_x_tbl[["x"]])
  )[["slope"]]
  g_x_tbl <- meta_x_tbl(d_g)
  sl_g <- compute_piecelin_density_coeffs(
    x_tbl = g_x_tbl, ind_vec = findInterval(comm_mid, g_x_tbl[["x"]])
  )[["slope"]]

  f_y_l <- d_f(comm_x)[-n]

  g_y <- d_g(comm_x)
  g_y_l <- g_y[-n]
  g_y_l_clipped <- pmax(g_y_l, clip)
  g_y_r <- g_y[-1]
  g_y_r_clipped <- pmax(g_y_r, clip)

  # Compute integrals. Here using `g_y_r_clipped` instead of `g_y_r` ensures
  # that no `NaN` happens because of `log(0)` (they can happen, though, if
  # `sl_g == 0`).
  piece_entropy_with_log <- -(
    log(g_y_r_clipped) *
      (0.5*sl_f*d^2 + d*f_y_l + f_y_l*g_y_l/sl_g - 0.5*sl_f*(g_y_l/sl_g)^2) +
      log(g_y_l_clipped) *
      (-f_y_l*g_y_l/sl_g + 0.5*sl_f*(g_y_l/sl_g)^2) +
      0.5*sl_f*d*g_y_l / sl_g - 0.25*sl_f*d^2 - d*f_y_l
  )
  piece_entropy_no_log <- -log(g_y_l_clipped) * (0.5*sl_f*d^2 + f_y_l*d)

  piece_entropy <- numeric(n - 1)
  int_has_log <- sl_g != 0
  piece_entropy[int_has_log] <- piece_entropy_with_log[int_has_log]
  piece_entropy[!int_has_log] <- piece_entropy_no_log[!int_has_log]

  common_entropy <- sum(piece_entropy)

  # Handling outside of common support. There either `d_f` is zero (then
  # probabilities will be zero) or `d_g` is zero (then it will be "replaced"
  # with constant `clip` value and integral will be just `d_f`'s probability of
  # outside region multiplied by `-log(clip)`.
  f_supp <- meta_support(d_f)
  p_f <- as_p(d_f)
  outside_entropy <- -log(clip) * (
    (p_f(comm_x[1]) - p_f(f_supp[1])) + (p_f(f_supp[2]) - p_f(comm_x[n]))
  )

  # Using `sum(*, na.rm = TRUE)` to account for possible `NaN`
  sum(common_entropy, outside_entropy, na.rm = TRUE)
}
