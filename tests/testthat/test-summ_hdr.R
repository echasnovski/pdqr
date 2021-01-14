context("test-summ_hdr")


# Helper data -------------------------------------------------------------
empty_hdr <- data.frame(left = numeric(0), right = numeric(0))


# summ_hdr ----------------------------------------------------------------
test_that("summ_hdr works with 'discrete' functions", {
  # Skip check on "noLD" platform due to complexity of accuracy checking
  # Don't use `skip_if()` because otherwise CRAN doesn't accept submission
  if (is_noLD()) {
    expect_true(TRUE)
    return()
  }

  cur_d_1 <- new_d(data.frame(x = 1:4, prob = 1:4 / 10), "discrete")
  expect_equal(summ_hdr(cur_d_1, 0.1), data.frame(left = 4, right = 4))
  expect_equal(summ_hdr(cur_d_1, 0.4), data.frame(left = 4, right = 4))
  expect_equal(summ_hdr(cur_d_1, 0.4001), data.frame(left = 3, right = 4))
  expect_equal(summ_hdr(cur_d_1, 0.95), data.frame(left = 1, right = 4))

  cur_d_2 <- new_d(
    data.frame(x = 1:4, prob = c(0.4, 0.2, 0.3, 0.1)), "discrete"
  )
  expect_equal(summ_hdr(cur_d_2, 0.1), data.frame(left = 1, right = 1))
  expect_equal(
    summ_hdr(cur_d_2, 0.4001), data.frame(left = c(1, 3), right = c(1, 3))
  )
  expect_equal(summ_hdr(cur_d_2, 0.9), data.frame(left = 1, right = 3))
  expect_equal(summ_hdr(cur_d_2, 0.95), data.frame(left = 1, right = 4))
})

test_that("summ_hdr works with basic 'continuous' functions", {
  # Skip check on "noLD" platform due to complexity of accuracy checking
  # Don't use `skip_if()` because otherwise CRAN doesn't accept submission
  if (is_noLD()) {
    expect_true(TRUE)
    return()
  }

  # Here some edges of output aren't exact because computation of target height
  # isn't exact

  # Single mode at support edge
  cur_d_1 <- new_d(data.frame(x = c(0, 2), y = c(0, 1)), "continuous")

  hdr_1 <- summ_hdr(cur_d_1, level = 0.1)
  expect_equal(hdr_1[["left"]], as_q(cur_d_1)(0.9), tolerance = 1e-4)
  expect_equal(hdr_1[["right"]], 2)

  # Single mode in the middle
  cur_d_2 <- new_d(data.frame(x = 0:2, y = c(0, 1, 0)), "continuous")
  hdr_2_1 <- summ_hdr(cur_d_2, 0.4)
  left_q_1 <- as_q(cur_d_2)(0.3)
  expect_equal(hdr_2_1[["left"]], left_q_1, tolerance = 3e-4)
  expect_equal(hdr_2_1[["right"]], 2 - left_q_1, tolerance = 3e-4)

  expect_equal(summ_hdr(cur_d_2, 0.75), data.frame(left = 0.5, right = 1.5))

  # Several global modes
  cur_d_3 <- new_d(
    data.frame(x = 0:6, y = c(0, 1, 0, 1, 0, 1, 0) / 3), "continuous"
  )
  expect_equal(
    summ_hdr(cur_d_3, 0.75),
    data.frame(left = c(0.5, 2.5, 4.5), right = c(1.5, 3.5, 5.5))
  )

  # Several local modes
  cur_d_4 <- new_d(data.frame(x = 1:5, y = c(1, 0, 2, 0, 1) / 3), "continuous")
  ## Here target height is equal to density at local edge modes (at 1 and 5).
  ## The output should have only intervals of non-zero width, i.e. no `[1; 1]`
  ## and no `[5; 5]`.
  expect_equal(summ_hdr(cur_d_4, 0.5), data.frame(left = 2.5, right = 3.5))
})

test_that("summ_hdr works with extreme 'continuous' functions", {
  # Skip check on "noLD" platform due to complexity of accuracy checking
  # Don't use `skip_if()` because otherwise CRAN doesn't accept submission
  if (is_noLD()) {
    expect_true(TRUE)
    return()
  }

  # Wide zero probability interval. Tests adequacy of `compute_target_height()`
  zero_left <- new_d(
    data.frame(x = c(1, 100000 + 0:2), y = c(0, 0, 1, 0)), "continuous"
  )
  expect_equal(
    summ_hdr(zero_left, level = 0.75),
    data.frame(left = 100000.5, right = 100001.5)
  )

  zero_mid <- new_d(
    data.frame(x = c(1:3, 100000 + 0:2), y = c(0, 1, 0, 0, 1, 0)), "continuous"
  )
  expect_equal(
    summ_hdr(zero_mid, level = 0.75),
    data.frame(left = c(1.5, 100000.5), right = c(2.5, 100001.5))
  )

  zero_right <- new_d(
    data.frame(x = c(1:3, 100000), y = c(0, 1, 0, 0)), "continuous"
  )
  expect_equal(
    summ_hdr(zero_right, level = 0.75), data.frame(left = 1.5, right = 2.5)
  )

  # Infinite values in original density
  d_beta <- as_d(dbeta, shape1 = 0.5, shape2 = 0.5)
  left_q_beta <- as_q(d_beta)(0.3)
  hdr_beta <- summ_hdr(d_beta, 0.6)
  expect_equal(hdr_beta[["left"]], c(0, 1 - left_q_beta), tolerance = 2e-3)
  expect_equal(hdr_beta[["right"]], c(left_q_beta, 1), tolerance = 2e-3)
})

test_that("summ_hdr works with dirac-like functions", {
  # Type "discrete"
  d_dis_dirac <- new_d(100, "discrete")
  expect_equal(summ_hdr(d_dis_dirac, 0.1), data.frame(left = 100, right = 100))
  expect_equal(summ_hdr(d_dis_dirac, 0.9), data.frame(left = 100, right = 100))

  # Type "continuous"
  d_dirac_1 <- new_d(1, "continuous")
  expect_equal(
    summ_hdr(d_dirac_1, level = 0.99),
    data.frame(left = 1 - 1e-8, right = 1 + 1e-8),
    tolerance = 1e-9
  )

  d_dirac_2 <- form_mix(list(new_d(1, "continuous"), new_d(2, "continuous")))
  expect_equal(
    summ_hdr(d_dirac_2, level = 0.99),
    data.frame(left = 1:2 - 1e-8, right = 1:2 + 1e-8),
    tolerance = 1e-9
  )
})

test_that("summ_hdr works with winsorized 'continuous' functions", {
  # Generally, errors in HDRs of winsorized functions are big. Current
  # hypothesis is that this happens because of numerical precision issues at
  # dirac-like intervals.

  cur_d <- new_d(data.frame(x = 0:2, y = c(0, 1, 0)), "continuous")
  winsor_quan <- as_q(cur_d)(c(0.1, 0.9))
  d_winsor <- form_tails(
    cur_d, level = 0.1, method = "winsor", direction = "both"
  )
  p_winsor <- as_p(d_winsor)

  # Case when dirac-like intervals contain all desired probability
  expect_equal(
    summ_hdr(d_winsor, level = 0.05),
    data.frame(
      left = winsor_quan + c(0, -1e-8), right = winsor_quan + c(1e-8, 0)
    ),
    tolerance = 1e-7
  )

  # Case when output should have intervals other than dirac-like.
  # For more robustness total probability is tested
  hdr_winsor_2 <- summ_hdr(d_winsor, level = 0.9)
  tot_prob_2 <- sum(
    p_winsor(hdr_winsor_2[["right"]]) - p_winsor(hdr_winsor_2[["left"]])
  )
  expect_equal(tot_prob_2, 0.9, 8e-3)
})

test_that("summ_hdr handles plateaus", {
  # Plateaus at target height result into more total probability of output HDR
  # than requested

  d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")
  hdr_unif <- data.frame(left = 0, right = 1)
  expect_equal(summ_hdr(d_unif, 0.05), hdr_unif)
  expect_equal(summ_hdr(d_unif, 0.95), hdr_unif)

  d_plateau <- new_d(data.frame(x = 1:4, y = c(2, 1, 1, 2) / 4), "continuous")
  # Here target height is equal to 1/4, the plateau height. So the output is
  # equal to the whole support because there is no region with less total width
  # and total probability not less than 0.75.
  expect_equal(summ_hdr(d_plateau, 0.75), data.frame(left = 1, right = 4))
  # However, if level is slightly (with respect to accuracy of target height
  # computation) less than 0.75, then there are two intervals in output
  expect_equal(nrow(summ_hdr(d_plateau, 0.75 - 1e-3)), 2)
})

test_that("summ_hdr works with real world cases", {
  # Type "discrete"
  d_binom <- as_d(dbinom, size = 10, prob = 0.5)
  ref_hdr_quan <- qbinom(c(0.025, 0.975), size = 10, prob = 0.5)
  expect_equal(
    summ_hdr(d_binom, level = 0.95),
    data.frame(left = ref_hdr_quan[1], right = ref_hdr_quan[2])
  )

  # Type "continuous"
  d_norm <- as_d(dnorm, mean = 10, sd = 10)
  ref_hdr_quan <- qnorm(c(0.025, 0.975), mean = 10, sd = 10)
  hdr_norm <- summ_hdr(d_norm, level = 0.95)
  # Outputs are not exact because `d_norm` is a pdqr-approximation to input
  # normal distribution
  expect_equal(hdr_norm[["left"]], ref_hdr_quan[1], tolerance = 5e-3)
  expect_equal(hdr_norm[["right"]], ref_hdr_quan[2], tolerance = 5e-3)

  d_unif <- as_d(dunif)
  expect_equal(summ_hdr(d_unif, level = 0.95), data.frame(left = 0, right = 1))
})

test_that("summ_hdr accepts different pdqr classes", {
  ref_hdr <- summ_hdr(d_con, 0.95)
  expect_equal(summ_hdr(p_con, 0.95), ref_hdr)
  expect_equal(summ_hdr(q_con, 0.95), ref_hdr)
  expect_equal(summ_hdr(r_con, 0.95), ref_hdr)
})

test_that("summ_hdr works with special values of `level`", {
  # Type "discrete"
  d_dis_mode <- summ_mode(d_dis, method = "global")
  d_dis_supp <- meta_support(d_dis)
  expect_equal(
    summ_hdr(d_dis, 0), data.frame(left = d_dis_mode, right = d_dis_mode)
  )
  expect_equal(
    summ_hdr(d_dis, 1), data.frame(left = d_dis_supp[1], right = d_dis_supp[2])
  )

  # Type "continuous"
  d_con_mode <- summ_mode(d_con, method = "global")
  d_con_supp <- meta_support(d_con)
  expect_equal(
    summ_hdr(d_con, 0), data.frame(left = d_con_mode, right = d_con_mode)
  )
  expect_equal(
    summ_hdr(d_con, 1),
    data.frame(left = d_con_supp[1], right = d_con_supp[2])
  )
})

test_that("summ_hdr validates input", {
  expect_error(summ_hdr("a"), "`f`.*not pdqr-function")
  expect_error(summ_hdr(d_dis, level = "a"), "`level`.*number")
  expect_error(summ_hdr(d_dis, level = c(0.05, 0.1)), "`level`.*single")
  expect_error(summ_hdr(d_dis, level = 1.1), "`level`.*between 0 and 1")
  expect_error(summ_hdr(d_dis, level = -0.1), "`level`.*between 0 and 1")
})


# hdr_dis -----------------------------------------------------------------
# Tested in `summ_hdr()`


# hdr_con -----------------------------------------------------------------
# Tested in `summ_hdr()`


# cut_down_density --------------------------------------------------------
# Tested in `summ_hdr()`


# compute_target_height ---------------------------------------------------
# Tested in `summ_hdr()`


# grid_on_pos_prob --------------------------------------------------------
# Tested in `summ_hdr()`


# compute_hdr_intervals ---------------------------------------------------
test_that("compute_hdr_intervals works", {
  cur_d_1 <- new_d(data.frame(x = c(0, 2), y = c(0, 1)), "continuous")
  expect_equal(
    compute_hdr_intervals(cur_d_1, 0), data.frame(left = 0, right = 2)
  )
  expect_equal(
    compute_hdr_intervals(cur_d_1, 1 / 3), data.frame(left = 2 / 3, right = 2)
  )
  # Here empty HDR is returned because otherwise output would have zero width
  expect_equal(compute_hdr_intervals(cur_d_1, 1), empty_hdr)
  # Whole support is returned because its every point has density more than `-1`
  expect_equal(
    compute_hdr_intervals(cur_d_1, -1), data.frame(left = 0, right = 2)
  )
  expect_equal(compute_hdr_intervals(cur_d_1, 2), empty_hdr)

  cur_d_2 <- new_d(data.frame(x = 0:2, y = c(0, 1, 0)), "continuous")
  expect_equal(
    compute_hdr_intervals(cur_d_2, 0), data.frame(left = 0, right = 2)
  )
  expect_equal(
    compute_hdr_intervals(cur_d_2, 0.5), data.frame(left = 0.5, right = 1.5)
  )
  expect_equal(compute_hdr_intervals(cur_d_2, 1), empty_hdr)

  cur_d_3 <- new_d(
    data.frame(x = 0:6, y = c(0, 1, 0, 1, 0, 1, 0) / 3), "continuous"
  )
  # Whole support in single row is returned because consecutive intervals in
  # output rows should be "collapsed" to one. In other words, there should be
  # non-zero distance between consecutive intervals in HDR.
  expect_equal(
    compute_hdr_intervals(cur_d_3, 0), data.frame(left = 0, right = 6)
  )
  expect_equal(
    compute_hdr_intervals(cur_d_3, 1 / 6),
    data.frame(left = c(0.5, 2.5, 4.5), right = c(1.5, 3.5, 5.5))
  )
  expect_equal(compute_hdr_intervals(cur_d_3, 1 / 3), empty_hdr)


  cur_d_4 <- new_d(data.frame(x = 1:5, y = c(1, 0, 2, 0, 1) / 3), "continuous")
  # Although 1 and 5 also cross height `1/3`, they can only contribute a zero
  # width intervals to output
  expect_equal(
    compute_hdr_intervals(cur_d_4, 1 / 3), data.frame(left = 2.5, right = 3.5)
  )
})

test_that("compute_hdr_intervals works with dirac-like intervals", {
  d_dirac_1 <- new_d(1, "continuous")
  expect_equal(
    compute_hdr_intervals(d_dirac_1, 0.5e8),
    data.frame(left = 1 - 0.5e-8, right = 1 + 0.5e-8),
    tolerance = 1e-12
  )

  d_dirac_2 <- form_mix(list(new_d(1, "continuous"), new_d(2, "continuous")))
  expect_equal(
    compute_hdr_intervals(d_dirac_2, 0.25e8),
    data.frame(left = 1:2 - 0.5e-8, right = 1:2 + 0.5e-8),
    tolerance = 1e-12
  )

  d_winsor <- form_tails(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    level = 0.05, method = "winsor", direction = "both"
  )
  expect_equal(
    compute_hdr_intervals(d_winsor, 1.2),
    data.frame(left = c(0.05, 0.95 - 1e-8), right = c(0.05 + 1e-8, 0.95)),
    tolerance = 1e-12
  )
})

test_that("compute_hdr_intervals handles plateaus", {
  cur_d_1 <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")
  expect_equal(
    compute_hdr_intervals(cur_d_1, 1), data.frame(left = 0, right = 1)
  )

  cur_d_2 <- new_d(
    data.frame(x = c(0, 1, 10, 11), y = c(1, 0, 0, 1)), "continuous"
  )
  expect_equal(
    compute_hdr_intervals(cur_d_2, 0), data.frame(left = 0, right = 11)
  )

  cur_d_3 <- new_d(
    data.frame(
      x = c(0, 1, 1.5, 2, 3, 4, 4.2, 5, 6), y = c(0, 1, 1, 1, 0, 1, 1, 1, 0) / 4
    ),
    "continuous"
  )
  # Only left and right edges of plateaus (that may stretch over several
  # consecutive intervals) should be returned.
  expect_equal(
    compute_hdr_intervals(cur_d_3, 1 / 4),
    data.frame(left = c(1, 4), right = c(2, 5))
  )
})


# compute_density_height_points -------------------------------------------
test_that("compute_density_height_points works", {
  cur_d_1 <- new_d(data.frame(x = c(0, 2), y = c(0, 1)), "continuous")
  expect_equal(compute_density_height_points(cur_d_1, 0), 0)
  expect_equal(compute_density_height_points(cur_d_1, 1 / 3), 2 / 3)
  expect_equal(compute_density_height_points(cur_d_1, 1), 2)
  expect_equal(compute_density_height_points(cur_d_1, -1), numeric(0))
  expect_equal(compute_density_height_points(cur_d_1, 2), numeric(0))

  cur_d_2 <- new_d(data.frame(x = 0:2, y = c(0, 1, 0)), "continuous")
  expect_equal(compute_density_height_points(cur_d_2, 0), c(0, 2))
  expect_equal(compute_density_height_points(cur_d_2, 0.5), 0.5 + 0:1)
  expect_equal(compute_density_height_points(cur_d_2, 1), 1)

  cur_d_3 <- new_d(
    data.frame(x = 0:6, y = c(0, 1, 0, 1, 0, 1, 0) / 3), "continuous"
  )
  expect_equal(compute_density_height_points(cur_d_3, 0), c(0, 2, 4, 6))
  expect_equal(compute_density_height_points(cur_d_3, 1 / 6), 0.5 + 0:5)
  expect_equal(compute_density_height_points(cur_d_3, 1 / 3), c(1, 3, 5))

  cur_d_4 <- new_d(data.frame(x = 1:5, y = c(1, 0, 2, 0, 1) / 3), "continuous")
  expect_equal(compute_density_height_points(cur_d_4, 1 / 3), c(1, 2.5, 3.5, 5))
})

test_that("compute_density_height_points works with dirac-like intervals", {
  d_dirac_1 <- new_d(1, "continuous")
  expect_equal(
    compute_density_height_points(d_dirac_1, 0.5e8), 1 + c(-1, 1) * 0.5e-8,
    tolerance = 1e-12
  )

  d_dirac_2 <- form_mix(list(new_d(1, "continuous"), new_d(2, "continuous")))
  expect_equal(
    compute_density_height_points(d_dirac_2, 0.25e8),
    c(1 + c(-1, 1) * 0.5e-8, 2 + c(-1, 1) * 0.5e-8),
    tolerance = 1e-12
  )

  d_winsor <- form_tails(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    level = 0.05, method = "winsor", direction = "both"
  )
  expect_equal(
    compute_density_height_points(d_winsor, 1.2), c(0.05, 0.95),
    tolerance = 1e-8
  )
})

test_that("compute_density_height_points handles plateaus", {
  # Only left and right edges of plateaus (that may stretch over several
  # consecutive intervals) should be returned.
  cur_d_1 <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")
  expect_equal(compute_density_height_points(cur_d_1, 1), 0:1)

  cur_d_2 <- new_d(
    data.frame(x = c(0, 1, 10, 11), y = c(1, 0, 0, 1)), "continuous"
  )
  expect_equal(compute_density_height_points(cur_d_2, 0), c(1, 10))

  cur_d_3 <- new_d(
    data.frame(
      x = c(0, 1, 1.5, 2, 3, 4, 4.2, 5, 6), y = c(0, 1, 1, 1, 0, 1, 1, 1, 0) / 4
    ),
    "continuous"
  )
  expect_equal(compute_density_height_points(cur_d_3, 1 / 4), c(1, 2, 4, 5))
})
