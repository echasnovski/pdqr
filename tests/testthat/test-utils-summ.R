context("test-utils-summ")


# Custom expectations -----------------------------------------------------
expect_raw_moment_works <- function(stat_data, thres_1 = 1e-6, thres_2 = 1e-6) {
  cur_d <- stat_data[["d_fun"]]
  cur_mean <- stat_data[["mean"]]

  expect_equal(raw_moment(cur_d, 1), cur_mean, tolerance = thres_1)
  expect_equal(
    raw_moment(cur_d, 2), stat_data[["var"]] + cur_mean^2,
    tolerance = thres_2
  )
}


# raw_moment --------------------------------------------------------------
test_that("raw_moment works with 'fin' functions", {
  # Small thresholds because in case of finite support moments should be exact
  expect_raw_moment_works(
    stat_list[["binom"]], thres_1 = 1e-12, thres_2 = 1e-12
  )
  # Here moments aren't exact because tail trimming is done during `as_d()`
  expect_raw_moment_works(stat_list[["pois"]], thres_1 = 1e-5, thres_2 = 1e-5)
})

test_that("raw_moment works with common 'infin' functions", {
  expect_raw_moment_works(stat_list[["beta"]])
  # Big thresholds because original density goes to infinity at edges
  expect_raw_moment_works(
    stat_list[["beta_inf"]], thres_1 = 2e-2, thres_2 = 2e-2
  )
  expect_raw_moment_works(stat_list[["chisq"]], thres_1 = 3e-5, thres_2 = 1e-3)
  # Big thresholds because original density goes to infinity at left edge
  expect_raw_moment_works(
    stat_list[["chisq_inf"]], thres_1 = 2e-2, thres_2 = 5e-2
  )
  expect_raw_moment_works(stat_list[["exp"]], thres_1 = 2e-5, thres_2 = 3e-4)
  expect_raw_moment_works(stat_list[["norm"]], thres_2 = 5e-5)
  expect_raw_moment_works(stat_list[["norm_2"]])
  expect_raw_moment_works(stat_list[["unif"]])
})

test_that("raw_moment works with dirac-like 'infin' functions", {
  d_dirac <- new_d(2, "infin")
  expect_equal(raw_moment(d_dirac, 1), 2)
  expect_equal(raw_moment(d_dirac, 2), 4)
  expect_equal(raw_moment(d_dirac, 10), 1024)
})

test_that("raw_moment works with 'infin' functions with few intervals", {
  d_unif_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  expect_equal(raw_moment(d_unif_1, 1), 1.5)
  expect_equal(raw_moment(d_unif_1, 2), 1/12 + 1.5^2)

  d_unif_2 <- new_d(data.frame(x = 0:2, y = c(1, 1, 1)/2), "infin")
  expect_equal(raw_moment(d_unif_2, 1), 1)
  expect_equal(raw_moment(d_unif_2, 2), 1/3 + 1^2)
})


# raw_moment_infin --------------------------------------------------------
# Tested in `raw_moment()`


# compute_density_crossings -----------------------------------------------
test_that("compute_density_crossings works", {
  cur_d_1 <- new_d(data.frame(x = 1:6, y = c(0, 1, 0, 0, 1, 0)), "infin")
  cur_d_2 <- new_d(data.frame(x = 1:6+0.5, y = c(0, 1, 0, 0, 1, 0)), "infin")
  expect_equal(
    compute_density_crossings(cur_d_1, cur_d_2), c(2.25, 3.5, 4, 5.25)
  )

  # Acceptence of different pdqr-functions
  expect_equal(
    compute_density_crossings(cur_d_1, as_p(cur_d_2)),
    compute_density_crossings(as_q(cur_d_1), as_r(cur_d_2))
  )
})

test_that("compute_density_crossings handles 'intervals of identity'", {
  # All consecutive intervals on which densities are identical should be treated
  # as a single identity interval and be represented in output with its edges.

  # "Partial" identity
  cur_d_1 <- new_d(data.frame(x = 1:5, y = c(0, 0, 0, 1, 0)), "infin")
  cur_d_2 <- new_d(
    data.frame(x = c(1:4, 4.5, 5.5, 6), y = c(0, 0, 0, 1, 0, 0, 1)), "infin"
  )
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), c(1, 4, 5))

  # Total identity
  cur_d <- new_d(data.frame(x = 1:6, y = c(0, 1, 1, 0, 1, 0)), "infin")
  expect_equal(compute_density_crossings(cur_d, cur_d), c(1, 6))
})

test_that("compute_density_crossings handles single intervals in 'x_tbl'", {
  cur_d_1 <- new_d(data.frame(x = 1:2, y = c(1, 0)), "infin")
  cur_d_2 <- new_d(data.frame(x = 1:2, y = c(0, 1)), "infin")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), 1.5)
})

test_that("compute_density_crossings handles intersection on grid", {
  cur_d_1 <- new_d(data.frame(x = 1:3, y = c(2, 1, 2)/3), "infin")
  cur_d_2 <- new_d(data.frame(x = 0:4, y = c(2, 0, 1, 0, 2)/3), "infin")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), 2)
})

test_that("compute_density_crossings handles intersection on edge", {
  # Right edge of first argument
  cur_d_1 <- new_d(data.frame(x = 1:2, y = c(1, 0)), "infin")
  cur_d_2 <- new_d(data.frame(x = 2:3, y = c(0, 1)), "infin")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), 2)

  # Left edge of first argument
  cur_d_1 <- new_d(data.frame(x = 1:2, y = c(1, 0)), "infin")
  cur_d_2 <- new_d(data.frame(x = 0:1, y = c(0, 1)), "infin")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), 1)
})

test_that("compute_density_crossings handles no intersections", {
  # Non-trivial intersection support
  cur_d_1 <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  cur_d_2 <- new_d(data.frame(x = c(0, 3), y = c(1, 1)/3), "infin")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_2), numeric(0))

  # No intersection support
  cur_d_3 <- new_d(data.frame(x = 11:12, y = c(1, 1)), "infin")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_3), numeric(0))
  expect_equal(compute_density_crossings(cur_d_3, cur_d_1), numeric(0))

  # Intersection support consists from one value
  cur_d_4 <- new_d(data.frame(x = c(-1, 1), y = c(1, 1)/2), "infin")
  expect_equal(compute_density_crossings(cur_d_1, cur_d_4), numeric(0))
})

test_that("compute_density_crossings handles real world examples", {
  # Case of crossing
  d_norm <- as_d(dnorm)
  d_norm_2 <- as_d(dnorm, mean = 1, sd = 0.1)
  inters <- compute_density_crossings(d_norm, d_norm_2)
  expect_true(length(inters) == 2)
  expect_equal(d_norm(inters), d_norm_2(inters))

  # Case of no crossing
  d_unif <- as_d(dunif, min = 2, max = 5)
  expect_equal(compute_density_crossings(d_norm, d_unif), numeric(0))
})


# compute_cdf_crossings ---------------------------------------------------
test_that("compute_cdf_crossings works", {
  cur_p_1 <- new_p(data.frame(x = 1:2, y = c(1, 1)), "infin")
  cur_p_2 <- new_p(data.frame(x = c(0, 4), y = c(1, 1)/4), "infin")
  expect_equal(compute_cdf_crossings(cur_p_1, cur_p_2), 4/3)

  # Acceptence of different pdqr-functions
  expect_equal(
    compute_density_crossings(cur_p_1, as_p(cur_p_2)),
    compute_density_crossings(as_q(cur_p_1), as_r(cur_p_2))
  )
})

test_that("compute_cdf_crossings handles 'intervals of identity'", {
  # All consecutive intervals on which CDFs are identical should be treated as a
  # single identity interval and be represented in output with its edges.

  # "Partial" identity
  cur_p_1 <- new_p(
    data.frame(x = c(1, 1.5, 2, 3, 3.5, 4), y = c(1, 0, 1, 1, 0, 1)), "infin"
  )
  cur_p_2 <- new_p(data.frame(x = 1:4, y = c(0, 1, 1, 0)), "infin")
  expect_equal(compute_cdf_crossings(cur_p_1, cur_p_2), c(1, 2, 3, 4))

  # Total identity
  expect_equal(compute_cdf_crossings(d_infin, d_infin), meta_support(d_infin))
})

test_that("compute_cdf_crossings handles no intersections", {
  # No intersection support
  cur_p_1 <- new_p(data.frame(x = 0:1, y = c(1, 1)), "infin")
  cur_p_2 <- new_p(data.frame(x = 11:12, y = c(1, 1)), "infin")
  expect_equal(compute_cdf_crossings(cur_p_1, cur_p_2), numeric(0))
  expect_equal(compute_cdf_crossings(cur_p_2, cur_p_1), numeric(0))

  # Intersection support consists from one value
  cur_p_3 <- new_p(data.frame(x = 1:2, y = c(1, 1)), "infin")
  expect_equal(compute_cdf_crossings(cur_p_1, cur_p_3), numeric(0))
})

test_that("compute_cdf_crossings handles real world examples", {
  # Case of crossing
  p_norm <- as_p(pnorm)
  p_norm_2 <- as_p(pnorm, mean = 1, sd = 0.1)
  inters <- compute_cdf_crossings(p_norm, p_norm_2)
  expect_true(length(inters) == 1)
  expect_equal(p_norm(inters), p_norm_2(inters))

  # Case of no crossing
  p_unif <- as_p(punif, min = 2, max = 5)
  expect_equal(compute_cdf_crossings(p_norm, p_unif), numeric(0))
})


# pair_cdf_data -----------------------------------------------------------
# Tested in `compute_cdf_crossings()`


# na_sqrt -----------------------------------------------------------------
test_that("na_sqrt works", {
  expect_equal(na_sqrt(c(-4, 0, 4)), c(NA, 0, 2))
})


# na_outside --------------------------------------------------------------
test_that("na_outside works", {
  expect_equal(na_outside(1:5, 2, 4), c(NA, 2:4, NA))
  expect_equal(na_outside(1:5, 0, 6), 1:5)
  expect_equal(na_outside(1:5, 2.5, 2.5), rep(NA_integer_, 5))
})


# region_is_in ------------------------------------------------------------
test_that("region_is_in works", {
  region_1 <- data.frame(left = 1:2, right = 1:2 + 0.5)
  x_test_1 <- c(0, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 3, -Inf, Inf)
  # Intervals are [1; 1.5] and [2; 2.5]
  expect_equal(
    region_is_in(region_1, x_test_1, left_closed = TRUE, right_closed = TRUE),
    c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  # Intervals are (1; 1.5] and (2; 2.5]
  expect_equal(
    region_is_in(region_1, x_test_1, left_closed = FALSE, right_closed = TRUE),
    c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  # Intervals are [1; 1.5) and [2; 2.5)
  expect_equal(
    region_is_in(region_1, x_test_1, left_closed = TRUE, right_closed = FALSE),
    c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  # Intervals are (1; 1.5) and (2; 2.5)
  expect_equal(
    region_is_in(region_1, x_test_1, left_closed = FALSE, right_closed = FALSE),
    c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )

  region_2 <- data.frame(left = 1:2, right = 2:3)
  x_test_2 <- 2
  # Intervals are [1; 2] and [2; 3]
  expect_equal(
    region_is_in(region_2, x_test_2, left_closed = TRUE, right_closed = TRUE),
    TRUE
  )
  # Intervals are (1; 2] and (2; 3]
  expect_equal(
    region_is_in(region_2, x_test_2, left_closed = FALSE, right_closed = TRUE),
    TRUE
  )
  # Intervals are [1; 2) and [2; 3)
  expect_equal(
    region_is_in(region_2, x_test_2, left_closed = TRUE, right_closed = FALSE),
    TRUE
  )
  # Intervals are (1; 2) and (2; 3)
  expect_equal(
    region_is_in(region_2, x_test_2, left_closed = FALSE, right_closed = FALSE),
    FALSE
  )

  region_3 <- data.frame(left = c(-1, 1, 2), right = c(0, 1, 3))
  x_test_3 <- 1
  # In case of zero-width interval one of `left_closed` or `right_closed` should
  # be `TRUE` in order to accept that point as "in region"
  expect_equal(
    region_is_in(region_3, x_test_3, left_closed = TRUE, right_closed = TRUE),
    TRUE
  )
  expect_equal(
    region_is_in(region_3, x_test_3, left_closed = FALSE, right_closed = TRUE),
    TRUE
  )
  # Intervals are [1; 2) and [2; 3)
  expect_equal(
    region_is_in(region_3, x_test_3, left_closed = TRUE, right_closed = FALSE),
    TRUE
  )
  # Intervals are (1; 2) and (2; 3)
  expect_equal(
    region_is_in(region_3, x_test_3, left_closed = FALSE, right_closed = FALSE),
    FALSE
  )
})

test_that("region_is_in validates input", {
  region <- data.frame(left = 1, right = 2)
  expect_error(region_is_in("a", 1), "`region`.*data frame")
  expect_error(region_is_in(data.frame(a = 1), 1), '`region`.*"left"')
  expect_error(region_is_in(region, "a"), "`x`.*numeric")
  expect_error(
    region_is_in(region, 1, left_closed = "a"), "`left_closed`.*TRUE.*FALSE"
  )
  expect_error(
    region_is_in(region, 1, right_closed = "a"), "`right_closed`.*TRUE.*FALSE"
  )
})


# region_prob -------------------------------------------------------------
test_that("region_prob works with 'fin' type", {
  cur_d <- new_d(data.frame(x = 1:4, prob = 1:4/10), "fin")

  region_1 <- data.frame(left = c(1, 3) - 0.25, right = c(1, 3) + 0.25)
  expect_equal(region_prob(region_1, cur_d), 0.1 + 0.3)

  region_2 <- data.frame(left = c(1, 3.25), right = c(3, 4.5))
  expect_equal(
    region_prob(region_2, cur_d, left_closed = TRUE, right_closed = TRUE),
    0.1 + 0.2 + 0.3 + 0.4
  )
  expect_equal(
    region_prob(region_2, cur_d, left_closed = FALSE, right_closed = TRUE),
    0.2 + 0.3 + 0.4
  )
  expect_equal(
    region_prob(region_2, cur_d, left_closed = TRUE, right_closed = FALSE),
    0.1 + 0.2 + 0.4
  )
  expect_equal(
    region_prob(region_2, cur_d, left_closed = FALSE, right_closed = FALSE),
    0.2 + 0.4
  )

  region_3 <- data.frame(left = c(1, 3), right = c(2, 4))
  expect_equal(
    region_prob(region_3, cur_d, left_closed = TRUE, right_closed = TRUE),
    0.1 + 0.2 + 0.3 + 0.4
  )
  expect_equal(
    region_prob(region_3, cur_d, left_closed = FALSE, right_closed = TRUE),
    0.2 + 0.4
  )
  expect_equal(
    region_prob(region_3, cur_d, left_closed = TRUE, right_closed = FALSE),
    0.1 + 0.3
  )
  expect_equal(
    region_prob(region_3, cur_d, left_closed = FALSE, right_closed = FALSE),
    0
  )

  region_4 <- data.frame(left = 1, right = 1)
  expect_equal(
    region_prob(region_4, cur_d, left_closed = TRUE, right_closed = TRUE),
    0.1
  )
  expect_equal(
    region_prob(region_4, cur_d, left_closed = FALSE, right_closed = TRUE),
    0.1
  )
  expect_equal(
    region_prob(region_4, cur_d, left_closed = TRUE, right_closed = FALSE),
    0.1
  )
  expect_equal(
    region_prob(region_4, cur_d, left_closed = FALSE, right_closed = FALSE),
    0
  )

  region_5 <- data.frame(left = -10, right = -9)
  expect_equal(
    region_prob(region_5, cur_d, left_closed = TRUE, right_closed = TRUE),
    0
  )
  expect_equal(
    region_prob(region_5, cur_d, left_closed = FALSE, right_closed = TRUE),
    0
  )
  expect_equal(
    region_prob(region_5, cur_d, left_closed = TRUE, right_closed = FALSE),
    0
  )
  expect_equal(
    region_prob(region_5, cur_d, left_closed = FALSE, right_closed = FALSE),
    0
  )
})

test_that("region_prob works with 'infin' type", {
  expect_region_prob_works_with_infin <- function(region, f, ref_output) {
    expect_equal(
      region_prob(region, f, left_closed = TRUE, right_closed = TRUE),
      ref_output
    )
    expect_equal(
      region_prob(region, f, left_closed = FALSE, right_closed = TRUE),
      ref_output
    )
    expect_equal(
      region_prob(region, f, left_closed = TRUE, right_closed = FALSE),
      ref_output
    )
    expect_equal(
      region_prob(region, f, left_closed = FALSE, right_closed = FALSE),
      ref_output
    )
  }

  d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "infin")

  expect_region_prob_works_with_infin(
    region = data.frame(left = c(0.1, 0.5), right = c(0.2, 0.7)),
    f = d_unif,
    ref_output = (0.2 - 0.1) + (0.7 - 0.5)
  )
  expect_region_prob_works_with_infin(
    region = data.frame(left = c(0.5, 0.7), right = c(0.6, 0.7)),
    f = d_unif,
    ref_output = 0.6 - 0.5
  )
  expect_region_prob_works_with_infin(
    region = data.frame(left = c(-1, 0.5, 0.7), right = c(-1, 0.5, 0.7)),
    f = d_unif,
    ref_output = 0
  )
  expect_region_prob_works_with_infin(
    region = data.frame(left = -1, right = -0.5),
    f = d_unif,
    ref_output = 0
  )
})

test_that("region_prob works with dirac-like 'infin' functions", {
  d_dirac_1 <- new_d(1, "infin")
  expect_equal(region_prob(data.frame(left = 0, right = 1), d_dirac_1), 0.5)
  expect_equal(region_prob(data.frame(left = 0, right = 1.5), d_dirac_1), 1)

  d_dirac_2 <- form_mix(list(new_d(1, "infin"), new_d(2, "infin")))
  expect_equal(region_prob(data.frame(left = 0, right = 1), d_dirac_2), 0.25)
  expect_equal(region_prob(data.frame(left = 0, right = 1.5), d_dirac_2), 0.5)
  expect_equal(
    region_prob(data.frame(left = c(0, 1.5), right = c(1, 2)), d_dirac_2),
    0.5
  )
  expect_equal(
    region_prob(data.frame(left = c(0, 1), right = c(1, 2)), d_dirac_2),
    0.75
  )
  expect_equal(
    region_prob(data.frame(left = c(0, 1), right = c(1, 2.5)), d_dirac_2),
    1
  )
})

test_that("region_prob handles real world examples", {
  d_unif <- as_d(dunif)
  expect_equal(region_prob(data.frame(left = -1, right = 0.5), d_unif), 0.5)

  d_norm <- as_d(dnorm)
  p_norm <- as_p(d_norm)
  expect_equal(
    region_prob(data.frame(left = -1, right = 0.5), d_norm),
    p_norm(0.5) - p_norm(-1)
  )
})

test_that("region_prob validates input", {
  region <- data.frame(left = 1, right = 2)
  expect_error(region_prob("a", d_fin), "`region`.*data frame")
  expect_error(region_prob(data.frame(a = 1), d_fin), '`region`.*"left"')
  expect_error(region_prob(region, "a"), "`f`.*function")
  expect_error(region_prob(region, function(x) {x}), "`f`.*pdqr")
  expect_error(
    region_prob(region, d_fin, left_closed = "a"), "`left_closed`.*TRUE.*FALSE"
  )
  expect_error(
    region_prob(region, d_fin, right_closed = "a"),
    "`right_closed`.*TRUE.*FALSE"
  )
})


# region_height -----------------------------------------------------------
test_that("region_height works with 'fin' type", {
  cur_d <- new_d(data.frame(x = 1:4, prob = 1:4/10), "fin")

  region_1 <- data.frame(left = c(1, 3) - 0.25, right = c(1, 3) + 0.25)
  expect_equal(region_height(region_1, cur_d), 0.1)

  region_2 <- data.frame(left = c(1, 3.25), right = c(3, 4.5))
  expect_equal(
    region_height(region_2, cur_d, left_closed = TRUE, right_closed = TRUE),
    0.1
  )
  expect_equal(
    region_height(region_2, cur_d, left_closed = FALSE, right_closed = TRUE),
    0.2
  )
  expect_equal(
    region_height(region_2, cur_d, left_closed = TRUE, right_closed = FALSE),
    0.1
  )
  expect_equal(
    region_height(region_2, cur_d, left_closed = FALSE, right_closed = FALSE),
    0.2
  )

  region_3 <- data.frame(left = c(1, 3), right = c(2, 4))
  expect_equal(
    region_height(region_3, cur_d, left_closed = TRUE, right_closed = TRUE),
    0.1
  )
  expect_equal(
    region_height(region_3, cur_d, left_closed = FALSE, right_closed = TRUE),
    0.2
  )
  expect_equal(
    region_height(region_3, cur_d, left_closed = TRUE, right_closed = FALSE),
    0.1
  )
  expect_equal(
    region_height(region_3, cur_d, left_closed = FALSE, right_closed = FALSE),
    0
  )

  region_4 <- data.frame(left = 1, right = 1)
  expect_equal(
    region_height(region_4, cur_d, left_closed = TRUE, right_closed = TRUE),
    0.1
  )
  expect_equal(
    region_height(region_4, cur_d, left_closed = FALSE, right_closed = TRUE),
    0.1
  )
  expect_equal(
    region_height(region_4, cur_d, left_closed = TRUE, right_closed = FALSE),
    0.1
  )
  expect_equal(
    region_height(region_4, cur_d, left_closed = FALSE, right_closed = FALSE),
    0
  )

  region_5 <- data.frame(left = -10, right = -9)
  expect_equal(
    region_height(region_5, cur_d, left_closed = TRUE, right_closed = TRUE),
    0
  )
  expect_equal(
    region_height(region_5, cur_d, left_closed = FALSE, right_closed = TRUE),
    0
  )
  expect_equal(
    region_height(region_5, cur_d, left_closed = TRUE, right_closed = FALSE),
    0
  )
  expect_equal(
    region_height(region_5, cur_d, left_closed = FALSE, right_closed = FALSE),
    0
  )
})

test_that("region_height works with 'infin' type", {
  cur_d <- new_d(data.frame(x = 1:5, y = c(0, 2, 1, 2, 0)/5), "infin")

  expect_equal(
    region_height(data.frame(left = 1.5, right = 2.5), cur_d), cur_d(1.5)
  )
  expect_equal(
    region_height(data.frame(left = c(1.5, 3.5), right = c(2.5, 4.5)), cur_d),
    cur_d(1.5)
  )
  expect_equal(
    region_height(data.frame(left = 2, right = 4), cur_d), cur_d(3)
  )
  expect_equal(
    region_height(data.frame(left = c(2, 3), right = c(3, 4)), cur_d),
    cur_d(3)
  )
  expect_equal(
    region_height(data.frame(left = c(2.5, 3.5), right = c(2.5, 3.5)), cur_d),
    min(cur_d(c(2.5, 3.5)))
  )
  expect_equal(
    region_height(data.frame(left = -1, right = -0.5), cur_d), 0
  )
  expect_equal(
    region_height(data.frame(left = -1, right = 10), cur_d), 0
  )
  expect_equal(
    region_height(data.frame(left = 1, right = 5), cur_d), 0
  )
})

test_that("region_height works with dirac-like 'infin' functions", {
  d_dirac_1 <- new_d(1, "infin")
  expect_equal(region_height(data.frame(left = 0, right = 1), d_dirac_1), 0)
  expect_equal(
    region_height(data.frame(left = 1, right = 1), d_dirac_1), d_dirac_1(1)
  )

  d_dirac_2 <- form_mix(
    list(new_d(1, "infin"), new_d(2, "infin")), weights = c(0.25, 0.75)
  )
  expect_equal(region_height(data.frame(left = 0, right = 1), d_dirac_2), 0)
  expect_equal(
    region_height(data.frame(left = 1, right = 1), d_dirac_2), d_dirac_2(1)
  )
  expect_equal(
    region_height(data.frame(left = c(1, 2), right = c(1, 2)), d_dirac_2),
    min(d_dirac_2(c(1, 2)))
  )
})

test_that("region_height validates input", {
  region <- data.frame(left = 1, right = 2)
  expect_error(region_height("a", d_fin), "`region`.*data frame")
  expect_error(region_height(data.frame(a = 1), d_fin), '`region`.*"left"')
  expect_error(region_height(region, "a"), "`f`.*function")
  expect_error(region_height(region, function(x) {x}), "`f`.*pdqr")
  expect_error(
    region_height(region, d_fin, left_closed = "a"),
    "`left_closed`.*TRUE.*FALSE"
  )
  expect_error(
    region_height(region, d_fin, right_closed = "a"),
    "`right_closed`.*TRUE.*FALSE"
  )
})


# assert_region -----------------------------------------------------------
test_that("assert_region works", {
  expect_silent(assert_region(data.frame(left = 1, right = 1)))
  expect_silent(assert_region(data.frame(left = 1:2, right = 1:2)))
  expect_silent(assert_region(data.frame(left = 1:2, right = 1:2+0.5)))
  expect_silent(assert_region(data.frame(left = 1:2, right = 1:2, c = -1:0)))

  input <- "a"
  expect_error(assert_region(input), "`input`.*data frame")

  # Presence and contents of "left" and "right" columns
  expect_error(assert_region(data.frame(right = 2:3)), 'have.*column.*"left"')
  expect_error(
    assert_region(data.frame(left = c("a", "b"), right = 2:3)),
    'have.*numeric.*"left"'
  )
  expect_error(
    assert_region(data.frame(left = c(-Inf, 2), right = 2:3)), 'finite'
  )
  expect_error(assert_region(data.frame(left = 1:2)), 'have.*column.*"right"')
  expect_error(
    assert_region(data.frame(left = 1:2, right = c("a", "b"))),
    'have.*numeric.*"right"'
  )
  expect_error(
    assert_region(data.frame(left = 1:2, right = c(2, Inf))), 'finite'
  )

  expect_error(assert_region(data.frame(left = 1, right = -1)), "all.*not less")
  expect_error(
    assert_region(data.frame(left = 1:2, right = c(-1, 3))), "all.*not less"
  )

  # Orderliness and uniqueness of intervals
  expect_silent(assert_region(data.frame(left = 1, right = 1)))
  expect_silent(assert_region(data.frame(left = 1, right = 2)))
  expect_silent(assert_region(data.frame(left = 1:2, right = 1:2)))
  expect_silent(assert_region(data.frame(left = 1:2, right = 2:3)))

  expect_error(
    assert_region(data.frame(left = c(1, 1), right = c(1, 1))), "distinct"
  )
  expect_error(
    assert_region(data.frame(left = c(1, 1), right = c(2, 2))), "distinct"
  )
  expect_error(
    assert_region(data.frame(left = c(1, 1), right = c(2, 3))), "ordered"
  )
  expect_error(
    assert_region(data.frame(left = c(1, 2, -1), right = c(1.5, 2.5, 3))),
    "ordered"
  )
})


# is_region_ordered -------------------------------------------------------
test_that("is_region_ordered works", {
  expect_true(is_region_ordered(data.frame(left = 1, right = 1)))
  expect_true(is_region_ordered(data.frame(left = 1, right = 2)))
  expect_true(is_region_ordered(data.frame(left = 1:2, right = 1:2)))
  expect_true(is_region_ordered(data.frame(left = 1:2, right = 2:3)))

  expect_false(is_region_ordered(data.frame(left = c(1, 1), right = c(1, 1))))
  expect_false(is_region_ordered(data.frame(left = c(1, 1), right = c(2, 2))))
  expect_false(is_region_ordered(data.frame(left = c(1, 1), right = c(2, 3))))
  expect_false(
    is_region_ordered(data.frame(left = c(1, 2, -1), right = c(1.5, 2.5, 3)))
  )
})
