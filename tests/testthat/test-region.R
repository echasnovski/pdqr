context("test-region")

library(grDevices)

# All tested plot calls are wrapped in `grDevices::recordPlot` to avoid printing
# of the result of their calls (`NULL`). This is a result of {vdiffr} approach:
# it `print()`s its input (resulting here with `NULL`), unless wrapped in
# function.


# Input data --------------------------------------------------------------
region_dis_1 <- data.frame(left = c(0, 0.5, 1), right = c(0, 0.5, 1))
region_dis_2 <- data.frame(left = c(0, 0.75, 1), right = c(0, 0.75, 1))
region_con_1 <- data.frame(left = c(0, 2), right = c(1, 3))
region_con_2 <- data.frame(left = c(0.5, 2.5), right = c(1.5, 3.5))
region_mix <- data.frame(left = c(0, 0.5), right = c(0, 1))
region_mix_as_con <- data.frame(left = 0.5, right = 1)

region_dis_1_pdqr <- region_as_pdqr(region_dis_1)
region_dis_2_pdqr <- region_as_pdqr(region_dis_2)
region_con_1_pdqr <- region_as_pdqr(region_con_1)
region_con_2_pdqr <- region_as_pdqr(region_con_2)
region_mix_pdqr <- region_as_pdqr(region_mix)


# Custom expectations -----------------------------------------------------
expect_region_distance_other_methods <- function(region, region2) {
  region_pdqr <- region_as_pdqr(region)
  region2_pdqr <- region_as_pdqr(region2)

  is_other_method <- !(methods_distance %in% c("avgdist", "entropy"))
  for (meth in methods_distance[is_other_method]) {
    expect_equal(
      region_distance(region, region2, method = meth),
      summ_distance(region_pdqr, region2_pdqr, method = meth)
    )
  }

  if (meta_type(region_pdqr) != meta_type(region2_pdqr)) {
    expect_error(
      region_distance(region, region2, method = "entropy"),
      '"entropy".*same type'
    )
  } else {
    expect_equal(
      region_distance(region, region2, method = "entropy"),
      summ_distance(region_pdqr, region2_pdqr, method = "entropy")
    )
  }
}


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
  expect_error(region_is_in(x = 1), "`region`.*missing.*region data frame")
  expect_error(region_is_in("a", 1), "`region`.*data frame")
  expect_error(region_is_in(data.frame(a = 1), 1), '`region`.*"left"')
  expect_error(region_is_in(region), "`x`.*missing.*numeric vector")
  expect_error(region_is_in(region, "a"), "`x`.*numeric")
  expect_error(
    region_is_in(region, 1, left_closed = "a"), "`left_closed`.*TRUE.*FALSE"
  )
  expect_error(
    region_is_in(region, 1, right_closed = "a"), "`right_closed`.*TRUE.*FALSE"
  )
})


# region_prob -------------------------------------------------------------
test_that("region_prob works with 'discrete' functions", {
  cur_d <- new_d(data.frame(x = 1:4, prob = 1:4 / 10), "discrete")

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

test_that("region_prob works with 'continuous' functions", {
  expect_region_prob_works_with_con <- function(region, f, ref_output) {
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

  d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")

  expect_region_prob_works_with_con(
    region = data.frame(left = c(0.1, 0.5), right = c(0.2, 0.7)),
    f = d_unif,
    ref_output = (0.2 - 0.1) + (0.7 - 0.5)
  )
  expect_region_prob_works_with_con(
    region = data.frame(left = c(0.5, 0.7), right = c(0.6, 0.7)),
    f = d_unif,
    ref_output = 0.6 - 0.5
  )
  expect_region_prob_works_with_con(
    region = data.frame(left = c(-1, 0.5, 0.7), right = c(-1, 0.5, 0.7)),
    f = d_unif,
    ref_output = 0
  )
  expect_region_prob_works_with_con(
    region = data.frame(left = -1, right = -0.5),
    f = d_unif,
    ref_output = 0
  )
})

test_that("region_prob works with dirac-like 'continuous' functions", {
  d_dirac_1 <- new_d(1, "continuous")
  expect_equal(region_prob(data.frame(left = 0, right = 1), d_dirac_1), 0.5)
  expect_equal(region_prob(data.frame(left = 0, right = 1.5), d_dirac_1), 1)

  d_dirac_2 <- form_mix(list(new_d(1, "continuous"), new_d(2, "continuous")))
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

test_that("region_prob works with real world cases", {
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
  expect_error(region_prob(f = d_dis), "`region`.*missing.*region data frame")
  expect_error(region_prob("a", d_dis), "`region`.*data frame")
  expect_error(region_prob(data.frame(a = 1), d_dis), '`region`.*"left"')
  expect_error(region_prob(region, "a"), "`f`.*not pdqr-function")
  expect_error(
    region_prob(region, d_dis, left_closed = "a"), "`left_closed`.*TRUE.*FALSE"
  )
  expect_error(
    region_prob(region, d_dis, right_closed = "a"),
    "`right_closed`.*TRUE.*FALSE"
  )
})


# region_height -----------------------------------------------------------
test_that("region_height works with 'discrete' functions", {
  cur_d <- new_d(data.frame(x = 1:4, prob = 1:4 / 10), "discrete")

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

test_that("region_height works with 'continuous' functions", {
  cur_d <- new_d(data.frame(x = 1:5, y = c(0, 2, 1, 2, 0) / 5), "continuous")

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

test_that("region_height works with dirac-like 'continuous' functions", {
  d_dirac_1 <- new_d(1, "continuous")
  expect_equal(region_height(data.frame(left = 0, right = 1), d_dirac_1), 0)
  expect_equal(
    region_height(data.frame(left = 1, right = 1), d_dirac_1), d_dirac_1(1)
  )

  d_dirac_2 <- form_mix(
    list(new_d(1, "continuous"), new_d(2, "continuous")),
    weights = c(0.25, 0.75)
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
  expect_error(region_height(f = d_dis), "`region`.*missing.*region data frame")
  expect_error(region_height("a", d_dis), "`region`.*data frame")
  expect_error(region_height(data.frame(a = 1), d_dis), '`region`.*"left"')
  expect_error(region_height(region, "a"), "`f`.*not pdqr-function")
  expect_error(
    region_height(region, d_dis, left_closed = "a"),
    "`left_closed`.*TRUE.*FALSE"
  )
  expect_error(
    region_height(region, d_dis, right_closed = "a"),
    "`right_closed`.*TRUE.*FALSE"
  )
})


# region_width ------------------------------------------------------------
test_that("region_width works", {
  expect_equal(
    region_width(data.frame(left = c(-1, 2), right = c(1, 2.5))),
    (1 - (-1)) + (2.5 - 2)
  )
  expect_equal(
    region_width(data.frame(left = c(-1, 2), right = c(-1, 2))),
    0
  )
})

test_that("region_width validates input", {
  expect_error(region_width(), "`region`.*missing.*region data frame")
  expect_error(region_width("a"), "`region`.*data frame")
  expect_error(region_width(data.frame(a = 1)), '`region`.*"left"')
})


# region_distance ---------------------------------------------------------
test_that("region_distance works with 'Jaccard' method", {
  # "Discrete" regions
  # If both inputs are purely "discrete" regions, widths of intersection and
  # union are zero, so output is `NaN`.
  expect_equal(
    region_distance(region_dis_1, region_dis_2, method = "Jaccard"), NaN
  )
  # If only one input is purely "discrete", then width of intersection is zero,
  # but width of union is not zero, so output is 1-0.
  expect_equal(
    region_distance(region_dis_1, region_con_2, method = "Jaccard"), 1
  )
  expect_equal(
    region_distance(region_con_1, region_dis_2, method = "Jaccard"), 1
  )

  # "Continuous" regions
  expect_equal(
    region_distance(region_con_1, region_con_2, method = "Jaccard"), 1 - 1 / 3
  )
  expect_equal(
    region_distance(region_con_1, region_con_1, method = "Jaccard"), 0
  )
  # Case of consecutive intervals in region
  expect_equal(
    region_distance(
      data.frame(left = c(0, 1),     right = c(1, 2)),
      data.frame(left = c(0, 1) + 0.5, right = c(1, 2) + 0.5),
      method = "Jaccard"
    ),
    1 - 1.5 / 2.5
  )

  # "Mixed" regions
  expect_equal(
    region_distance(region_dis_1, region_mix, method = "Jaccard"),
    region_distance(region_dis_1, region_mix_as_con, method = "Jaccard")
  )
  expect_equal(
    region_distance(region_con_1, region_mix, method = "Jaccard"),
    region_distance(region_con_1, region_mix_as_con, method = "Jaccard")
  )
})

test_that("region_distance works with 'avgdist' method", {
  # Output for "avgdist" method should be equivalent to when treating regions
  # as distributions and applying `summ_distance(*, method = "avgdist")` (but
  # more accurate for "continuous" case).

  # "Discrete" regions
  expect_equal(
    region_distance(region_dis_1, region_dis_2, method = "avgdist"),
    summ_distance(region_dis_1_pdqr, region_dis_2_pdqr, method = "avgdist")
  )
  expect_equal(
    region_distance(region_dis_1, region_con_2, method = "avgdist"),
    summ_distance(region_dis_1_pdqr, region_con_2_pdqr, method = "avgdist"),
    tol = 1e-6
  )
  expect_equal(
    region_distance(region_con_1, region_dis_2, method = "avgdist"),
    summ_distance(region_con_1_pdqr, region_dis_2_pdqr, method = "avgdist"),
    tol = 1e-6
  )

  # "Continuous" regions
  expect_equal(
    region_distance(region_con_1, region_con_2, method = "avgdist"),
    summ_distance(region_con_1_pdqr, region_con_2_pdqr, method = "avgdist"),
    tol = 1e-6
  )

  # "Mixed" regions
  expect_equal(
    region_distance(region_dis_1, region_mix, method = "avgdist"),
    summ_distance(region_dis_1_pdqr, region_mix_pdqr, method = "avgdist")
  )
  expect_equal(
    region_distance(region_con_1, region_mix, method = "avgdist"),
    summ_distance(region_con_1_pdqr, region_mix_pdqr, method = "avgdist"),
    tol = 1e-6
  )
})

test_that("region_distance works with the rest of `summ_distance()` methods", {
  # "Discrete" regions
  expect_region_distance_other_methods(region_dis_1, region_dis_2)
  expect_region_distance_other_methods(region_dis_1, region_con_2)

  # "Continuous" regions
  expect_region_distance_other_methods(region_con_1, region_con_2)

  # "Mixed" regions
  expect_region_distance_other_methods(region_dis_1, region_mix)
  expect_region_distance_other_methods(region_con_1, region_mix)
})

test_that("region_distance validates input", {
  region <- data.frame(left = c(0, 2), right = c(1, 3))
  region2 <- data.frame(left = c(0, 2) + 0.5, right = c(1, 3) + 0.5)

  expect_error(region_distance(list(0:1, 1:2), region2), "`region`")
  expect_error(region_distance(region, list(0:1, 1:2)), "`region2`")
  expect_error(region_distance(region, region2, method = 1), "`method`.*string")
  expect_error(
    region_distance(region, region2, method = "a"), "`method`.*one of"
  )
})


# region_distance_jaccard -------------------------------------------------
# Tested in `region_distance()`


# region_distance_avgdist -------------------------------------------------
# Tested in `region_distance()`


# region_draw -------------------------------------------------------------
test_that("region_draw works", {
  cur_d <- new_d(data.frame(x = 1:11, y = c(0, rep(c(1, 0), 5))), "continuous")
  region <- data.frame(
    left = c(-100, 3.5, 5.5, 7.5),
    right = c(2.5,   5, 6.5, 100)
  )

  # Basic usage. Also if `region_draw()` is implemented with
  # `rect(*, ytop = 2e8, *)`, will give incorrect output
  expect_doppelganger_2(
    "region_draw-basic", recordPlot({
      plot(cur_d)
      region_draw(region)
    })
  )

  # Setting different color
  expect_doppelganger_2(
    "region_draw-col-1", recordPlot({
      plot(cur_d)
      region_draw(region, col = "green")
    })
  )
  expect_doppelganger_2(
    "region_draw-col-2", recordPlot({
      plot(cur_d)
      region_draw(region, col = "#FF0000")
    })
  )

  # Setting different `alpha`
  expect_doppelganger_2(
    "region_draw-alpha", recordPlot({
      plot(cur_d)
      region_draw(region, alpha = 0.7)
    })
  )
})

test_that("region_draw validates input", {
  region <- data.frame(left = 1, right = 2)
  expect_error(region_draw(), "`region`.*missing.*region data frame")
  expect_error(region_draw("a"), "`region`.*data frame")
  expect_error(region_draw(data.frame(a = 1)), '`region`.*"left"')
  expect_error(region_draw(region, col = "a"), "`col`.*color")
  expect_error(region_draw(region, col = c("black", "red")), "`col`.*single")
  expect_error(region_draw(region, alpha = "a"), "`alpha`.*number")
  expect_error(region_draw(region, alpha = 0:1), "`alpha`.*single")
})


# region_new --------------------------------------------------------------
test_that("region_new works", {
  expect_equal(region_new(1, 2), data.frame(left = 1, right = 2))
  expect_equal(
    region_new(c(1, 2), c(1.5, 2)),
    data.frame(left = c(1, 2), right = c(1.5, 2))
  )

  expect_error(region_new(1, -1), "not less")
})


# assert_region -----------------------------------------------------------
test_that("assert_region works", {
  expect_silent(assert_region(data.frame(left = 1, right = 1)))
  expect_silent(assert_region(data.frame(left = 1:2, right = 1:2)))
  expect_silent(assert_region(data.frame(left = 1:2, right = 1:2 + 0.5)))
  expect_silent(assert_region(data.frame(left = 1:2, right = 1:2, c = -1:0)))

  input <- "a"
  expect_error(assert_region(input), "`input` is not a region.*data frame")

  # Presence and contents of "left" and "right" columns
  expect_error(assert_region(data.frame(right = 2:3)), 'have.*column.*"left"')
  expect_error(
    assert_region(data.frame(left = c("a", "b"), right = 2:3)),
    'have.*numeric.*"left"'
  )
  expect_error(
    assert_region(data.frame(left = c(-Inf, 2), right = 2:3)), "finite"
  )
  expect_error(assert_region(data.frame(left = 1:2)), 'have.*column.*"right"')
  expect_error(
    assert_region(data.frame(left = 1:2, right = c("a", "b"))),
    'have.*numeric.*"right"'
  )
  expect_error(
    assert_region(data.frame(left = 1:2, right = c(2, Inf))), "finite"
  )

  expect_error(assert_region(data.frame(left = 1, right = -1)), "All.*not less")
  expect_error(
    assert_region(data.frame(left = 1:2, right = c(-1, 3))), "All.*not less"
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

test_that("assert_region respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_region("a"))
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


# region_as_pdqr ----------------------------------------------------------
test_that("region_as_pdqr works with 'all-continuous' region", {
  region <- data.frame(left = c(1, 3), right = c(1.5, 8))

  out <- region_as_pdqr(region)
  out_ref <- form_mix(
    f_list = list(
      new_d(data.frame(x = c(1, 1.5), y = c(1, 1)), "continuous"),
      new_d(data.frame(x = c(3, 8), y = c(1, 1)), "continuous")
    ),
    weights = c(0.5, 5) / 5.5
  )
  expect_equal(meta_x_tbl(out), meta_x_tbl(out_ref))
})

test_that("region_as_pdqr works with 'all-discrete' region", {
  x <- sort(runif(10))
  region <- data.frame(left = x, right = x)

  out <- region_as_pdqr(region)
  out_ref <- new_d(x, "discrete")
  expect_equal(meta_x_tbl(out), meta_x_tbl(out_ref))
})

test_that("region_as_pdqr works with 'mixed-type' region", {
  region <- data.frame(left = c(1, 2, 3, 10), right = c(1.5, 2, 8, 10))

  out <- region_as_pdqr(region)
  # Only intervals with positive lengths are used
  out_ref <- form_mix(
    f_list = list(
      new_d(data.frame(x = c(1, 1.5), y = c(1, 1)), "continuous"),
      new_d(data.frame(x = c(3, 8), y = c(1, 1)), "continuous")
    ),
    weights = c(0.5, 5) / 5.5
  )
  expect_equal(meta_x_tbl(out), meta_x_tbl(out_ref))
})
