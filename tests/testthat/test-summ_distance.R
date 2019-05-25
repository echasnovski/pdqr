context("test-summ_distance")


# summ_distance -----------------------------------------------------------
# More thorough tests are done in `distance_*()` functions
test_that("summ_distance works", {
  p_f <- as_p(punif)
  p_g <- as_p(punif, max = 0.5)

  expect_equal(summ_distance(p_f, p_g, method = "KS"), 0.5)
})

test_that("summ_distance returns 0 for identical inputs", {
  expect_equal(summ_distance(d_fin, d_fin, method = "KS"), 0)
  expect_equal(summ_distance(d_infin, d_infin, method = "KS"), 0)
})

test_that("summ_distance validates input", {
  expect_error(summ_distance("a", d_fin), "`f`.*not pdqr-function")
  expect_error(summ_distance(d_fin, "a"), "`g`.*not pdqr-function")
  expect_error(summ_distance(d_fin, d_fin, method = "a"), "`method`.*one of")
})


# distance_ks -------------------------------------------------------------
test_that("distance_ks works with two 'fin' functions", {
  p_f <- new_p(1:3, "fin")
  p_g <- new_p(1:3+1.5, "fin")
  expect_equal(distance_ks(p_f, p_g), 2/3)
  # Checking twice to test independence of argument order
  expect_equal(distance_ks(p_g, p_f), 2/3)

  expect_equal(
    distance_ks(
      new_p(data.frame(x = 1:4, prob = 1:4/10), "fin"),
      new_p(data.frame(x = 1:4, prob = 4:1/10), "fin")
    ),
    0.4
  )

  p_f <- as_p(ppois, lambda = 10)
  p_g <- as_p(ppois, lambda = 5)
  expect_equal(distance_ks(p_f, p_g), abs(p_f(7) - p_g(7)))
})

test_that("distance_ks works with mixed-type functions", {
  # These two cases represent "supremum-not-maximum" quality of K-S distatnce,
  # when actual distance is achieved as limit of distances from left side
  cur_fin <- new_p(1:10, "fin")
  cur_infin <- new_p(data.frame(x = c(0, 10), y = c(1, 1)/10), "infin")
  expect_equal(distance_ks(cur_fin, cur_infin), 0.1)
  # Checking twice to test independence of argument order
  expect_equal(distance_ks(cur_infin, cur_fin), 0.1)

  expect_equal(
    distance_ks(
      new_p(data.frame(x = 1:2, y = c(1, 1)), "infin"), new_p(2, "fin")
    ),
    1
  )

  # Test that the smallest "x" value is returned in case of several candidates
  p_fin_2 <- new_p(data.frame(x = 2:3, prob = c(0.5, 0.5)), "fin")
  p_infin_2 <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "infin")
  expect_equal(distance_ks(p_fin_2, p_infin_2), 0.5)

  # Case when smallest "x" value with maximum absolute CDF difference is one
  # of "x" values from "x_tbl" of "infin" pdqr-function
  p_fin_3 <- new_p(2.5, "fin")
  p_infin_3 <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "infin")
  expect_equal(distance_ks(p_fin_3, p_infin_3), 0.5)
})

test_that("distance_ks works with two 'infin' functions", {
  # Maximum at density crossings
  p_f <- new_p(data.frame(x = 0:1, y = c(2, 0)), "infin")
  p_g <- new_p(data.frame(x = c(0.5, 1, 1.5), y = c(0, 2, 0)), "infin")
  expect_equal(distance_ks(p_f, p_g), abs(p_f(2/3) - p_g(2/3)))
  # Checking twice to test independence of argument order
  expect_equal(distance_ks(p_g, p_f), abs(p_f(2/3) - p_g(2/3)))

  # Multiple density intersections in real-world example
  p_f <- new_p(data.frame(x = c(1:3, 5, 7), y = c(0, 0.5, 0, 0.25, 0)), "infin")
  p_g <- new_p(
    data.frame(x = c(1:3, 5, 7) + 0.5, y = c(0, 0.5, 0, 0.25, 0)), "infin"
  )
  expect_equal(distance_ks(p_f, p_g), abs(p_f(2.25) - p_g(2.25)))

  # Non-intersecting densities in real-world example
  p_f <- as_p(punif)
  p_g <- as_p(punif, min = -1, max = 3)
  expect_equal(distance_ks(p_f, p_g), abs(p_f(1) - p_g(1)))

  # Common y-zero plateau. Maximum difference on edge of one of support.
  p_f <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "infin")
  p_g <- new_p(data.frame(x = 0:5, y = c(0, 1, 0, 0, 1, 0)/2), "infin")
  expect_equal(distance_ks(p_f, p_g), abs(p_f(1) - p_g(1)))

  # Common y-zero plateau. Maximum difference on edge of plateau.
  p_f <- new_p(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "infin")
  p_g <- new_p(data.frame(x = 3:6, y = c(0, 0, 1, 0)), "infin")
  expect_equal(distance_ks(p_f, p_g), 1)
})

test_that("distance_ks returns zero with identical inputs", {
  expect_equal(distance_ks(d_fin, d_fin), 0)
  expect_equal(distance_ks(d_infin, d_infin), 0)

  d_dirac <- new_d(2, "infin")
  expect_equal(distance_ks(d_dirac, d_dirac), 0)
})

test_that("distance_ks works with non-overlapping supports", {
  cur_fin_1 <- new_p(1:4, "fin")
  cur_fin_2 <- new_p(5:6, "fin")
  cur_infin_1 <- new_p(data.frame(x = 1:4, y = c(1, 1)/3), "infin")
  cur_infin_2 <- new_p(data.frame(x = 5:6, y = c(1, 1)), "infin")
  cur_infin_3 <- new_p(data.frame(x = 4:5, y = c(1, 1)), "infin")

  # "Two fin"
  expect_equal(distance_ks(cur_fin_1, cur_fin_2), 1)
  expect_equal(distance_ks(cur_fin_2, cur_fin_1), 1)

  # "Mixed-typed"
  expect_equal(distance_ks(cur_fin_1, cur_infin_2), 1)
  expect_equal(distance_ks(cur_infin_2, cur_fin_1), 1)
    # "Touching" supports
  expect_equal(distance_ks(cur_fin_1, cur_infin_3), 1)
  expect_equal(distance_ks(cur_infin_3, cur_fin_1), 1)

  # "Two infin"
  expect_equal(distance_ks(cur_infin_1, cur_infin_2), 1)
  expect_equal(distance_ks(cur_infin_2, cur_infin_1), 1)
    # "Touching" supports
  expect_equal(distance_ks(cur_infin_1, cur_infin_3), 1)
  expect_equal(distance_ks(cur_infin_3, cur_infin_1), 1)
})

test_that("distance_ks works with dirac-like functions", {
  # K-S distance when "dirac" function is involved should be essentially (but
  # not exactly) the same as if it is replaced with corresponding "fin" (except
  # the case when the other one is "fin" with one of points lying inside "dira"
  # support)
  d_dirac <- new_d(2, "infin")
  d_dirac_fin <- new_d(2, "fin")

  # "Mixed-type" case
  expect_equal(distance_ks(d_fin, d_dirac), distance_ks(d_fin, d_dirac_fin))
  # Here 0.5 because one of points from `d_dirac_fin` lies inside `d_dirac`
  # support
  expect_equal(distance_ks(d_dirac, d_dirac_fin), 0.5)

  # "Two infin" case
  expect_equal(distance_ks(d_infin, d_dirac), distance_ks(d_infin, d_dirac_fin))
  # Non-overlapping dirac-like functions
  expect_equal(distance_ks(d_dirac, new_d(3, "infin")), 1)
})

test_that("distance_ks works with different pdqr classes", {
  expect_equal(distance_ks(d_fin, d_infin), distance_ks(p_fin, q_infin))
})


# distance_ks_two_fin -----------------------------------------------------
# Tested in `distance_ks()`


# distance_ks_mixed -------------------------------------------------------
# Tested in `distance_ks()`


# distance_ks_two_infin ---------------------------------------------------
# Tested in `distance_ks()`
