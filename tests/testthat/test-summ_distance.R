context("test-summ_distance")


# Custom expectations -----------------------------------------------------
expect_dist_entropy <- function(f, g) {
  expect_equal(
    distance_entropy(f, g),
    summ_entropy2(f, g, method = "relative") +
      summ_entropy2(g, f, method = "relative")
  )
}


# summ_distance -----------------------------------------------------------
# More thorough tests are done in `distance_*()` functions
test_that("summ_distance works", {
  p_f <- as_p(punif)
  p_g <- as_p(punif, max = 0.5)

  # Method "KS"
  expect_equal(summ_distance(p_f, p_g, method = "KS"), 0.5)

  # Method "totvar"
  expect_equal(summ_distance(p_f, p_g, method = "totvar"), 0.5)
  expect_equal(
    summ_distance(new_d(1:2, "fin"), new_d(1:2, "infin"), method = "totvar"),
    1
  )

  # Method "wass"
  expect_equal(
    # Output isn't exact because `stats::integrate()` is used
    summ_distance(p_f, p_f + 10, method = "wass"), 10, tolerance = 1e-5
  )

  # Method "cramer"
    # Result is sum of squares of two triangles
  expect_equal(summ_distance(p_f, p_g, method = "cramer"), 1/12)

  # Method "entropy"
  expect_equal(
    summ_distance(p_f, p_g, method = "entropy"),
    summ_entropy2(p_f, p_g, method = "relative") +
      summ_entropy2(p_g, p_f, method = "relative")
  )
})

test_that("summ_distance returns 0 for identical inputs", {
  d_dirac <- new_d(2, "infin")

  expect_equal(summ_distance(d_fin, d_fin, method = "KS"), 0)
  expect_equal(summ_distance(d_infin, d_infin, method = "KS"), 0)
  expect_equal(summ_distance(d_dirac, d_dirac, method = "KS"), 0)

  expect_equal(summ_distance(d_fin, d_fin, method = "totvar"), 0)
  expect_equal(summ_distance(d_infin, d_infin, method = "totvar"), 0)
  expect_equal(summ_distance(d_dirac, d_dirac, method = "totvar"), 0)

  expect_equal(summ_distance(d_fin, d_fin, method = "wass"), 0)
  expect_equal(summ_distance(d_infin, d_infin, method = "wass"), 0)
  expect_equal(summ_distance(d_dirac, d_dirac, method = "wass"), 0)

  expect_equal(summ_distance(d_fin, d_fin, method = "cramer"), 0)
  expect_equal(summ_distance(d_infin, d_infin, method = "cramer"), 0)
  expect_equal(summ_distance(d_dirac, d_dirac, method = "cramer"), 0)

  expect_equal(summ_distance(d_fin, d_fin, method = "entropy"), 0)
  expect_equal(summ_distance(d_infin, d_infin, method = "entropy"), 0)
  expect_equal(summ_distance(d_dirac, d_dirac, method = "entropy"), 0)
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
  # the case when the other one is "fin" with one of points lying inside "dirac"
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


# distance_totvar ---------------------------------------------------------
test_that("distance_totvar works with two 'fin' functions", {
  p_f <- new_p(1:3, "fin")
  p_g <- new_p(1:3+1.5, "fin")
  expect_equal(distance_totvar(p_f, p_g), 1)
  # Checking twice to test independence of argument order
  expect_equal(distance_totvar(p_g, p_f), 1)

  p_f <- new_p(data.frame(x = 1:4, prob = 1:4/10), "fin")
  p_g <- new_p(data.frame(x = 1:4, prob = 4:1/10), "fin")
  expect_equal(distance_totvar(p_f, p_g), 0.4)

  p_f <- new_p(1:10, "fin")
  p_g <- new_p(5:14, "fin")
  expect_equal(distance_totvar(p_f, p_g), 0.4)
})

test_that("distance_totvar works with mixed-type functions", {
  expect_equal(distance_totvar(d_fin, d_infin), 1)
  # Checking twice to test independence of argument order
  expect_equal(distance_totvar(d_infin, d_fin), 1)
})

test_that("distance_totvar works with two 'infin' functions", {
  # Multiple density intersections in real-world example
  p_f <- new_p(data.frame(x = c(1:3, 5, 7), y = c(0, 0.5, 0, 0.25, 0)), "infin")
  p_g <- new_p(
    data.frame(x = c(1:3, 5, 7) + 0.5, y = c(0, 0.5, 0, 0.25, 0)), "infin"
  )
  # `{x | d_f(x) > d_g(x)}` is a union of (1; 2.25) and (3.4; 5.25)
  out_ref <- ((p_f(2.25)-p_f(1)) + (p_f(5.25)-p_f(3.4))) -
    ((p_g(2.25)-p_g(1)) + (p_g(5.25)-p_g(3.4)))

  expect_equal(distance_totvar(p_f, p_g), out_ref)
  # Checking twice to test independence of argument order
  expect_equal(distance_totvar(p_g, p_f), out_ref)

  # Non-intersecting densities in real-world example
  p_f <- as_p(punif)
  p_g <- as_p(punif, min = -1, max = 3)
  expect_equal(distance_totvar(p_f, p_g), 0.75)

  # Common y-zero plateau #1
  p_f <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "infin")
  p_g <- new_p(data.frame(x = 0:5, y = c(0, 1, 0, 0, 1, 0)/2), "infin")
  # `{x | d_f(x) > d_g(x)}` is a union of (1; 2) and (3; 4)
  out_ref <- ((p_f(2)-p_f(1)) + (p_f(4)-p_f(3))) -
    ((p_g(2)-p_g(1)) + (p_g(4)-p_g(3)))
  expect_equal(distance_totvar(p_f, p_g), out_ref)

  # Common y-zero plateau #2
  p_f <- new_p(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "infin")
  p_g <- new_p(data.frame(x = 3:6, y = c(0, 0, 1, 0)), "infin")
  expect_equal(distance_totvar(p_f, p_g), 1)
})

test_that("distance_totvar returns zero with identical inputs", {
  expect_equal(distance_totvar(d_fin, d_fin), 0)
  expect_equal(distance_totvar(d_infin, d_infin), 0)

  d_dirac <- new_d(2, "infin")
  expect_equal(distance_totvar(d_dirac, d_dirac), 0)
})

test_that("distance_totvar works with non-overlapping supports", {
  cur_fin_1 <- new_p(1:4, "fin")
  cur_fin_2 <- new_p(5:6, "fin")
  cur_infin_1 <- new_p(data.frame(x = 1:4, y = c(1, 1)/3), "infin")
  cur_infin_2 <- new_p(data.frame(x = 5:6, y = c(1, 1)), "infin")
  cur_infin_3 <- new_p(data.frame(x = 4:5, y = c(1, 1)), "infin")

  # "Two fin"
  expect_equal(distance_totvar(cur_fin_1, cur_fin_2), 1)
  expect_equal(distance_totvar(cur_fin_2, cur_fin_1), 1)

  # "Two infin"
  expect_equal(distance_totvar(cur_infin_1, cur_infin_2), 1)
  expect_equal(distance_totvar(cur_infin_2, cur_infin_1), 1)
  # "Touching" supports
  expect_equal(distance_totvar(cur_infin_1, cur_infin_3), 1)
  expect_equal(distance_totvar(cur_infin_3, cur_infin_1), 1)
})

test_that("distance_totvar works with dirac-like functions", {
  # Total variation distance when "dirac" function is involved should be
  # essentially (but not exactly) the same as if it is replaced with
  # corresponding "fin" (except the case when the other one is "fin" with one of
  # points lying inside "dirac" support)
  d_dirac <- new_d(2, "infin")
  d_dirac_fin <- new_d(2, "fin")

  # "Two infin" case
  expect_equal(
    distance_totvar(d_infin, d_dirac), distance_totvar(d_infin, d_dirac_fin)
  )
  # Non-overlapping dirac-like functions
  expect_equal(distance_totvar(d_dirac, new_d(3, "infin")), 1)
})

test_that("distance_totvar works with different pdqr classes", {
  expect_equal(distance_totvar(d_fin, d_infin), distance_totvar(p_fin, q_infin))
})


# distance_totvar_two_infin -----------------------------------------------
# Tested in `distance_totvar()`


# distance_totvar_two_fin -------------------------------------------------
# Tested in `distance_totvar()`


# distance_wass -----------------------------------------------------------
test_that("distance_wass works with two 'fin' functions", {
  p_f <- new_p(1:3, "fin")
  p_g <- new_p(1:3+1.5, "fin")
  expect_equal(distance_wass(p_f, p_g), 1.5)
  # Checking twice to test independence of argument order
  expect_equal(distance_wass(p_g, p_f), 1.5)

  p_f <- new_p(data.frame(x = 1:4, prob = 1:4/10), "fin")
  p_g <- new_p(data.frame(x = 1:4, prob = 4:1/10), "fin")
  expect_equal(distance_wass(p_f, p_g), 0.3*3 + 0.1*1)

  p_f <- new_p(1:10, "fin")
  p_g <- new_p(6:10, "fin")
  expect_equal(distance_wass(p_f, p_g), 0.1*5*5)
})

test_that("distance_wass works with mixed-type functions", {
  cur_fin <- new_p(1:10, "fin")
  cur_infin <- new_p(data.frame(x = c(0, 10), y = c(1, 1)/10), "infin")
  # Total integral is multiple of squares of "triangles"
  expect_equal(distance_wass(cur_fin, cur_infin), 10*(1*0.1)/2)
  # Checking twice to test independence of argument order
  expect_equal(distance_wass(cur_infin, cur_fin), 10*(1*0.1)/2)

  expect_equal(
    distance_wass(
      new_p(data.frame(x = 1:2, y = c(1, 1)), "infin"), new_p(2, "fin")
    ),
    1*1/2
  )

  # Case of common CDF plateau (zero density plateau)
  p_fin_2 <- new_p(data.frame(x = 2:3, prob = c(0.5, 0.5)), "fin")
  p_infin_2 <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "infin")
  # Result is twice the integral of `-0.5*x^2 + 2*x - 1.5` (equation of first
  # third of CDF) from 1 to 2
  expect_equal(distance_wass(p_fin_2, p_infin_2), 2/3)

  p_fin_3 <- new_p(2.5, "fin")
  p_infin_3 <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "infin")
  # Result is equal to previous plus square of "infin" CDF from 2 to 3
  expect_equal(
    distance_wass(p_fin_3, p_infin_3), 2/3 + 0.5,
    tolerance = 4e-7
  )
})

test_that("distance_wass works with two 'infin' functions", {
  p_f <- new_p(data.frame(x = c(1:3, 5, 7), y = c(0, 0.5, 0, 0.25, 0)), "infin")
  p_g <- p_f + 0.5
  expect_equal(distance_wass(p_f, p_g), 0.5, tolerance = 5e-7)
  # Checking twice to test independence of argument order
  expect_equal(distance_wass(p_g, p_f), 0.5, tolerance = 5e-7)

  p_f <- as_p(punif)
  p_g <- as_p(punif, max = 0.5)
  # Result is sum of squares of two triangles
  expect_equal(distance_wass(p_f, p_g), (0.5*1/2-0.5*0.5/2) + 0.5*0.5/2)

  # Common y-zero density plateau #1
  p_f <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "infin")
  p_g <- new_p(data.frame(x = 0:5, y = c(0, 1, 0, 0, 1, 0)/2), "infin")
  expect_equal(distance_wass(p_f, p_g), 1/3, tolerance = 4e-8)

  # Common y-zero density plateau #2
  p_f <- new_p(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "infin")
  p_g <- new_p(data.frame(x = 3:6, y = c(0, 0, 1, 0)), "infin")
  # Result can be reasoned as distance at which "density triangle" should be
  # moved
  expect_equal(distance_wass(p_f, p_g), 3, tolerance = 7e-8)
})

test_that("distance_wass works with non-overlapping supports", {
  cur_fin_1 <- new_p(1:4, "fin")
  cur_fin_2 <- new_p(5:6, "fin")
  cur_infin_1 <- new_p(data.frame(x = 1:4, y = c(1, 1)/3), "infin")
  cur_infin_2 <- new_p(data.frame(x = 5:6, y = c(1, 1)), "infin")
  cur_infin_3 <- new_p(data.frame(x = 4:5, y = c(1, 1)), "infin")

  # "Two fin"
  # Result is square of 12 1x0.25 blocks
  expect_equal(distance_wass(cur_fin_1, cur_fin_2), 12*1*0.25)
  expect_equal(distance_wass(cur_fin_2, cur_fin_1), 12*1*0.25)

  # "Mixed-typed"
  expect_equal(
    distance_wass(cur_fin_1, cur_infin_2), 6/4+1+0.5,
    tolerance = 4e-6
  )
  expect_equal(
    distance_wass(cur_infin_2, cur_fin_1), 6/4+1+0.5,
    tolerance = 4e-6
  )
  # "Touching" supports
  expect_equal(distance_wass(cur_fin_1, cur_infin_3), 6/4+0.5)
  expect_equal(distance_wass(cur_infin_3, cur_fin_1), 6/4+0.5)

  # "Two infin"
  expect_equal(
    distance_wass(cur_infin_1, cur_infin_2), 3*1/2 + 1 + 0.5,
    tolerance = 4e-6
  )
  expect_equal(
    distance_wass(cur_infin_2, cur_infin_1), 3*1/2 + 1 + 0.5,
    tolerance = 4e-6
  )
  # "Touching" supports
  expect_equal(distance_wass(cur_infin_1, cur_infin_3), 3*1/2 + 0.5)
  expect_equal(distance_wass(cur_infin_3, cur_infin_1), 3*1/2 + 0.5)
})

test_that("distance_wass works for very distant distributions", {
  p_f <- as_p(punif, max = 1e4)
  p_g <- as_p(punif)
  expect_equal(
    distance_wass(p_f, p_g), (1-0.5e-4) + (1e4-1)*(1-1e-4)/2
  )
  # A correct result is 1e4 but seems like 1 is added due to
  # `stats::integrate()` behavior on wide intervals. In this situation it
  # happens on distance around 500 (more like around 460)
  expect_equal(distance_wass(p_g, p_g + 1e4), 1e4 + 1)
})

test_that("distance_wass works with dirac-like functions", {
  # Wasserstein distance when "dirac" function is involved should be essentially
  # (but not exactly) the same as if it is replaced with corresponding "fin"
  # (except the case when the other one is "fin" with one of points lying inside
  # "dirac" support)
  d_dirac <- new_d(2, "infin")
  d_dirac_fin <- new_d(2, "fin")

  # "Two infin" case
  expect_equal(
    distance_wass(d_infin, d_dirac),
    distance_wass(d_infin, d_dirac_fin)
  )
  # Non-overlapping dirac-like functions
  expect_equal(distance_wass(d_dirac, new_d(3, "infin")), 1 + 1e-8)
})

test_that("distance_wass works with different pdqr classes", {
  expect_equal(distance_wass(d_fin, d_infin), distance_wass(p_fin, q_infin))
})


# distance_cramer ---------------------------------------------------------
test_that("distance_cramer works with two 'fin' functions", {
  p_f <- new_p(1:3, "fin")
  p_g <- new_p(1:3+1.5, "fin")
  expect_equal(distance_cramer(p_f, p_g), 5*0.5/9 + 2*0.5*4/9)
  # Checking twice to test independence of argument order
  expect_equal(distance_cramer(p_g, p_f), 5*0.5/9 + 2*0.5*4/9)
})

test_that("distance_cramer works with mixed-type functions", {
  cur_fin <- new_p(1:10, "fin")
  cur_infin <- new_p(data.frame(x = c(0, 10), y = c(1, 1)/10), "infin")
  # Total integral is multiple of squares of "triangles"
  expect_equal(distance_cramer(cur_fin, cur_infin), 10/300)
  # Checking twice to test independence of argument order
  expect_equal(distance_cramer(cur_infin, cur_fin), 10/300)
})

test_that("distance_cramer works with two 'infin' functions", {
  p_f <- as_p(punif)
  p_g <- as_p(punif, max = 0.5)
  # Result is sum of squares of two triangles
  expect_equal(distance_cramer(p_f, p_g), 1/12)
  # Checking twice to test independence of argument order
  expect_equal(distance_cramer(p_g, p_f), 1/12)
})

test_that("distance_cramer works with non-overlapping supports", {
  cur_fin_1 <- new_p(1:4, "fin")
  cur_fin_2 <- new_p(5:6, "fin")
  cur_infin_1 <- new_p(data.frame(x = 1:4, y = c(1, 1)/3), "infin")
  cur_infin_2 <- new_p(data.frame(x = 5:6, y = c(1, 1)), "infin")
  cur_infin_3 <- new_p(data.frame(x = 4:5, y = c(1, 1)), "infin")

  # "Two fin"
  # Here 2.125 = 0.25^2 + 0.5^2 + 0.75^2 + 1^2 + 0.5^2
  expect_equal(distance_cramer(cur_fin_1, cur_fin_2), 2.125)
  expect_equal(distance_cramer(cur_fin_2, cur_fin_1), 2.125)

  # "Mixed-typed"
  # Here
  expect_equal(
    distance_cramer(cur_fin_1, cur_infin_2), 0.25^2+0.5^2+0.75^2+1^2 + 1/3,
    tolerance = 1e-7
  )
  expect_equal(
    distance_cramer(cur_infin_2, cur_fin_1), 0.25^2+0.5^2+0.75^2+1^2 + 1/3,
    tolerance = 1e-7
  )
  # "Touching" supports
  expect_equal(
    distance_cramer(cur_fin_1, cur_infin_3), 0.25^2+0.5^2+0.75^2 + 1/3
  )
  expect_equal(
    distance_cramer(cur_infin_3, cur_fin_1), 0.25^2+0.5^2+0.75^2 + 1/3
  )

  # "Two infin"
  expect_equal(
    distance_cramer(cur_infin_1, cur_infin_2), 1 + 1 + 1/3,
    tolerance = 1e-6
  )
  expect_equal(
    distance_cramer(cur_infin_2, cur_infin_1), 1 + 1 + 1/3,
    tolerance = 1e-6
  )
  # "Touching" supports
  expect_equal(distance_cramer(cur_infin_1, cur_infin_3), 1 + 1/3)
  expect_equal(distance_cramer(cur_infin_3, cur_infin_1), 1 + 1/3)
})

test_that("distance_cramer works for very distant distributions", {
  p_f <- as_p(punif, max = 1e4)
  p_g <- as_p(punif)
  # Accuracy is not very good probably because of second power
  expect_equal(
    distance_cramer(p_f, p_g), (1-1e-4)^2 * (1 + 10^4*(1-1e-4)) / 3,
    tolerance = 0.7
  )
  # A correct result is 1e4-1/3 but seems like 4/3 is added due to
  # `stats::integrate()` behavior on wide intervals. In this situation it
  # happens on distance around 500 (more like around 460)
  expect_equal(distance_cramer(p_g, p_g + 1e4), 1e4 + 1)
})

test_that("distance_cramer works with dirac-like functions", {
  # Cramer distance when "dirac" function is involved should be essentially
  # (but not exactly) the same as if it is replaced with corresponding "fin"
  # (except the case when the other one is "fin" with one of points lying inside
  # "dirac" support)
  d_dirac <- new_d(2, "infin")
  d_dirac_fin <- new_d(2, "fin")

  # "Two infin" case
  expect_equal(
    distance_cramer(d_infin, d_dirac),
    distance_cramer(d_infin, d_dirac_fin)
  )
  # Non-overlapping dirac-like functions
  expect_equal(distance_cramer(d_dirac, new_d(3, "infin")), 1 + 2e-8)
})

test_that("distance_cramer works with different pdqr classes", {
  expect_equal(distance_cramer(d_fin, d_infin), distance_cramer(p_fin, q_infin))
})


# distance_entropy --------------------------------------------------------
test_that("distance_entropy works with 'fin' functions", {
  expect_dist_entropy(new_d(1:10, "fin"), new_d(5:12, "fin"))
  expect_dist_entropy(
    as_d(dbinom, size = 10, prob = 0.4), as_d(dbinom, size = 10, prob = 0.7)
  )
})

test_that("distance_entropy works with 'infin' functions", {
  expect_dist_entropy(as_d(dunif, max = 0.5), as_d(dunif))
  expect_dist_entropy(as_d(dnorm), as_d(dnorm, sd = 0.1))
})

test_that("distance_entropy works with dirac-like functions", {
  expect_dist_entropy(d_infin, new_d(1, "infin"))
  d_winsor <- form_resupport(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "infin"), c(0.1, 0.7)
  )
  expect_dist_entropy(d_infin, d_winsor)
})

test_that("distance_entropy throws error functions with different types", {
  expect_error(distance_entropy(d_fin, d_infin), "`f`.*`g`.*type")
})

test_that("distance_entropy works with different pdqr classes", {
  expect_equal(
    distance_entropy(d_infin, d_infin-1), distance_entropy(p_infin, q_infin-1)
  )
})


# integrate_cdf_absdiff ---------------------------------------------------
# Tested in `distance_wass()` and `distance_cramer()`
