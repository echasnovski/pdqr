context("test-summ_separation")


# Input data --------------------------------------------------------------
classmetric_sep_methods <- c("GM", "OP", "F1", "MCC")


# summ_separation ---------------------------------------------------------
test_that("summ_separation works with method 'KS'", {
  # More thorough tests are done in `separation_ks()`
  # Checking everything twice to test independece of result from function order

  # Two "discrete" functions
  p_f <- new_p(data.frame(x = 1:4, prob = 1:4/10), "discrete")
  p_g <- new_p(data.frame(x = 1:4, prob = 4:1/10), "discrete")
  expect_equal(summ_separation(p_f, p_g, method = "KS"), 2)
  expect_equal(summ_separation(p_g, p_f, method = "KS"), 2)

  # Mixed types
  p_f <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "continuous")
  p_g <- new_p(
    data.frame(x = c(1.5, 2.5, 4.5), prob = c(0.1, 0.7, 0.3)), "discrete"
  )
  expect_equal(summ_separation(p_f, p_g, method = "KS"), 2)
  expect_equal(summ_separation(p_g, p_f, method = "KS"), 2)

  # Two "continuous" functions
  p_f <- new_p(data.frame(x = c(0, 1), y = c(0, 2)), "continuous")
  p_g <- new_p(data.frame(x = c(0, 1), y = c(2, 0)), "continuous")
  expect_equal(summ_separation(p_f, p_g, method = "KS"), 0.5)
  expect_equal(summ_separation(p_g, p_f, method = "KS"), 0.5)
})

test_that("summ_separation works with methods from `summ_classmetric`", {
  # More thorough tests are done in `separation_classmetric()`
  # Checking everything twice to test independece of result from function order

  # Two "discrete" functions
  p_f <- new_d(1:4, "discrete")
  p_g <- new_d(2:6+0.1, "discrete")
  expect_equal(summ_separation(p_f, p_g, "GM"), 3, tolerance = 4e-4)
  expect_equal(summ_separation(p_g, p_f, "GM"), 3, tolerance = 4e-4)

  # Mixed types
  p_f <- new_d(seq(-5, 4, by = 1), "discrete")
  p_g <- as_d(dnorm)
  expect_equal(summ_separation(p_f, p_g, "GM"), -1, tolerance = 1e-3)
  expect_equal(summ_separation(p_g, p_f, "GM"), -1, tolerance = 1e-3)

  # Two "continuous" functions
  p_f <- as_d(dunif, min = -2)
  p_g <- as_d(dnorm)
  expect_equal(summ_separation(p_f, p_g, "GM"), -0.331789, tolerance = 3e-8)
  expect_equal(summ_separation(p_g, p_f, "GM"), -0.331789, tolerance = 3e-8)
})

test_that("summ_separation returns smallest among alternatives", {
  d_1 <- new_d(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "continuous")
  d_2 <- new_d(data.frame(x = 3:6, y = c(0, 0, 1, 0)), "continuous")

  expect_equal(summ_separation(d_1, d_2, "KS"), 3)
  expect_equal(summ_separation(d_1, d_2, "GM"), 3)
  expect_equal(summ_separation(d_1, d_2, "OP"), 3)
  expect_equal(summ_separation(d_1, d_2, "F1"), 3)
  expect_equal(summ_separation(d_1, d_2, "MCC"), 3)
})

test_that("summ_separation uses `n_grid` argument", {
  f_con <- new_d(data.frame(x = c(0, 1), y = c(1, 1)), "continuous")
  g_con <- new_d(data.frame(x = c(0, 1.5), y = c(1, 1)/1.5), "continuous")
  expect_equal(
    # Correct answer is 0.75 but it is not very near any grid element
    summ_separation(f_con, g_con, method = "GM", n_grid = 4), 0.5
  )
})

test_that("summ_separation works with non-overlapping supports", {
  cur_dis_1 <- new_p(1:4, "discrete")
  cur_dis_2 <- new_p(5:6, "discrete")
  cur_con_1 <- new_p(data.frame(x = 1:4, y = c(1, 1)/3), "continuous")
  cur_con_2 <- new_p(data.frame(x = 5:6, y = c(1, 1)), "continuous")
  cur_con_3 <- new_p(data.frame(x = 4:5, y = c(1, 1)), "continuous")
  cur_dirac_1 <- new_d(1, "continuous")
  cur_dirac_2 <- new_d(2, "continuous")

  # "Two discrete"
  expect_equal(summ_separation(cur_dis_1, cur_dis_2), 4.5)
  expect_equal(summ_separation(cur_dis_2, cur_dis_1), 4.5)
    # "Touching" supports
  expect_equal(
    summ_separation(new_p(1:2, "discrete"), new_p(2:3, "discrete")), 2
  )
  expect_equal(
    summ_separation(new_p(2:3, "discrete"), new_p(1:2, "discrete")), 2
  )

  # "Mixed-typed"
  expect_equal(summ_separation(cur_dis_1, cur_con_2), 4.5)
  expect_equal(summ_separation(cur_con_2, cur_dis_1), 4.5)
    # "Touching" supports
  expect_equal(summ_separation(cur_dis_1, cur_con_3), 4)
  expect_equal(summ_separation(cur_con_3, cur_dis_1), 4)

  # "Two continuous"
  expect_equal(summ_separation(cur_con_1, cur_con_2), 4.5)
  expect_equal(summ_separation(cur_con_2, cur_con_1), 4.5)
    # "Touching" supports
  expect_equal(summ_separation(cur_con_1, cur_con_3), 4)
  expect_equal(summ_separation(cur_con_3, cur_con_1), 4)

  # Dirac-like functions
  expect_equal(summ_separation(cur_dirac_1, cur_dirac_2), 1.5)
  expect_equal(summ_separation(cur_dirac_2, cur_dirac_1), 1.5)
})

test_that("summ_separation validates input", {
  expect_error(summ_separation("a", d_dis), "`f`.*not pdqr-function")
  expect_error(summ_separation(d_dis, "a"), "`g`.*not pdqr-function")
  expect_error(summ_separation(d_dis, d_con, method = 1), "`method`.*string")
  expect_error(
    summ_separation(d_dis, d_con, method = "a"), "`method`.*one of"
  )
  expect_error(
    summ_separation(d_dis, d_con, n_grid = "a"), "`n_grid`.*number"
  )
  expect_error(
    summ_separation(d_dis, d_con, n_grid = 1:2), "`n_grid`.*single"
  )
})


# separation_ks -----------------------------------------------------------
test_that("separation_ks works with two 'discrete' functions", {
  # Returns the smallest value on which maximum of |F - G| is located
  p_f <- new_d(1:3, "discrete")
  p_g <- new_d(1:3+1.5, "discrete")
  expect_equal(separation_ks(p_f, p_g), 3)
  # Checking twice to test independence of argument order
  expect_equal(separation_ks(p_g, p_f), 3)

  expect_equal(
    separation_ks(
      new_p(data.frame(x = 1:4, prob = 1:4/10), "discrete"),
      new_p(data.frame(x = 1:4, prob = 4:1/10), "discrete")
    ),
    2
  )

  p_f <- as_p(ppois, lambda = 10)
  p_g <- as_p(ppois, lambda = 5)
  expect_equal(separation_ks(p_f, p_g), 7)
})

test_that("separation_ks works with mixed-type functions", {
  # These two cases represent "supremum-not-maximum" quality of K-S distatnce,
  # when actual distance is achieved as limit of distances from left side
  cur_dis <- new_p(1:10, "discrete")
  cur_con <- new_p(data.frame(x = c(0, 10), y = c(1, 1)/10), "continuous")
  expect_equal(separation_ks(cur_dis, cur_con), 1)
  # Checking twice to test independence of argument order
  expect_equal(separation_ks(cur_con, cur_dis), 1)

  expect_equal(
    separation_ks(
      new_p(data.frame(x = 1:2, y = c(1, 1)), "continuous"),
      new_p(2, "discrete")
    ),
    2
  )

  # Test that the smallest "x" value is returned in case of several candidates
  p_dis_2 <- new_p(data.frame(x = 2:3, prob = c(0.5, 0.5)), "discrete")
  p_con_2 <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "continuous")
  expect_equal(separation_ks(p_dis_2, p_con_2), 2)

  # Case when smallest "x" value with maximum absolute CDF difference is one
  # of "x" values from "x_tbl" of "continuous" pdqr-function
  p_dis_3 <- new_p(2.5, "discrete")
  p_con_3 <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "continuous")
  expect_equal(separation_ks(p_dis_3, p_con_3), 2)
})

test_that("separation_ks works with two 'continuous' functions", {
  # Maximum at density crossings
  p_f <- new_p(data.frame(x = 0:1, y = c(2, 0)), "continuous")
  p_g <- new_p(data.frame(x = c(0.5, 1, 1.5), y = c(0, 2, 0)), "continuous")
  expect_equal(separation_ks(p_f, p_g), 2/3)
  # Checking twice to test independence of argument order
  expect_equal(separation_ks(p_g, p_f), 2/3)

  # Multiple density crossings in real-world example
  p_f <- new_p(
    data.frame(x = c(1:3, 5, 7), y = c(0, 0.5, 0, 0.25, 0)), "continuous"
  )
  p_g <- new_p(
    data.frame(x = c(1:3, 5, 7) + 0.5, y = c(0, 0.5, 0, 0.25, 0)), "continuous"
  )
  expect_equal(separation_ks(p_f, p_g), 2.25)

  # Non-crossing densities in real-world example
  p_f <- as_p(punif)
  p_g <- as_p(punif, min = -1, max = 3)
  expect_equal(separation_ks(p_f, p_g), 1)

  # Common y-zero plateau. Maximum difference on both edges of one of support.
  p_f <- new_p(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "continuous")
  p_g <- new_p(data.frame(x = 0:5, y = c(0, 1, 0, 0, 1, 0)/2), "continuous")
  # Output should be the smallest x value
  expect_equal(separation_ks(p_f, p_g), 1)

  # Common y-zero plateau. Maximum difference on edge of plateau.
  p_f <- new_p(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "continuous")
  p_g <- new_p(data.frame(x = 3:6, y = c(0, 0, 1, 0)), "continuous")
  expect_equal(separation_ks(p_f, p_g), 3)
})

test_that("separation_ks works with identical inputs", {
  expect_equal(separation_ks(d_dis, d_dis), meta_support(d_dis)[1])
  expect_equal(separation_ks(d_con, d_con), meta_support(d_con)[1])

  d_dirac <- new_d(2, "continuous")
  expect_equal(
    separation_ks(d_dirac, d_dirac), meta_support(d_dirac)[1],
    tolerance = 1e-9
  )
})

test_that("separation_ks works with dirac-like functions", {
  # K-S distance when "dirac" function is involved should be essentially (but
  # not exactly) the same as if it is replaced with corresponding "discrete"
  # (except the case when the other one is "discrete" with one of points lying
  # inside "dirac" support)
  d_dirac <- new_d(2, "continuous")
  d_dirac_dis <- new_d(2, "discrete")

  # "Mixed-type" case
  expect_equal(separation_ks(d_dis, d_dirac), separation_ks(d_dis, d_dirac_dis))
  expect_equal(separation_ks(d_dirac, d_dirac_dis), 2, tolerance = 1e-10)

  # "Two continuous"
  expect_equal(
    separation_ks(d_con, d_dirac), separation_ks(d_con, d_dirac_dis)
  )
})


# separation_ks_two_dis ---------------------------------------------------
# Tested in `separation_ks()`


# separation_ks_mixed -----------------------------------------------------
# Tested in `separation_ks()`


# separation_ks_two_con ---------------------------------------------------
# Tested in `separation_ks()`


# separation_classmetric --------------------------------------------------
test_that("separation_classmetric works with two 'discrete' functions", {
  f_dis <- new_d(1:4, "discrete")
  g_dis <- new_d(2:6+0.1, "discrete")

  separations <- vapply(
    classmetric_sep_methods, separation_classmetric, numeric(1),
    f = f_dis, g = g_dis
  )
  expect_equal(
    separations, c(GM = 3, OP = 3, F1 = 2, MCC = 4), tolerance = 5e-4
  )
})

test_that("separation_classmetric works with mixed-type functions", {
  f_dis <- new_d(seq(-5, 4, by = 1), "discrete")
  g_con <- as_d(dnorm)

  separations <- vapply(
    classmetric_sep_methods, separation_classmetric, numeric(1),
    f = f_dis, g = g_con
  )
  expect_equal(
    separations, c(GM = -1, OP = 0, F1 = -2, MCC = -2), tolerance = 5e-4
  )
})

test_that("separation_classmetric works with two 'continuous' functions", {
  f_con <- as_d(dunif, min = -2)
  g_con <- as_d(dnorm)

  separations <- vapply(
    classmetric_sep_methods, separation_classmetric, numeric(1),
    f = f_con, g = g_con
  )
  expect_equal(
    separations[c("GM", "OP", "F1")],
    c(GM = -0.331789, OP = -0.2291151, F1 = -1.3043396),
    tolerance = 6e-8
  )
  expect_equal(separations["MCC"], c(MCC = 1), tolerance = 2e-4)
})

test_that("separation_classmetric works with different pdqr classes", {
  expect_equal(
    separation_classmetric(d_dis, d_con, "GM"),
    separation_classmetric(p_dis, q_con, "GM")
  )
})
