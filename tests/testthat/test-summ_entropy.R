context("test-summ_entropy")


# Input data --------------------------------------------------------------
def_clip <- exp(-20)
max_entropy <- -log(def_clip)


# Custom expectations -----------------------------------------------------
expect_relative_summ_entropy2_works <- function(f, g) {
  expect_equal(
    summ_entropy2(f, g, method = "relative"),
    summ_entropy2(f, g, method = "cross") - summ_entropy(f)
  )
  expect_equal(
    summ_entropy2(g, f, method = "relative"),
    summ_entropy2(g, f, method = "cross") - summ_entropy(g)
  )
}


# summ_entropy ------------------------------------------------------------
# There are also tests in `cross_entropy()`
test_that("summ_entropy works with 'discrete' functions", {
  expect_equal_stat(summ_entropy, stat_list[["binom"]], "entropy")

  # Output isn't exact because of tail trimming during `as_d()` and not exact
  # value of entropy in `stat_list[["pois"]][["entropy"]]`
  expect_equal_stat(summ_entropy, stat_list[["pois"]], "entropy", thres = 8e-4)
})

test_that("summ_entropy works with 'continuous' functions", {
  expect_equal_stat(summ_entropy, stat_list[["beta"]], "entropy", thres = 3e-5)
  # Big threshold because original density goes to infinity at edges
  expect_equal_stat(
    summ_entropy, stat_list[["beta_inf"]], "entropy", thres = 0.25
  )
  expect_equal_stat(summ_entropy, stat_list[["chisq"]], "entropy", thres = 2e-5)
  # Big threshold because original density goes to infinity at left edge
  expect_equal_stat(
    summ_entropy, stat_list[["chisq_inf"]], "entropy", thres = 8e-2
  )
  expect_equal_stat(summ_entropy, stat_list[["exp"]], "entropy", thres = 2e-5)
  expect_equal_stat(summ_entropy, stat_list[["norm"]], "entropy", thres = 4e-5)
  expect_equal_stat(
    summ_entropy, stat_list[["norm_2"]], "entropy", thres = 4e-5
  )
  expect_equal_stat(summ_entropy, stat_list[["unif"]], "entropy")
})

test_that("summ_entropy works with dirac-like functions", {
  expect_equal(summ_entropy(new_d(10, "discrete")), 0)
  # Entropy of dirac-like "continuous" function is computed from symmetric
  # triangular distribution with width 2*1e-8
  expect_equal(summ_entropy(new_d(10, "continuous")), 0.5 + log(1e-8))
})

test_that("summ_entropy handles zero probabilities/densities", {
  cur_d_dis <- new_d(data.frame(x = 1:3, prob = c(0, 0.5, 0.5)), "discrete")
  expect_equal(summ_entropy(cur_d_dis), -2*0.5*log(0.5))

  cur_d_con <- new_d(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "continuous")
  ref_d_con <- new_d(data.frame(x = 1:3, y = c(0, 1, 0)), "continuous")
  expect_equal(summ_entropy(cur_d_con), summ_entropy(ref_d_con))
})

test_that("summ_entropy validates input", {
  expect_error(summ_entropy("a"), "`f`.*not pdqr-function")
})


# summ_entropy2 -----------------------------------------------------------
# More tests in `cross_entropy()`
test_that("summ_entropy2 works with 'discrete' functions", {
  f <- new_d(1:4, "discrete")
  g <- new_d(3:7, "discrete")

  # Method "relative"
  expect_relative_summ_entropy2_works(f, g)

  # Method "cross"
  expect_equal(
    summ_entropy2(f, g, method = "cross"), -2*0.25*log(0.2) + 2*0.25*max_entropy
  )
  expect_equal(
    summ_entropy2(g, f, method = "cross"), -2*0.2*log(0.25) + 3*0.2*max_entropy
  )
})

test_that("summ_entropy2 works with 'continuous' functions", {
  f <- new_d(data.frame(x = c(0, 4), y = c(1, 1)/4), "continuous")
  g <- new_d(data.frame(x = c(-1, 9), y = c(1, 1)/10), "continuous")

  # Method "relative"
  expect_relative_summ_entropy2_works(f, g)

  # Method "cross"
  expect_equal(summ_entropy2(f, g, method = "cross"), -4*0.25*log(0.1))
  expect_equal(
    summ_entropy2(g, f, method = "cross"), -4*0.1*log(0.25) + 6*0.1*max_entropy
  )
})

test_that("summ_entropy2 works with dirac-like 'continuous' functions", {
  f <- new_d(data.frame(x = c(0, 4), y = c(1, 1)/4), "continuous")
  g_dirac <- new_d(2, "continuous")

  # Method "relative"
  expect_relative_summ_entropy2_works(f, g_dirac)

  # Method "cross"
  expect_equal(summ_entropy2(f, g_dirac, method = "cross"), max_entropy)
  expect_equal(summ_entropy2(g_dirac, f, method = "cross"), -log(0.25))
})

test_that("summ_entropy2 handles numerical representation accuracy", {
  # More info in corresponding `cross_entropy()` test
  approx_unif <- new_d(
    data.frame(x = 0:1, y = c(0.5-0.3, 0.3-0.1)/2), "continuous"
  )
  beta <- as_d(dbeta, shape1 = 1, shape2 = 2)

  expect_equal(summ_entropy2(beta, approx_unif, method = "cross"), 0)
})

test_that("summ_entropy2 throws error on inputs of different types", {
  expect_error(summ_entropy2(d_dis, d_con), "`f`.*`g`.*same type")
  expect_error(summ_entropy2(d_con, d_dis), "`f`.*`g`.*same type")
})

test_that("summ_entropy2 uses `clip` argument", {
  f <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  g <- new_d(data.frame(x = 3:4, y = c(1, 1)), "continuous")
  expect_equal(summ_entropy2(f, g, method = "cross", clip = exp(-10)), 10)
})

test_that("summ_entropy2 validates input", {
  expect_error(summ_entropy2("a", d_dis), "`f`.*not pdqr-function")
  expect_error(summ_entropy2(d_dis, "a"), "`g`.*not pdqr-function")
  expect_error(summ_entropy2(d_dis, d_dis, clip = "a"), "`clip`.*number")
  expect_error(summ_entropy2(d_dis, d_dis, clip = 1:2), "`clip`.*single")
  expect_error(summ_entropy2(d_dis, d_dis, clip = -1), "`clip`.*non-negative")
})


# cross_entropy -----------------------------------------------------------
test_that("cross_entropy works with 'discrete' functions", {
  # Simple uniform discrete distribution
  f <- new_d(1:15, "discrete")
  expect_equal(cross_entropy(f, f), -log(1/15))

  # Two distributions with different `x`s
  f <- new_d(1:4, "discrete")
  g <- new_d(3:7, "discrete")
  expect_equal(cross_entropy(f, g), -2*0.25*log(0.2) + 2*0.25*max_entropy)
  expect_equal(cross_entropy(g, f), -2*0.2*log(0.25) + 3*0.2*max_entropy)
})

test_that("cross_entropy works with 'continuous' functions", {
  # Simple uniform distribution
  f <- new_d(data.frame(x = c(0, 4), y = c(1, 1)/4), "continuous")
  g <- new_d(data.frame(x = c(-1, 9), y = c(1, 1)/10), "continuous")
  expect_equal(cross_entropy(f, f), log(4))
  expect_equal(cross_entropy(g, g), log(10))
  expect_equal(cross_entropy(f, g), -4*0.25*log(0.1))
  expect_equal(cross_entropy(g, f), -4*0.1*log(0.25) + 6*0.1*max_entropy)

  # Real world examples
  f <- as_d(dbeta, shape1 = 1, shape2 = 2, support = c(0, 1))
  g <- as_d(dbeta, shape1 = 2, shape2 = 1, support = c(0, 1))
  expect_equal(cross_entropy(f, f), log(beta(1, 2)) - psigamma(2) + psigamma(3))
  expect_equal(cross_entropy(g, g), log(beta(2, 1)) - psigamma(2) + psigamma(3))
  expect_equal(cross_entropy(f, g), 1.5-log(2))
  expect_equal(cross_entropy(g, f), 1.5-log(2))
})

test_that("cross_entropy handles zero plateaus in 'continuous' functions", {
  d_unif <- new_d(data.frame(x = c(1, 6), y = c(1, 1)/5), "continuous")
  f <- new_d(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "continuous")
  g <- new_d(data.frame(x = 3:6, y = c(0, 0, 1, 0)), "continuous")
  expect_equal(cross_entropy(f, d_unif), -log(0.2))
  # `-integral{log(f(x))}` on `[1; 3]` equals 2
  expect_equal(cross_entropy(d_unif, f), 0.2*2 + 3*0.2*max_entropy)
  # These are equal to `max_entropy` because `f` and `g` intersect only on
  # zero plateau.
  expect_equal(cross_entropy(f, g), max_entropy)
  expect_equal(cross_entropy(g, f), max_entropy)
})

test_that("cross_entropy works with dirac-like 'continuous' functions", {
  f <- new_d(data.frame(x = c(0, 4), y = c(1, 1)/4), "continuous")
  g_dirac <- new_d(2, "continuous")
  expect_equal(cross_entropy(f, g_dirac), max_entropy)
  expect_equal(cross_entropy(g_dirac, f), -log(0.25))

  # Entropy of dirac-like function is computed from symmetric triangular
  # distribution with width 2*1e-8
  expect_equal(cross_entropy(g_dirac, g_dirac), 0.5 + log(1e-8))
})

test_that("cross_entropy returns maximum value with non-overlapping supports", {
  # Type "discrete"
  f_dis <- new_d(1:4, "discrete")
  g_dis <- new_d(5:8, "discrete")
  expect_equal(cross_entropy(f_dis, g_dis, clip = exp(-20)), 20)
  expect_equal(cross_entropy(g_dis, f_dis, clip = exp(-20)), 20)

  expect_equal(cross_entropy(f_dis, g_dis, clip = exp(-10)), 10)

  # Type "continuous"
  f_con <- new_d(data.frame(x = 1:2, y = c(1, 1)), "continuous")
  g_con <- new_d(data.frame(x = 2:3, y = c(1, 1)), "continuous")
  h_con <- new_d(data.frame(x = 3:4, y = c(1, 1)), "continuous")
  expect_equal(cross_entropy(f_con, g_con, clip = exp(-20)), 20)
  expect_equal(cross_entropy(g_con, f_con, clip = exp(-20)), 20)

  expect_equal(cross_entropy(f_con, h_con, clip = exp(-20)), 20)
  expect_equal(cross_entropy(h_con, f_con, clip = exp(-20)), 20)

  expect_equal(cross_entropy(f_con, g_con, clip = exp(-10)), 10)
})

test_that("cross_entropy handles numerical representation accuracy", {
  # Here `y` values are not **exactly** equal but differ by ~1.38e-17 (on most
  # platforms, according to "Note" from `==`'s help page).
  approx_unif <- new_d(
    data.frame(x = 0:1, y = c(0.5-0.3, 0.3-0.1)/2), "continuous"
  )
  beta <- as_d(dbeta, shape1 = 1, shape2 = 2)

  # When this type of issue is in place, one of the previous implementations
  # checked for slope of second function being equal to 0 as `sl_g != 0`. This
  # returned `FALSE` and proceeded with great errors because `sl_g` is used in
  # fraction denominator several times. This was fixed by using
  # `!is_near(sl_g, 0, tol = 1e-10)` instead.

  # Here output should be equal to 0 because `log(1)` is zero
  expect_equal(cross_entropy(beta, approx_unif), 0)
})

test_that("cross_entropy throws error on inputs of different types", {
  expect_error(cross_entropy(d_dis, d_con), "`f`.*`g`.*same type")
  expect_error(cross_entropy(d_con, d_dis), "`f`.*`g`.*same type")
})


# cross_entropy_dis -------------------------------------------------------
# Tested in `cross_entropy()`


# cross_entropy_con -------------------------------------------------------
# Tested in `cross_entropy()`
