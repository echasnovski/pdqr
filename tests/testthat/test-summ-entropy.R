context("test-summ-entropy")


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
test_that("summ_entropy works with 'fin' functions", {
  expect_equal_stat(summ_entropy, stat_list[["binom"]], "entropy")

  # Output isn't exact because of tail trimming during `as_d()` and not exact
  # value of entropy in `stat_list[["pois"]][["entropy"]]`
  expect_equal_stat(summ_entropy, stat_list[["pois"]], "entropy", thres = 8e-4)
})

test_that("summ_entropy works with 'infin' functions", {
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
  expect_equal(summ_entropy(new_d(10, "fin")), 0)
  # Entropy of dirac-like "infin" function is computed from symmetric triangular
  # distribution with width 2*1e-8
  expect_equal(summ_entropy(new_d(10, "infin")), 0.5 + log(1e-8))
})

test_that("summ_entropy handles zero probabilities/densities", {
  cur_d_fin <- new_d(data.frame(x = 1:3, prob = c(0, 0.5, 0.5)), "fin")
  expect_equal(summ_entropy(cur_d_fin), -2*0.5*log(0.5))

  cur_d_infin <- new_d(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "infin")
  ref_d_infin <- new_d(data.frame(x = 1:3, y = c(0, 1, 0)), "infin")
  expect_equal(summ_entropy(cur_d_infin), summ_entropy(ref_d_infin))
})

test_that("summ_entropy validates input", {
  expect_error(summ_entropy("a"), "`f`.*function")
  expect_error(summ_entropy(function(x) {x}), "`f`.*pdqr")
})


# summ_entropy2 -----------------------------------------------------------
# More tests in `cross_entropy()`
test_that("summ_entropy2 works with 'fin' functions", {
  f <- new_d(1:4, "fin")
  g <- new_d(3:7, "fin")

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

test_that("summ_entropy2 works with 'infin' functions", {
  f <- new_d(data.frame(x = c(0, 4), y = c(1, 1)/4), "infin")
  g <- new_d(data.frame(x = c(-1, 9), y = c(1, 1)/10), "infin")

  # Method "relative"
  expect_relative_summ_entropy2_works(f, g)

  # Method "cross"
  expect_equal(summ_entropy2(f, g, method = "cross"), -4*0.25*log(0.1))
  expect_equal(
    summ_entropy2(g, f, method = "cross"), -4*0.1*log(0.25) + 6*0.1*max_entropy
  )
})

test_that("summ_entropy2 works with dirac-like 'infin' functions", {
  f <- new_d(data.frame(x = c(0, 4), y = c(1, 1)/4), "infin")
  g_dirac <- new_d(2, "infin")

  # Method "relative"
  expect_relative_summ_entropy2_works(f, g_dirac)

  # Method "cross"
  expect_equal(summ_entropy2(f, g_dirac, method = "cross"), max_entropy)
  expect_equal(summ_entropy2(g_dirac, f, method = "cross"), -log(0.25))
})

test_that("summ_entropy2 throws error on inputs of different types", {
  expect_error(summ_entropy2(d_fin, d_infin), "`f`.*`g`.*same type")
  expect_error(summ_entropy2(d_infin, d_fin), "`f`.*`g`.*same type")
})

test_that("summ_entropy2 uses `clip` argument", {
  f <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  g <- new_d(data.frame(x = 3:4, y = c(1, 1)), "infin")
  expect_equal(summ_entropy2(f, g, method = "cross", clip = exp(-10)), 10)
})

test_that("summ_entropy2 validates input", {
  expect_error(summ_entropy2("a", d_fin), "`f`.*function")
  expect_error(summ_entropy2(function(x) {x}, d_fin), "`f`.*pdqr")
  expect_error(summ_entropy2(d_fin, "a"), "`g`.*function")
  expect_error(summ_entropy2(d_fin, function(x) {x}), "`g`.*pdqr")
  expect_error(summ_entropy2(d_fin, d_fin, clip = "a"), "`clip`.*number")
  expect_error(summ_entropy2(d_fin, d_fin, clip = 1:2), "`clip`.*single")
  expect_error(summ_entropy2(d_fin, d_fin, clip = -1), "`clip`.*non-negative")
})


# cross_entropy -----------------------------------------------------------
test_that("cross_entropy works with 'fin' functions", {
  # Simple uniform discrete distribution
  f <- new_d(1:15, "fin")
  expect_equal(cross_entropy(f, f), -log(1/15))

  # Two distributions with different `x`s
  f <- new_d(1:4, "fin")
  g <- new_d(3:7, "fin")
  expect_equal(cross_entropy(f, g), -2*0.25*log(0.2) + 2*0.25*max_entropy)
  expect_equal(cross_entropy(g, f), -2*0.2*log(0.25) + 3*0.2*max_entropy)
})

test_that("cross_entropy works with 'infin' functions", {
  # Simple uniform distribution
  f <- new_d(data.frame(x = c(0, 4), y = c(1, 1)/4), "infin")
  g <- new_d(data.frame(x = c(-1, 9), y = c(1, 1)/10), "infin")
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

test_that("cross_entropy handles zero plateaus in 'infin' functions", {
  d_unif <- new_d(data.frame(x = c(1, 6), y = c(1, 1)/5), "infin")
  f <- new_d(data.frame(x = 1:4, y = c(0, 1, 0, 0)), "infin")
  g <- new_d(data.frame(x = 3:6, y = c(0, 0, 1, 0)), "infin")
  expect_equal(cross_entropy(f, d_unif), -log(0.2))
  # `-integral{log(f(x))}` on `[1; 3]` equals 2
  expect_equal(cross_entropy(d_unif, f), 0.2*2 + 3*0.2*max_entropy)
  # These are equal to `max_entropy` because `f` and `g` intersect only on
  # zero plateau.
  expect_equal(cross_entropy(f, g), max_entropy)
  expect_equal(cross_entropy(g, f), max_entropy)
})

test_that("cross_entropy works with dirac-like 'infin' functions", {
  f <- new_d(data.frame(x = c(0, 4), y = c(1, 1)/4), "infin")
  g_dirac <- new_d(2, "infin")
  expect_equal(cross_entropy(f, g_dirac), max_entropy)
  expect_equal(cross_entropy(g_dirac, f), -log(0.25))

  # Entropy of dirac-like function is computed from symmetric triangular
  # distribution with width 2*1e-8
  expect_equal(cross_entropy(g_dirac, g_dirac), 0.5 + log(1e-8))
})

test_that("cross_entropy returns maximum value with non-overlapping supports", {
  # Type "fin"
  f_fin <- new_d(1:4, "fin")
  g_fin <- new_d(5:8, "fin")
  expect_equal(cross_entropy(f_fin, g_fin, clip = exp(-20)), 20)
  expect_equal(cross_entropy(g_fin, f_fin, clip = exp(-20)), 20)

  expect_equal(cross_entropy(f_fin, g_fin, clip = exp(-10)), 10)

  # Type "infin"
  f_infin <- new_d(data.frame(x = 1:2, y = c(1, 1)), "infin")
  g_infin <- new_d(data.frame(x = 2:3, y = c(1, 1)), "infin")
  h_infin <- new_d(data.frame(x = 3:4, y = c(1, 1)), "infin")
  expect_equal(cross_entropy(f_infin, g_infin, clip = exp(-20)), 20)
  expect_equal(cross_entropy(g_infin, f_infin, clip = exp(-20)), 20)

  expect_equal(cross_entropy(f_infin, h_infin, clip = exp(-20)), 20)
  expect_equal(cross_entropy(h_infin, f_infin, clip = exp(-20)), 20)

  expect_equal(cross_entropy(f_infin, g_infin, clip = exp(-10)), 10)
})

test_that("cross_entropy throws error on inputs of different types", {
  expect_error(cross_entropy(d_fin, d_infin), "`f`.*`g`.*same type")
  expect_error(cross_entropy(d_infin, d_fin), "`f`.*`g`.*same type")
})


# cross_entropy_fin -------------------------------------------------------
# Tested in `cross_entropy()`


# cross_entropy_infin -----------------------------------------------------
# Tested in `cross_entropy()`
