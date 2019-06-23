context("test-summ_order")


# summ_order --------------------------------------------------------------
test_that("summ_order works", {
  # General case
  mean_vec <- runif(4)
  f_list <- lapply(mean_vec, function(m) {as_d(dnorm, mean = m)})
  ref_order <- order(mean_vec)

  for (meth in c("compare", "mean", "median", "mode")) {
    expect_equal(summ_order(f_list, method = meth), ref_order)
  }

  # Works with different types
  f_list <- list(as_d(dnorm), new_d(1:10, "discrete"))
  for (meth in c("compare", "mean", "median", "mode")) {
    expect_equal(summ_order(f_list, method = meth), 1:2)
  }
})

test_that("summ_order works with method 'compare'", {
  # Case of different order compared to "mean". Here `P(f >= g) < 0.5` (`f` and
  # `g` are first and second elements of `non_mean_list`) but `summ_mean(f) >=
  # summ_mean(g)` is `TRUE`. So `f` should be considered as being "less" than g.
  non_mean_list <- list(
    new_d(data.frame(x = c(0.56, 0.815), y = c(1, 1)), "continuous"),
    new_d(data.frame(x = 0:1, y = c(0, 1)), "continuous")
  )
  expect_equal(summ_order(non_mean_list, method = "compare"), c(1, 2))

  # Case of non-transitive triplet. Here first element is less than second
  # (`summ_prob_true(f_list[[1]] <= f_list[[2]])` is more than 0.5), second is
  # less than third, but third is less than first.
  non_trans_list <- list(
    new_d(data.frame(x = c(0.39, 0.44, 0.46), y = c(17, 14, 0)), "continuous"),
    new_d(data.frame(x = c(0.05, 0.3, 0.70), y = c(4, 0, 4)), "continuous"),
    new_d(data.frame(x = c(0.03, 0.40, 0.80), y = c(1, 1, 1)), "continuous")
  )
    # One important feature here should be independence of output ordering
    # from order of input, i.e. eventual sorting should be the same
  expect_equal(
    summ_order(non_trans_list, method = "compare"),
    summ_order(non_trans_list[c(2, 3, 1)])[c(2, 3, 1)]
  )

  # Case of equivalent pdqr-functions (`P(f >= g) = 0.5` for all pairs).
  equi_list <- list(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
    new_d(data.frame(x = c(0, 0.5, 1), y = c(0, 1, 0)), "continuous"),
    new_d(data.frame(x = c(0, 0.5, 1), y = c(1, 0, 1)), "continuous"),
    new_d(c(0, 0.5, 1), "discrete")
  )
  # Input order should be preserved
  expect_equal(summ_order(equi_list, method = "compare"), 1:4)
  expect_equal(summ_order(equi_list[c(2, 3, 4, 1)], method = "compare"), 1:4)
})

test_that("summ_order works with methods from `summ_center()`", {
  # Mean of log-normal distribution is `exp(m + 0.5*s^2)`, median - `exp(m)`,
  # mode - `exp(m - s^2)`, where `m` is `meanlog` and `s` is `sdlog`.
  m <- c(0, 0.2, 0.1)
  s <- c(1.1, 1.2, 1.3)

  f_list <- lapply(seq_along(m), function(i) {
    as_d(dlnorm, meanlog = m[i], sdlog = s[i])
  })

  expect_equal(summ_order(f_list, method = "mean"), order(exp(m + 0.5*s^2)))
  expect_equal(summ_order(f_list, method = "median"), order(exp(m)))
  expect_equal(summ_order(f_list, method = "mode"), order(exp(m - s^2)))
})

test_that("summ_order uses `decreasing` argument", {
  f_list <- list(d_dis, d_dis + 1)

  expect_equal(
    summ_order(f_list, method = "compare", decreasing = TRUE),
    rev(summ_order(f_list, method = "compare", decreasing = FALSE))
  )
  expect_equal(
    summ_order(f_list, method = "mean", decreasing = TRUE),
    rev(summ_order(f_list, method = "mean", decreasing = FALSE))
  )
})

test_that("summ_order works with list of length 1", {
  expect_equal(summ_order(list(d_dis), method = "compare"), 1)
  expect_equal(summ_order(list(d_dis), method = "mean"), 1)
})

test_that("summ_order works with different pdqr classes", {
  expect_equal(
    summ_order(list(d_dis, d_con)), summ_order(list(p_dis, q_con))
  )
})

test_that("summ_order validates input", {
  expect_error(summ_order("a"), "`f_list`.*list")
  expect_error(summ_order(list()), "`f_list`.*empty")
  expect_error(summ_order(list(function(x) {x})), "`f_list`.*pdqr")
  expect_error(summ_order(list(d_dis), method = 1), "`method`.*string")
  expect_error(summ_order(list(d_dis), method = "a"), "`method`.*one of")
})


# summ_sort ---------------------------------------------------------------
# More thorough testing is done in `summ_order()`
test_that("summ_sort works", {
  # General case
  mean_vec <- runif(4)
  f_list <- lapply(mean_vec, function(m) {as_d(dnorm, mean = m)})
  ref_list <- f_list[order(mean_vec)]

  for (meth in c("compare", "mean", "median", "mode")) {
    expect_equal(summ_sort(f_list, method = meth), ref_list)
  }

  # Works with different types
  f_list <- list(as_d(dnorm), new_d(1:10, "discrete"))
  ref_list <- f_list
  for (meth in c("compare", "mean", "median", "mode")) {
    expect_equal(summ_sort(f_list, method = meth), ref_list)
  }
})

test_that("summ_sort uses `decreasing` argument", {
  f_list <- list(d_dis, d_dis + 1)

  expect_equal(
    summ_sort(f_list, method = "compare", decreasing = TRUE),
    rev(summ_sort(f_list, method = "compare", decreasing = FALSE))
  )
  expect_equal(
    summ_sort(f_list, method = "mean", decreasing = TRUE),
    rev(summ_sort(f_list, method = "mean", decreasing = FALSE))
  )
})

test_that("summ_sort validates input", {
  expect_error(summ_sort("a"), "`f_list`.*list")
  expect_error(summ_sort(list()), "`f_list`.*empty")
  expect_error(summ_sort(list(function(x) {x})), "`f_list`.*pdqr")
  expect_error(summ_sort(list(d_dis), method = 1), "`method`.*string")
  expect_error(summ_sort(list(d_dis), method = "a"), "`method`.*one of")
})


# summ_rank ---------------------------------------------------------------
# More thorough testing is done in `summ_order()`
test_that("summ_rank works", {
  # General case
  mean_vec <- runif(4)
  f_list <- lapply(mean_vec, function(m) {as_d(dnorm, mean = m)})
  ref_rank <- rank(mean_vec)

  for (meth in c("compare", "mean", "median", "mode")) {
    expect_equal(summ_rank(f_list, method = meth), ref_rank)
  }

  # Works with different types
  f_list <- list(as_d(dnorm), new_d(1:10, "discrete"))
  for (meth in c("compare", "mean", "median", "mode")) {
    expect_equal(summ_rank(f_list, method = meth), 1:2)
  }
})

test_that("summ_rank preserves names", {
  f_list <- list(a = d_dis, d_dis + 1, c = d_dis + 2)
  expect_named(summ_rank(f_list), c("a", "", "c"))
})

test_that("summ_rank validates input", {
  expect_error(summ_rank("a"), "`f_list`.*list")
  expect_error(summ_rank(list()), "`f_list`.*empty")
  expect_error(summ_rank(list(function(x) {x})), "`f_list`.*pdqr")
  expect_error(summ_rank(list(d_dis), method = 1), "`method`.*string")
  expect_error(summ_rank(list(d_dis), method = "a"), "`method`.*one of")
})


# order_compare -----------------------------------------------------------
# Tested in `summ_order()`


# order_center ------------------------------------------------------------
# Tested in `summ_order()`


# `[.pdqr_list` -----------------------------------------------------------
# Tested in `summ_order()`


# `>.pdqr_list` -----------------------------------------------------------
# Tested in `summ_order()`


# `==.pdqr_list` ----------------------------------------------------------
# Tested in `summ_order()`
