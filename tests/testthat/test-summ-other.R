context("test-summ-other")


# summ_quantile -----------------------------------------------------------
test_that("summ_quantile works", {
  probs <- seq(0, 1, by = 0.25)

  # Type "discrete"
  expect_equal(summ_quantile(d_dis, probs), q_dis(probs))

  # Type "continuous"
  expect_equal(summ_quantile(d_con, probs), q_con(probs))
})

test_that("summ_quantile validates input", {
  expect_error(summ_quantile("a", c(0, 1)), "`f`.*not pdqr-function")
  expect_error(summ_quantile(d_dis), "`probs`.*quantile probabilities")
  expect_error(summ_quantile(d_dis, "a"), "`probs`.*numeric")
  expect_error(summ_quantile(d_dis, c(-1e-6, 0)), "`probs`.*\\[0; 1\\]")
  expect_error(summ_quantile(d_dis, c(1, 1+1e-6)), "`probs`.*\\[0; 1\\]")
})


# summ_prob_true ----------------------------------------------------------
test_that("summ_prob_true works", {
  cur_bool_1 <- new_d(data.frame(x = c(0, 1), prob = c(0.3, 0.7)), "discrete")
  expect_equal(summ_prob_true(cur_bool_1), 0.7)

  cur_bool_2 <- new_d(data.frame(x = c(0, 1), prob = c(1, 0)), "discrete")
  expect_equal(summ_prob_true(cur_bool_2), 0)
})

test_that("summ_prob_true warns about non-boolean pdqr-function", {
  expect_warning(summ_prob_true(new_d(-1, "discrete")), "not.*boolean")
  # "Boolean" pdqr-function should have both 0 and 1 in `x`
  expect_warning(summ_prob_true(new_d(0, "discrete")), "not.*boolean")
  expect_warning(summ_prob_true(new_d(1, "discrete")), "not.*boolean")
})

test_that("summ_prob_true handles absence of 0 in 'x' column of 'x_tbl'", {
  cur_dis <- new_d(data.frame(x = c(-1, 1), prob = c(1, 1)/2), "discrete")
  expect_equal(expect_warning(summ_prob_true(cur_dis)), 1)

  cur_con <- new_d(data.frame(x = c(-1, 1), y = c(1, 1)), "continuous")
  expect_equal(expect_warning(summ_prob_true(cur_con)), 1)
})

test_that("summ_prob_true validates input", {
  expect_error(summ_prob_true("a"), "`f`.*not pdqr-function")
})


# summ_prob_false ---------------------------------------------------------
test_that("summ_prob_false works", {
  cur_bool_1 <- new_d(data.frame(x = c(0, 1), prob = c(0.3, 0.7)), "discrete")
  expect_equal(summ_prob_false(cur_bool_1), 0.3)

  cur_bool_2 <- new_d(data.frame(x = c(0, 1), prob = c(1, 0)), "discrete")
  expect_equal(summ_prob_false(cur_bool_2), 1)
})

test_that("summ_prob_false warns about non-boolean pdqr-function", {
  expect_warning(summ_prob_false(new_d(-1, "discrete")), "not.*boolean")
  # "Boolean" pdqr-function should have both 0 and 1 in `x`
  expect_warning(summ_prob_false(new_d(0, "discrete")), "not.*boolean")
  expect_warning(summ_prob_false(new_d(1, "discrete")), "not.*boolean")
})

test_that("summ_prob_false handles absence of 0 in 'x' column of 'x_tbl'", {
  cur_dis <- new_d(data.frame(x = c(-1, 1), prob = c(1, 1)/2), "discrete")
  expect_equal(expect_warning(summ_prob_false(cur_dis)), 0)

  cur_con <- new_d(data.frame(x = c(-1, 1), y = c(1, 1)), "continuous")
  expect_equal(expect_warning(summ_prob_false(cur_con)), 0)
})

test_that("summ_prob_false validates input", {
  expect_error(summ_prob_false("a"), "`f`.*not pdqr-function")
})
