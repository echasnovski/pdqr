context("test-summ_pval")


# Custom expectations -----------------------------------------------------
expect_pval <- function(f, obs, out) {
  # `out` should be for methods: default, "both",  "right", "left"
  expect_equal(summ_pval(f, obs), out[1])
  expect_equal(summ_pval(f, obs, method = "both"), out[2])
  expect_equal(summ_pval(f, obs, method = "right"), out[3])
  expect_equal(summ_pval(f, obs, method = "left"), out[4])
}

expect_adjust <- function(p_f, obs, adjust) {
  output_none <- summ_pval(p_f, obs, adjust = "none")
  output <- summ_pval(p_f, obs, adjust = adjust)

  expect_equal(output, stats::p.adjust(output_none, method = adjust))
}


# summ_pval ---------------------------------------------------------------
test_that("summ_pval works", {
  # Type "discrete"
  cur_dis <- new_d(data.frame(x = 1:5, prob = c(2, 2, 3, 2, 1)/10), "discrete")
  expect_pval(cur_dis,    -1, c(     0,      0,    1,    0))
  expect_pval(cur_dis, 2.999, c(2*4/10, 2*4/10, 6/10, 4/10))
  expect_pval(cur_dis,     3, c(     1,      1, 6/10, 7/10))
  expect_pval(cur_dis,   4.5, c(2*1/10, 2*1/10, 1/10, 9/10))
  expect_pval(cur_dis,   100, c(     0,      0,    0,    1))

  # Type "continuous"
  cur_con <- new_d(
    data.frame(x = 1:11, y = c(0, rep(c(1, 0), 5))), "continuous"
  )
  expect_pval(cur_con,  -1, c(    0,     0,   1,   0))
  expect_pval(cur_con,   3, c(2*0.2, 2*0.2, 0.8, 0.2))
  expect_pval(cur_con,   6, c(2*0.5, 2*0.5, 0.5, 0.5))
  expect_pval(cur_con,  10, c(2*0.1, 2*0.1, 0.1, 0.9))
  expect_pval(cur_con, 100, c(    0,     0,   0,   1))
})

test_that("summ_pval works with vector observations", {
  # This is also test for `summ_pval()` not adjusting if `adjust='none'`
  cur_con <- new_d(
    data.frame(x = 1:11, y = c(0, rep(c(1, 0), 5))), "continuous"
  )
  expect_equal(
    summ_pval(cur_con, c(-1, 3, 6, 10, 100), adjust = "none"),
    c(0, 0.4, 1, 0.2, 0)
  )
})

test_that("summ_pval adjusts multiple p-values", {
  obs_vec <- seq(0, 0.1, by = 0.01)

  expect_adjust(p_con, obs_vec, "holm")
  expect_adjust(p_con, obs_vec, "hochberg")
  expect_adjust(p_con, obs_vec, "hommel")
  expect_adjust(p_con, obs_vec, "bonferroni")
  expect_adjust(p_con, obs_vec, "BH")
  expect_adjust(p_con, obs_vec, "BY")
  expect_adjust(p_con, obs_vec, "fdr")
})

test_that("summ_pval accepts any pdqr class", {
  obs_vec <- seq(0, 0.1, by = 0.01)
  expect_equal(summ_pval(d_con, obs_vec), summ_pval(p_con, obs_vec))
  expect_equal(summ_pval(q_con, obs_vec), summ_pval(p_con, obs_vec))
  expect_equal(summ_pval(r_con, obs_vec), summ_pval(p_con, obs_vec))
})

test_that("summ_pval validates input", {
  expect_error(summ_pval("a", 1), "`f`.*not pdqr-function")
  expect_error(summ_pval(p_dis), "`obs`.*missing.*vector of observation\\(s\\)")
  expect_error(summ_pval(p_dis, "a"), "`obs`.*numeric")
  expect_error(summ_pval(p_dis, 1, method = 1), "`method`.*string")
  expect_error(summ_pval(p_dis, 1, method = "a"), "`method`.*one of")
  expect_error(summ_pval(p_dis, 1, adjust = 1), "`adjust`.*string")
  expect_error(summ_pval(p_dis, 1, adjust = "b"), "`adjust`.*one of")
})


# both_pval ---------------------------------------------------------------
# Tested in `summ_pval()`


# right_pval --------------------------------------------------------------
# Tested in `summ_pval()`


# left_pval ---------------------------------------------------------------
# Tested in `summ_pval()`
