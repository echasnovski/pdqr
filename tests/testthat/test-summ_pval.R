context("test-summ_pval")

set.seed(6666)


# Custom expectations -----------------------------------------------------
expect_equal_r <- function(x, y, digits = 3) {
  expect_equal(round(x, digits), round(y, digits))
}

expect_pval <- function(f, obs, out, digits = 3) {
  # `out` should be for directions: default, "right", "left", "both"
  expect_equal_r(summ_pval(f, obs), out[1], digits)
  expect_equal_r(summ_pval(f, obs, direction = "right"), out[2], digits)
  expect_equal_r(summ_pval(f, obs, direction = "left"), out[3], digits)
  expect_equal_r(summ_pval(f, obs, direction = "both"), out[4], digits)
}

expect_adjust <- function(p_f, obs, adjust, digits = 3) {
  output <- summ_pval(p_f, obs, adjust = adjust)

  expect_equal_r(output, stats::p.adjust(output, method = adjust), digits)
}


# summ_pval ---------------------------------------------------------------
test_that("summ_pval works", {
  expect_pval(p_fin, 0.9,   c(1,    1,    0,   0))
  expect_pval(p_fin, 1,     c(1,    1,    0.1, 0.2))
  expect_pval(p_fin, 5,     c(0.55, 0.55, 0.5, 1))
  expect_pval(p_fin, 9.001, c(0,    0,    1,   0))

  expect_pval(p_infin,          -100, c(1,     1,     0,     0))
  expect_pval(p_infin,  min(x_infin), c(0.957, 0.957, 0.043, 0.087))
  expect_pval(p_infin,             0, c(0.574, 0.574, 0.426, 0.852))
  expect_pval(p_infin,  max(x_infin), c(0.061, 0.061, 0.939, 0.122))

  expect_pval(p_custom, 0,    c(1,    1,    0,    0))
  expect_pval(p_custom, 0.01, c(0.98, 0.98, 0.02, 0.04))
  expect_pval(p_custom, 0.51, c(0.24, 0.24, 0.76, 0.48))
  expect_pval(p_custom, 1,    c(0,    0,    1,    0))
})

test_that("summ_pval works with vector observations", {
  expect_equal(
    summ_pval(p_fin, c(0.9, 1, 5, 9.001), adjust = "none"),
    c(1, 1, 0.55, 0)
  )
})

test_that("summ_pval adjusts multiple p-values", {
  obs_vec <- seq(0, 0.1, by = 0.01)

  expect_adjust(p_infin, obs_vec, "holm")
  expect_adjust(p_infin, obs_vec, "hochberg")
  expect_adjust(p_infin, obs_vec, "hommel")
  expect_adjust(p_infin, obs_vec, "bonferroni")
  expect_adjust(p_infin, obs_vec, "BH")
  expect_adjust(p_infin, obs_vec, "BY")
  expect_adjust(p_infin, obs_vec, "fdr")
  expect_adjust(p_infin, obs_vec, "none")
})

test_that("summ_pval excepts not only objects of class 'p'", {
  expect_pval(q_fin, 5, c(0.55, 0.55, 0.5, 1))

  expect_pval(d_infin, 0, c(0.574, 0.574, 0.426, 0.852))
  expect_pval(r_infin, 0, c(0.574, 0.574, 0.426, 0.852), digits = 2)

  # First two values differ because there is some randomness involved during
  # conversion from r-function to p-function inside `summ_pval()`.
  expect_pval(r_custom, 0.51, c(0.24, 0.24, 0.76, 0.48), digits = 2)
})

test_that("summ_pval throws errors", {
  expect_error(summ_pval(user_p, 1), "f.*pdqr")
  expect_error(
    summ_pval(structure(user_d, class = c("d", "pdqr")), 1),
    "f.*proper.*type"
  )

  expect_error(summ_pval(p_fin, "a"), "obs.*numeric")
  expect_error(summ_pval(p_fin, 1, direction = 1), "direction.*string")
  expect_error(
    summ_pval(p_fin, 1, direction = "a"),
    '`direction`.*one of.*"left".*"right".*"both"'
  )
  expect_error(summ_pval(p_fin, 1, adjust = 1), "adjust.*string")

  expect_error(
    summ_pval(p_fin, 1, adjust = "b"),
    paste0(
      c("`adjust`", stats::p.adjust.methods), collapse = ".*"
    )
  )
})


# left_pval ---------------------------------------------------------------
# Tested in `summ_pval()`


# right_pval --------------------------------------------------------------
# Tested in `summ_pval()`


# both_pval ---------------------------------------------------------------
# Tested in `summ_pval()`
