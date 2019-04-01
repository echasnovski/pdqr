context("test-summ-other")


# summ_quantile -----------------------------------------------------------
test_that("summ_quantile works", {
  probs <- seq(0, 1, by = 0.25)

  # Type "fin"
  expect_equal(summ_quantile(d_fin, probs), q_fin(probs))

  # Type "infin"
  expect_equal(summ_quantile(d_infin, probs), q_infin(probs))
})

test_that("summ_quantile validates input", {
  expect_error(summ_quantile("a", c(0, 1)), "`f`.*function")
  expect_error(summ_quantile(function(x) {x}, c(0, 1)), "`f`.*pdqr")
  expect_error(summ_quantile(d_fin, "a"), "`probs`.*numeric")
  expect_error(summ_quantile(d_fin, c(-1e-6, 0)), "`probs`.*\\[0; 1\\]")
  expect_error(summ_quantile(d_fin, c(1, 1+1e-6)), "`probs`.*\\[0; 1\\]")
})
