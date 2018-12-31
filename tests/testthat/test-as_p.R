context("test-as_p")

set.seed(1111)


# as_p --------------------------------------------------------------------
test_that('as_p works with "d"', {
  expect_equal_distr(as_p(d_raw), p_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_p(d_smooth), p_smooth, grid = x_smooth_vec_ext)
})

test_that('as_p works with "q"', {
  expect_equal_distr(as_p(q_raw), p_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_p(q_smooth), p_smooth, grid = x_smooth_vec_ext)
})

test_that('as_p works with "r"', {
  expect_equal_distr(as_p(r_raw), p_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_p(r_smooth), p_smooth, grid = x_smooth_vec_ext)
})
