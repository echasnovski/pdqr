context("test-as_d")

set.seed(2222)


# as_d --------------------------------------------------------------------
test_that('as_d works with "p"', {
  expect_equal_distr(as_d(p_raw), d_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_d(p_smooth), d_smooth, x_smooth_vec_ext)
})

test_that('as_d works with "q"', {
  expect_equal_distr(as_d(q_raw), d_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_d(q_smooth), d_smooth, grid = x_smooth_vec_ext)
})

test_that('as_d works with "r"', {
  expect_equal_distr(as_d(r_raw), d_raw, grid = c(x_raw_vec_ext, x_raw_vec))
  expect_equal_distr(as_d(r_smooth), d_smooth, grid = x_smooth_vec_ext)
})
