context("test-as_q")

set.seed(3333)


# as_q --------------------------------------------------------------------
test_that('as_q works with "p"', {
  expect_equal_distr(as_q(p_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(p_smooth), q_smooth, grid = p_vec)
})

test_that('as_q works with "d"', {
  expect_equal_distr(as_q(d_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(d_smooth), q_smooth, grid = p_vec)
})

test_that('as_q works with "r"', {
  expect_equal_distr(as_q(r_raw), q_raw, grid = p_vec)
  expect_equal_distr(as_q(r_smooth), q_smooth, grid = p_vec)
})
