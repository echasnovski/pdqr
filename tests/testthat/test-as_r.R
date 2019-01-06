context("test-as_r")

set.seed(4444)


# as_r --------------------------------------------------------------------
test_that('as_r works with "p"', {
  expect_equal_r_funs(as_r(p_raw), r_raw)
  expect_equal_r_funs(as_r(p_smooth), r_smooth)
})

test_that('as_r works with "d"', {
  expect_equal_r_funs(as_r(d_raw), r_raw)
  expect_equal_r_funs(as_r(d_smooth), r_smooth)
})

test_that('as_r works with "q"', {
  expect_equal_r_funs(as_r(q_raw), r_raw)
  expect_equal_r_funs(as_r(q_smooth), r_smooth)
})

test_that('as_r works with "r"', {
  expect_equal_r_funs(as_r(r_raw), r_raw)
  expect_equal_r_funs(as_r(r_smooth), r_smooth)
})

test_that("as_r.pdqr throws errors on bad input", {
  expect_error(as_r(structure(user_r, class = c("p", "pdqr"))), "`f`")
})
