expect_distr_fun <- function(input, distr_type, type) {
  expect_true(is.function(input))
  expect_is(input, distr_type)
  expect_false(is.null(meta(input)))
  expect_equal(meta(input, "type"), type)
}

expect_equal_distr <- function(f_1, f_2, grid, domain,
                               thres = sqrt(.Machine$double.eps)) {
  expect_true(all(abs(f_1(grid) - f_2(grid)) <= thres))
  expect_equal(class(f_1), class(f_2))
  expect_equal(meta(f_1, "type"), meta(f_2, "type"))
  expect_equal(meta(f_1, "extra"), meta(f_2, "extra"))
  if (!is.null(domain)) {
    expect_equal(meta(f_1, domain), meta(f_2, domain))
  }
}
