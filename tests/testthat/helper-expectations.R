expect_distr_fun <- function(input, distr_type, type) {
  expect_true(is.function(input))
  expect_is(input, distr_type)
  expect_equal(meta(input, "type"), type)
  expect_true(is_support(meta(input, "support")))
}

is_support <- function(supp) {
  is.numeric(supp) && (length(supp) == 2) &&
    (supp[1] <= supp[2])
}

expect_equal_distr <- function(f_1, f_2, grid,
                               thres = sqrt(.Machine$double.eps),
                               check_supp = TRUE) {
  expect_true(all(abs(f_1(grid) - f_2(grid)) <= thres))
  expect_equal(class(f_1), class(f_2))

  expect_equal(length(meta(f_1)), length(meta(f_2)))
  expect_equal(meta(f_1, "type"), meta(f_2, "type"))
  expect_equal(meta(f_1, "extra"), meta(f_2, "extra"))
  if (check_supp) {
    expect_equal(meta(f_1, "support"), meta(f_2, "support"))
  }
}
