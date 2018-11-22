expect_distr_fun <- function(input, distr_type, type) {
  expect_true(is.function(input))
  expect_is(input, distr_type)
  expect_is(input, "pdqr_fun")
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

expect_different_distr <- function(f_1, f_2, grid,
                                   thres = sqrt(.Machine$double.eps)) {
  expect_true(max(abs(f_1(grid) - f_2(grid)), na.rm = TRUE) >= thres)
}

expect_pdqr_print <- function(f, f_name) {
  supp_regex <- "Support: \\[[-0-9\\.]+, [-0-9\\.]+\\]"

  f_raw <- f(x_raw, type = "raw", attach_x = TRUE)
  raw_sample_regex <- paste0(
    'Sample \\("x"\\) is attached \\(', length(x_raw), " elements\\)"
  )
  expect_output(
    print(f_raw),
    regex_scatter(
      f_name, "raw type", supp_regex, raw_sample_regex,
      "Extra", "is not attached"
    )
  )

  f_smooth <- f(x_smooth, type = "smooth", attach_x = FALSE, extra = list(1, 2))
  smooth_sample_regex <- 'Sample \\("x"\\) is not attached'
  expect_output(
    print(f_smooth),
    regex_scatter(
      f_name, "smooth type", supp_regex, smooth_sample_regex,
      "Extra", "is attached \\(2 elements\\)"
    )
  )

  f_single <- f(1, type = "raw")
  single_sample_regex <- 'Sample \\("x"\\) is attached \\(1 element\\)'
  expect_output(print(f_single), single_sample_regex)
}

regex_scatter <- function(...) {
  paste0(c(...), collapse = ".*")
}
