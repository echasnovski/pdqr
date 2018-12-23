expect_distr_fun <- function(input, distr_type, type) {
  expect_true(is.function(input))
  expect_is(input, distr_type)
  expect_is(input, "pdqr")
  expect_equal(meta(input, "type"), type)
  expect_true(is_support(meta(input, "support")))
}

expect_equal_distr <- function(f_1, f_2, grid, thres = 10^(-8),
                               meta_not_check = character(0)) {
  expect_true(all(abs(f_1(grid) - f_2(grid)) <= thres))
  expect_equal(class(f_1), class(f_2))

  meta_names_1 <- setdiff(names(meta(f_1)), meta_not_check)
  meta_names_2 <- setdiff(names(meta(f_2)), meta_not_check)
  expect_equal(
    lapply(meta_names_1, meta, obj = f_1),
    lapply(meta_names_2, meta, obj = f_2)
  )
}

expect_different_distr <- function(f_1, f_2, grid, thres = 10^(-8)) {
  expect_true(max(abs(f_1(grid) - f_2(grid)), na.rm = TRUE) >= thres)
}

expect_pdqr_print <- function(f, f_name) {
  supp_regex <- "Support: \\[[-0-9\\.]+, [-0-9\\.]+\\]"

  f_raw <- f(x_raw, type = "raw")
  expect_output(
    print(f_raw),
    regex_scatter(f_name, "raw type", supp_regex)
  )

  f_smooth <- f(x_smooth, type = "smooth")
  expect_output(
    print(f_smooth),
    regex_scatter(f_name, "smooth type", supp_regex)
  )
}

regex_scatter <- function(...) {
  paste0(c(...), collapse = ".*")
}
