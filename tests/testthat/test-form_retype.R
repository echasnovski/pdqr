context("test-form_retype")

set.seed(12321)


# form_retype -------------------------------------------------------------
test_that("form_retype converts to opposite type by default", {
  expect_equal(meta_type(form_retype(d_dis)), "continuous")
  expect_equal(meta_type(form_retype(d_con)), "discrete")
})

test_that("form_retype works with `method='value'`", {
  # "discrete" -> "continuous"
  cur_dis <- new_d(
    data.frame(x = c(0, 1, 5), prob = c(0.8, 0, 0.2)), "discrete"
  )
  expect_ref_x_tbl(
    form_retype(cur_dis, "continuous", method = "value"),
    data.frame(x = c(0, 1, 5), y = c(1, 0, 0.25))
  )

  # "continous" -> "discrete"
  cur_con <- new_d(data.frame(x = 1:3, y = 1:3 / 4), "continuous")
  expect_ref_x_tbl(
    form_retype(cur_con, "discrete", method = "value"),
    data.frame(x = 1:3, prob = 1:3 / 6)
  )

  # Double retyping should return self
  expect_equal_x_tbl(
    form_retype(form_retype(d_dis, "continuous", "value"), "discrete", "value"),
    d_dis
  )
  expect_equal_x_tbl(
    form_retype(form_retype(d_con, "discrete", "value"), "continuous", "value"),
    d_con
  )
})

test_that("form_retype converts to 'discrete' with `method='piecelin'`", {
  output <- form_retype(p_con, "discrete", method = "piecelin")
  expect_distr_fun(output, "p", "discrete")

  # "continuous" function should have one row more in "x_tbl" than "discrete"
  # function
  expect_equal(nrow(meta_x_tbl(p_con)) - 1, nrow(meta_x_tbl(output)))
})

test_that("form_retype converts to 'continuous' with `method='piecelin'`", {
  output <- form_retype(d_dis, "continuous", method = "piecelin")
  expect_distr_fun(output, "d", "continuous")

  # "continuous" function should have one row more in "x_tbl" than "discrete"
  # function
  expect_equal(nrow(meta_x_tbl(d_dis)), nrow(meta_x_tbl(output)) - 1)
})

test_that("form_retype handles adjacent 0 densities with `method='piecelin'`", {
  input_con <- new_d(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "continuous")
  output_dis <- form_retype(input_con, "discrete", method = "piecelin")

  # "Centre of mass" in case both ends have zero density should be in the middle
  expect_equal(meta_x_tbl(output_dis)[["x"]][2], 2.5)
})

test_that("form_retype works with `method='dirac'`", {
  d_dis <- new_d(data.frame(x = 1:3, prob = c(0.1, 0.2, 0.7)), "discrete")
  d_con_dirac <- form_retype(d_dis, "continuous", method = "dirac")

  expect_ref_x_tbl(
    d_con_dirac,
    data.frame(
      x = c(1 - 1e-8, 1,   1 + 1e-8, 2 - 1e-8, 2,   2 + 1e-8, 3 - 1e-8, 3,
        3 + 1e-8),
      y = c(0,        1e7, 0,        0,        2e7, 0,        0,        7e7,
        0)
    )
  )

  d_dis_dirac <- form_retype(d_con_dirac, "discrete", method = "dirac")
  expect_equal_x_tbl(d_dis_dirac, d_dis)
})

test_that("form_retype retypes well 'continuous'->'discrete'->'continuous'", {
  skip_on_cran()

  error <- vapply(
    fam_list[c("fam_norm", "fam_exp", "fam_exp_rev", "fam_beta", "fam_chisq")],
    function(fam) {
      d_f <- as_d(fam$d, fam$support)
      d_f_new <- form_retype(form_retype(d_f, "discrete"), "continuous")

      # `quan999()` is used instead of `max()` because retyping can slightly
      # shrink "true" support (and signigicantly, if support is set manually).
      # It means that outside of output's support error can be quite high.
      quan999(abs(d_f(fam$grid) - d_f_new(fam$grid)))
    },
    numeric(1))
  expect_true(all(error < 1e-6))
})

test_that("form_retype retypes well 'discrete'->'continuous'->'discrete'", {
  skip_on_cran()

  test_grid <- seq(-4, 4, length.out = 1e5)
  error <- vapply(
    1:10,
    function(i) {
      n <- sample(1:1000, 1)
      x <- sort(rnorm(n))
      prob <- runif(n)
      prob <- prob / sum(prob)

      p_f <- new_p(data.frame(x = x, prob = prob), "discrete")
      p_f_new <- form_retype(form_retype(p_f, "continuous"), "discrete")

      max(abs(p_f(test_grid) - p_f_new(test_grid)))
    },
    numeric(1))

  # Maximum error is quite big because algorithm inside `form_retype()` isn't
  # perfect. Also there is a possibility of very close `x` values in input with
  # almost the same probability as in other. It induces very high values of
  # "continuous" density which can disturb all CDF.
  expect_true(all(error < 0.2))
})

test_that("form_retype returns input when types match", {
  expect_identical(form_retype(d_dis, "discrete", method = "piecelin"), d_dis)
  expect_identical(form_retype(d_dis, "discrete", method = "dirac"), d_dis)
  expect_identical(form_retype(d_con, "continuous", method = "piecelin"), d_con)
  expect_identical(form_retype(d_con, "continuous", method = "dirac"), d_con)
})

test_that("form_retype validates input", {
  expect_error(form_retype("a", "discrete"), "`f`.*not pdqr-function")
  expect_error(form_retype(d_dis, "a"), '`type`.*"discrete".*"continuous"')
  expect_error(form_retype(d_dis, "continuous", 1), "`method`.*string")
  expect_error(
    form_retype(d_dis, "continuous", "a"), '`method`.*"piecelin".*"dirac"'
  )

  # Number of elements in "x_tbl" metadata for "piecelin" method
  dis_small_n_vals <- new_d(1:3, "discrete")
  expect_error(
    form_retype(dis_small_n_vals, "continuous", method = "piecelin"),
    "4.*values"
  )
})


# retype_dis --------------------------------------------------------------
# Tested in `form_retype()`


# retype_dis_value --------------------------------------------------------
# Tested in `form_retype()`


# retype_dis_piecelin -----------------------------------------------------
# Tested in `form_retype()`


# retype_dis_dirac --------------------------------------------------------
# Tested in `form_retype()`


# retype_con --------------------------------------------------------------
# Tested in `form_retype()`


# retype_con_value --------------------------------------------------------
# Tested in `form_retype()`


# retype_con_piecelin -----------------------------------------------------
# Tested in `form_retype()`


# retype_con_dirac --------------------------------------------------------
# Tested in `form_retype()`
