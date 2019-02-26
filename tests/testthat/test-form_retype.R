context("test-form_retype")

set.seed(12321)


# form_retype -------------------------------------------------------------
test_that("form_retype converts to `type` 'fin' with `method` 'piecelin'", {
  output <- form_retype(p_infin, "fin", method = "piecelin")
  expect_distr_fun(output, "p", "fin")

  # "infin" function should have one row more in "x_tbl" than "fin" function
  expect_equal(nrow(meta_x_tbl(p_infin)) - 1, nrow(meta_x_tbl(output)))
})

test_that("form_retype converts to `type` 'infin' with `method` 'piecelin'", {
  output <- form_retype(d_fin, "infin", method = "piecelin")
  expect_distr_fun(output, "d", "infin")

  # "infin" function should have one row more in "x_tbl" than "fin" function
  expect_equal(nrow(meta_x_tbl(d_fin)), nrow(meta_x_tbl(output)) - 1)
})

test_that("form_retype handles adjacent 0 densities with `method`='piecelin'", {
  input_infin <- new_d(data.frame(x = 1:4, y = c(1, 0, 0, 1)), "infin")
  output_fin <- form_retype(input_infin, "fin", method = "piecelin")

  # "Centre of mass" in case both ends have zero density should be in the middle
  expect_equal(meta_x_tbl(output_fin)[["x"]][2], 2.5)
})

test_that("form_retype works with `method` 'dirac'", {
  d_fin <- new_d(data.frame(x = 1:3, prob = c(0.1, 0.2, 0.7)), "fin")
  d_infin_dirac <- form_retype(d_fin, "infin", method = "dirac")

  expect_ref_x_tbl(
    d_infin_dirac,
    data.frame(
      x = c(1-1e-8,   1, 1+1e-8, 2-1e-8,   2, 2+1e-8, 3-1e-8,   3, 3+1e-8),
      y = c(     0, 1e7,      0,      0, 2e7,      0,      0, 7e7,      0)
    )
  )

  d_fin_dirac <- form_retype(d_infin_dirac, "fin", method = "dirac")
  expect_equal_x_tbl(d_fin_dirac, d_fin)
})

test_that("form_retype by default accurately retypes 'infin'->'fin'->'infin'", {
  skip_on_cran()

  error <- vapply(
    fam_list[c("fam_norm", "fam_exp", "fam_exp_rev", "fam_beta", "fam_chisq")],
    function(fam) {
      d_f <- as_d(fam$d, fam$support)
      d_f_new <- form_retype(form_retype(d_f, "fin"), "infin")

      # `quan999()` is used instead of `max()` because retyping can slightly
      # shrink "true" support (and signigicantly, if support is set manually).
      # It means that outside of output's support error can be quite high.
      quan999(abs(d_f(fam$grid) - d_f_new(fam$grid)))
    },
    numeric(1))
  expect_true(all(error < 1e-6))
})

test_that("form_retype by default accurately retypes 'fin'->'infin'->'fin'", {
  skip_on_cran()

  test_grid <- seq(-4, 4, length.out = 1e5)
  error <- vapply(
    1:10,
    function(i) {
      n <- sample(1:1000, 1)
      x <- sort(rnorm(n))
      prob <- runif(n)
      prob <- prob / sum(prob)

      p_f <- new_p(data.frame(x = x, prob = prob), "fin")
      p_f_new <- form_retype(form_retype(p_f, "infin"), "fin")

      max(abs(p_f(test_grid) - p_f_new(test_grid)))
    },
    numeric(1))

  # Maximum error is quite big because algorithm inside `form_retype()` isn't
  # perfect. Also there is a possibility of very close `x` values in input with
  # almost the same probability as in other. It induces very high values of
  # "infin" density which can disturb all CDF.
  expect_true(all(error < 0.2))
})

test_that("form_retype returns input when types match", {
  expect_identical(form_retype(d_fin, "fin", method = "piecelin"), d_fin)
  expect_identical(form_retype(d_fin, "fin", method = "dirac"), d_fin)
  expect_identical(form_retype(d_infin, "infin", method = "piecelin"), d_infin)
  expect_identical(form_retype(d_infin, "infin", method = "dirac"), d_infin)
})

test_that("form_retype asserts bad input", {
  expect_error(form_retype("a", "fin"), "`f`.*function")
  expect_error(form_retype(d_fin, "a"), '`type`.*"fin".*"infin"')
  expect_error(form_retype(d_fin, "infin", 1), '`method`.*string')
  expect_error(
    form_retype(d_fin, "infin", "a"), '`method`.*"piecelin".*"dirac"'
  )

  fin_small_n_vals <- new_d(1:3, "fin")
  expect_error(form_retype(fin_small_n_vals, "infin"), "4.*values")
})


# retype_fin --------------------------------------------------------------
# Tested in `form_retype()`


# retype_fin_piecelin -----------------------------------------------------
# Tested in `form_retype()`


# retype_fin_dirac --------------------------------------------------------
# Tested in `form_retype()`


# retype_infin ------------------------------------------------------------
# Tested in `form_retype()`


# retype_infin_piecelin ---------------------------------------------------
# Tested in `form_retype()`


# retype_infin_dirac ------------------------------------------------------
# Tested in `form_retype()`
