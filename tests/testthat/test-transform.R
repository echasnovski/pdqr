context("test-transform")


# Reference data ----------------------------------------------------------
sq <- function(x) {x * x}

# Analytical form of random variable `X^2`
p_custom_ref <- structure(
  function(q) {
    user_p(sqrt(q)) - user_p(-sqrt(q))
  },
  class = c("p_fun", "pdqr_fun", "function"),
  meta = list(type = "smooth", support = c(0, 1))
)

x_norm_seq <- seq(-10, 10, by = 0.01)


# pdqr_transform ----------------------------------------------------------
test_that("pdqr_transform works", {
  output_custom <- pdqr_transform(sq, p_custom)
  expect_distr_fun(output_custom, "p_fun", "smooth")
  expect_equal_distr(
    output_custom, p_custom_ref,
    grid = x_custom_trunc, thres = 0.05, check_supp = FALSE
  )

  # Normal distribution multiplied by 2
  r_norm_input <- as_r(
    function(n) {rnorm(n, mean = 0, sd = 1)},
    type = "smooth", support = c(-10, 10)
  )
  d_norm_ref <- as_d(
    function(x) {dnorm(x, mean = 0, sd = 2)},
    type = "smooth", support = c(-20, 20)
  )

  output_norm <- pdqr_transform(`*`, r_norm_input, 2, .pdqr_class = "d_fun")
  expect_distr_fun(output_norm, "d_fun", "smooth")
  expect_equal_distr(
    output_norm, d_norm_ref,
    grid = x_norm_seq, thres = 0.01, check_supp = FALSE
  )
})

test_that("pdqr_transform doesn't attach `x` by default", {
  output <- pdqr_transform(sq, q_raw_withx)
  expect_false(has_meta(output, "x"))
})

test_that("pdqr_transform correctly restores 'pdqr' type", {
  expect_is(pdqr_transform(`+`, 1, p_raw), "p_fun")
  expect_is(pdqr_transform(`+`, d_raw, p_raw), "d_fun")
  expect_is(pdqr_transform(`+`, d_raw, p_raw, .pdqr_class = "r_fun"), "r_fun")
})

test_that("pdqr_transform throws errors", {
  expect_error(pdqr_transform(1, p_custom), "trans.*function")
  expect_error(pdqr_transform(`+`, r_raw, user_r), "`...`.*should.*pdqr.*fun")
  expect_error(pdqr_transform(`+`, r_raw, 1:2), "`...`.*should.*single numbers")
  expect_error(pdqr_transform(`+`, 1, 2), "`...`.*should.*at least one.*pdqr")
  expect_error(
    pdqr_transform(sq, p_custom, .n_sample = "a"), "\\.n_sample.*single number"
  )
  expect_error(
    pdqr_transform(sq, p_custom, .n_sample = 1:2), "\\.n_sample.*single number"
  )
  expect_error(
    pdqr_transform(sq, p_custom, .pdqr_class = 1), "\\.pdqr_class.*string"
  )
  expect_error(
    pdqr_transform(sq, p_custom, .pdqr_class = c("a", "b")),
    "\\.pdqr_class.*string"
  )
  expect_error(
    pdqr_transform(sq, p_custom, .pdqr_args = "a"), "\\.pdqr_args.*list"
  )
})


# assert_trans_dots -------------------------------------------------------
# Tested in `pdqr_transform()`


# find_ref_f --------------------------------------------------------------
test_that("find_ref_f works", {
  expect_equal(find_ref_f(list(1, p_raw, 2)), p_raw)
  expect_null(find_ref_f(list(1, 2)))
})


# impute_pdqr_fun ---------------------------------------------------------
test_that("impute_pdqr_fun works", {
  expect_equal(impute_pdqr_fun("p_fun", NULL), p_fun)
  expect_equal(impute_pdqr_fun("d_fun", NULL), d_fun)
  expect_equal(impute_pdqr_fun("q_fun", NULL), q_fun)
  expect_equal(impute_pdqr_fun("r_fun", NULL), r_fun)

  expect_equal(impute_pdqr_fun(NULL, p_raw), p_fun)
})

test_that("impute_pdqr_fun throws errors", {
  expect_error(
    impute_pdqr_fun("a", r_raw),
    'pdqr_class.*one of.*"p_fun", "d_fun", "q_fun", "r_fun"'
  )
})


# get_pdqr_class ----------------------------------------------------------
test_that("get_pdqr_class works", {
  expect_equal(get_pdqr_class(structure("a", class = "p_fun")), "p_fun")
  expect_equal(
    get_pdqr_class(structure("a", class = c("p_fun", "d_fun"))), "p_fun"
  )
  expect_equal(get_pdqr_class(structure("a", class = "bbb")), NA_character_)
})


# Math.pdqr_fun -----------------------------------------------------------
test_that("Math.pdqr_fun works", {
  d_lnorm <- as_d(dlnorm, type = "smooth", support = c(0, 100))
  d_norm_ref <- as_d(dnorm, type = "smooth", support = c(-10, 10))
  d_norm_out <- log(d_lnorm)

  expect_distr_fun(d_norm_out, "d_fun", "smooth")
  expect_equal_distr(
    d_norm_out, d_norm_ref,
    grid = x_norm_seq, thres = 0.05, check_supp = FALSE
  )
})


# Ops.pdqr_fun ------------------------------------------------------------
test_that("Ops.pdqr_fun works", {
  p_unif <- as_p(punif, "smooth", c(0, 1))
  p_norm_ref <- as_p(pnorm, "smooth", c(-10, 10))
  # Approximation of standard normal as centered sum of 12 uniform
  p_norm_out <- p_unif + p_unif + p_unif + p_unif + p_unif + p_unif +
    p_unif + p_unif + p_unif + p_unif + p_unif + p_unif - 6

  expect_distr_fun(p_norm_out, "p_fun", "smooth")
  expect_equal_distr(
    p_norm_out, p_norm_ref,
    grid = x_norm_seq, thres = 0.05, check_supp = FALSE
  )
})

test_that("Ops.pdqr_fun warns about not numeric type", {
  expect_warning(p_raw >= p_smooth, ">=.*logical.*[Cc]onvert")
})

test_that("Ops.pdqr_fun works with for generics with one argument", {
  expect_warning(!p_raw, "!.*logical.*[Cc]onvert")
})
