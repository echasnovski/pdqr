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

# Normal distribution multiplied by 4
r_norm_input <- structure(
  function(n) {rnorm(n, mean = 0, sd = 1)},
  class = c("r_fun", "pdqr_fun", "function"),
  meta = list(type = "smooth", support = c(-10, 10))
)
d_norm_ref <- structure(
  function(x) {dnorm(x, mean = 0, sd = 2)},
  class = c("d_fun", "pdqr_fun", "function"),
  meta = list(type = "smooth", support = c(-20, 20))
)


# pdqr_transform ----------------------------------------------------------
test_that("pdqr_transform works", {
  output_custom <- pdqr_transform(sq, p_custom)
  expect_distr_fun(output_custom, "p_fun", "smooth")
  expect_equal_distr(
    output_custom, p_custom_ref,
    grid = x_custom_trunc, thres = 0.05, check_supp = FALSE
  )

  output_norm <- pdqr_transform(`*`, r_norm_input, 2, .pdqr_type = "d_fun")
  expect_distr_fun(output_norm, "d_fun", "smooth")
  expect_equal_distr(
    output_norm, d_norm_ref,
    grid = seq(-10, 10, by = 0.01), thres = 0.01, check_supp = FALSE
  )
})

test_that("pdqr_transform throws errors", {
  expect_error(
    pdqr_transform(sq, p_custom, .pdqr_args = "a"), "\\.pdqr_args.*list"
  )
  expect_error(pdqr_transform(`+`, r_raw, user_r), "`...`.*should.*pdqr.*fun")
})


# assert_trans_dots -------------------------------------------------------
# Tested in `pdqr_transform()`


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
    'pdqr_type.*one of.*"p_fun", "d_fun", "q_fun", "r_fun"'
  )
})


# get_pdqr_type -----------------------------------------------------------
test_that("get_pdqr_type works", {
  expect_equal(get_pdqr_type(structure("a", class = "p_fun")), "p_fun")
  expect_equal(
    get_pdqr_type(structure("a", class = c("p_fun", "d_fun"))), "p_fun"
  )
  expect_equal(get_pdqr_type(structure("a", class = "bbb")), NA_character_)
})
