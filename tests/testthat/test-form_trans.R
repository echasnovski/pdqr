context("test-form_trans")

set.seed(7777)


# Reference data ----------------------------------------------------------
sq <- function(x) {x * x}

# Analytical form of random variable `X^2`
p_custom_ref <- structure(
  function(q) {
    user_p(sqrt(q)) - user_p(-sqrt(q))
  },
  class = c("p", "pdqr", "function"),
  meta = list(type = "infin", support = c(0, 1))
)

x_norm_seq <- seq(-10, 10, by = 0.01)


# form_trans --------------------------------------------------------------
test_that("form_trans works", {
  output_custom <- form_trans(sq, p_custom)
  expect_distr_fun(output_custom, "p", "infin")
  expect_equal_distr(
    output_custom, p_custom_ref,
    grid = x_custom_trunc, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )

  # Normal distribution multiplied by 2
  r_norm_input <- new_r(rnorm(10000, mean = 0, sd = 1))

  norm_seq <- seq(-20, 20, by = 0.001)
  d_norm_ref <- new_d(
    data.frame(x = norm_seq, y = dnorm(norm_seq, mean = 0, sd = 2))
  )

  output_norm <- form_trans(`*`, r_norm_input, 2, .pdqr_class = "d")
  expect_distr_fun(output_norm, "d", "infin")
  expect_equal_distr(
    output_norm, d_norm_ref,
    grid = x_norm_seq, thres = 0.02,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that("form_trans doesn't attach `x` by default", {
  output <- form_trans(sq, q_fin)
  expect_false(has_meta(output, "x"))
})

test_that("form_trans correctly restores 'pdqr' type", {
  expect_is(form_trans(`+`, 1, p_fin), "p")
  expect_is(form_trans(`+`, d_fin, p_fin), "d")
  expect_is(form_trans(`+`, d_fin, p_fin, .pdqr_class = "r"), "r")
})

test_that("form_trans throws errors", {
  expect_error(form_trans(1, p_custom), "trans.*function")
  expect_error(form_trans(`+`, r_fin, user_r), "`...`.*should.*pdqr.*fun")
  expect_error(form_trans(`+`, r_fin, 1:2), "`...`.*should.*single numbers")
  expect_error(form_trans(`+`, 1, 2), "`...`.*should.*at least one.*pdqr")
  expect_error(
    form_trans(sq, p_custom, .n_sample = "a"), "\\.n_sample.*single number"
  )
  expect_error(
    form_trans(sq, p_custom, .n_sample = 1:2), "\\.n_sample.*single number"
  )
  expect_error(
    form_trans(sq, p_custom, .pdqr_class = 1), "\\.pdqr_class.*string"
  )
  expect_error(
    form_trans(sq, p_custom, .pdqr_class = c("a", "b")),
    "\\.pdqr_class.*string"
  )
  expect_error(form_trans(sq, p_custom, .pdqr_args = "a"), "\\.pdqr_args.*list")
})


# assert_trans_dots -------------------------------------------------------
# Tested in `form_trans()`


# find_ref_f --------------------------------------------------------------
test_that("find_ref_f works", {
  expect_equal(find_ref_f(list(1, p_fin, 2)), p_fin)
  expect_null(find_ref_f(list(1, 2)))
})


# impute_pdqr_fun ---------------------------------------------------------
test_that("impute_pdqr_fun works", {
  expect_equal(impute_pdqr_fun("p", NULL), new_p)
  expect_equal(impute_pdqr_fun("d", NULL), new_d)
  expect_equal(impute_pdqr_fun("q", NULL), new_q)
  expect_equal(impute_pdqr_fun("r", NULL), new_r)

  expect_equal(impute_pdqr_fun(NULL, p_fin), new_p)
})

test_that("impute_pdqr_fun throws errors", {
  expect_error(
    impute_pdqr_fun("a", r_fin), 'pdqr_class.*one of.*"p", "d", "q", "r"'
  )
})


# get_pdqr_class ----------------------------------------------------------
test_that("get_pdqr_class works", {
  expect_equal(get_pdqr_class(structure("a", class = "p")), "p")
  expect_equal(
    get_pdqr_class(structure("a", class = c("p", "d"))), "p"
  )
  expect_equal(get_pdqr_class(structure("a", class = "bbb")), NA_character_)
})


# Math.pdqr ---------------------------------------------------------------
test_that("Math.pdqr works", {
  lnorm_x <- seq(0, 100, by = 0.001)
  d_lnorm <- new_d(data.frame(x = lnorm_x, y = dlnorm(lnorm_x)))

  d_norm_ref <- new_d(data.frame(x = x_norm_seq, y = dnorm(x_norm_seq)))
  d_norm_out <- log(d_lnorm)

  expect_distr_fun(d_norm_out, "d", "infin")
  expect_equal_distr(
    d_norm_out, d_norm_ref,
    grid = x_norm_seq, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})


# Ops.pdqr ----------------------------------------------------------------
test_that("Ops.pdqr works", {
  unif_x <- seq(0, 1, by = 0.001)
  p_unif <- new_p(data.frame(x = unif_x, y = dunif(unif_x)))

  p_norm_ref <- new_p(data.frame(x = x_norm_seq, y = dnorm(x_norm_seq)))
  # Approximation of standard normal as centered sum of 12 uniform
  p_norm_out <- p_unif + p_unif + p_unif + p_unif + p_unif + p_unif +
    p_unif + p_unif + p_unif + p_unif + p_unif + p_unif - 6

  expect_distr_fun(p_norm_out, "p", "infin")
  expect_equal_distr(
    p_norm_out, p_norm_ref,
    grid = x_norm_seq, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that("Ops.pdqr warns about not numeric type", {
  expect_warning(p_fin >= p_infin, ">=.*logical.*[Cc]onvert")
})

test_that("Ops.pdqr works with for generics with one argument", {
  expect_warning(!p_fin, "!.*logical.*[Cc]onvert")
})


# Summary.pdqr ------------------------------------------------------------
test_that("Summary.pdqr works", {
  expect_distr_fun(min(p_fin), "p", "fin")
  expect_distr_fun(max(p_fin, p_fin), "p", "fin")
  expect_distr_fun(sum(q_custom, q_infin, q_infin), "q", "infin")
  expect_distr_fun(prod(r_custom, r_custom, na.rm = TRUE), "r", "infin")
})

test_that("Summary.pdqr throws error on `range()`", {
  expect_error(range(p_fin, p_fin), "range.*two.*numbers")
})
