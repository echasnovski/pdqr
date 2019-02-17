context("test-form_trans")

set.seed(7777)


# Reference data ----------------------------------------------------------
sq <- function(x) {x * x}

# Analytical form of random variable `X^2`
p_custom_ref <- structure(
  function(q) {
    user_p(sqrt(q)) - user_p(-sqrt(q))
  },
  class = c("p", "pdqr", "function")
)
assign("type", "infin", environment(p_custom_ref))
assign("support", c(0, 1), environment(p_custom_ref))

x_norm_seq <- seq(-10, 10, by = 0.01)


# form_trans --------------------------------------------------------------
test_that("form_trans works", {
  output_custom <- form_trans(list(p_custom), sq)
  expect_distr_fun(output_custom, "p", "infin")
  expect_equal_distr(
    output_custom, p_custom_ref,
    grid = x_custom_trunc, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )

  # Normal distribution multiplied by 2
  d_norm_input <- new_d(rnorm(10000, mean = 0, sd = 1))

  norm_seq <- seq(-20, 20, by = 0.001)
  d_norm_ref <- new_d(
    data.frame(x = norm_seq, y = dnorm(norm_seq, mean = 0, sd = 2))
  )

  output_norm <- form_trans(list(2, d_norm_input), `*`)
  expect_distr_fun(output_norm, "d", "infin")
  expect_equal_distr(
    output_norm, d_norm_ref,
    grid = x_norm_seq, thres = 0.02,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that("form_trans produces correct 'pdqr' class", {
  expect_is(form_trans(list(1, p_fin), `+`), "p")
  expect_is(form_trans(list(p_fin, 1), `+`), "p")
  expect_is(form_trans(list(d_fin, p_fin), `+`), "d")
  expect_is(form_trans(list(p_fin, d_fin), `+`), "p")
})

test_that("form_trans produces correct 'pdqr' type", {
  expect_equal(meta_type(form_trans(list(1, p_fin), `+`)), "fin")
  expect_equal(meta_type(form_trans(list(p_fin, q_fin), `+`)), "fin")
  expect_equal(meta_type(form_trans(list(p_infin, q_fin), `+`)), "infin")
  expect_equal(meta_type(form_trans(list(d_fin, p_infin), `+`)), "infin")
  expect_equal(meta_type(form_trans(list(p_infin, d_infin), `+`)), "infin")

  # If `trans` produces logical output, type should be "fin"
  expect_equal(meta_type(form_trans(list(p_infin, q_infin), `>=`)), "fin")
})

test_that("form_trans throws error if `trans` produces bad output",  {
  bad_trans <- function(x) {rep("a", length(x))}
  expect_error(
    form_trans(list(p_fin), bad_trans), "transformation.*numeric.*logical"
  )
})

test_that("form_trans uses `...` as `density` argument",  {
  output <- form_trans(list(p_infin), sq, n = 3)
  expect_equal(nrow(meta_x_tbl(output)), 3)
})

test_that("form_trans uses `n_sample` argument",  {
  output <- form_trans(list(p_fin), sq, n_sample = 1)
  expect_equal(nrow(meta_x_tbl(output)), 1)
})

test_that("form_trans throws errors", {
  expect_error(form_trans(p_fin, sq), "`f_list`.*list")
  expect_error(form_trans(list("a"), sq), "`f_list`.*pdqr-function.*number")
  expect_error(form_trans(list(1), sq), "`f_list`.*one.*pdqr-function")
  expect_error(form_trans(list(p_fin), 1), "`trans`.*function")
  expect_error(
    form_trans(list(p_fin), sq, n_sample = "a"), "`n_sample`.*single number"
  )
  expect_error(
    form_trans(list(p_fin), sq, n_sample = 1:2), "`n_sample`.*single number"
  )
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

test_that("Ops.pdqr works with logical functions", {
  d_leq <- d_fin <= 3
  expect_true(abs(d_leq(1) - p_fin(3)) <= 0.1)

  d_negate <- !d_fin
  expect_equal(d_negate(1), 0)
})

test_that("Ops.pdqr works with for generics with one argument", {
  d_minus <- -d_fin
  expect_true(abs(d_minus(-1) - d_fin(1)) <= 0.1)
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
