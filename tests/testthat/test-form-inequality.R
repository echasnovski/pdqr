context("test-form-inequality")

set.seed(8888)


# Input data --------------------------------------------------------------
f_fin_1 <- new_d(data.frame(x = 0:2, prob = c(0.2, 0.3, 0.5)), "fin")
f_fin_2 <- new_d(data.frame(x = 1:3, prob = c(0.3, 0.2, 0.5)), "fin")
f_infin_1 <- new_d(data.frame(x = 0:2, y = c(0, 1/3, 1/3)), "infin")
f_infin_2 <- new_d(data.frame(x = 0:2 + 0.5, y = c(0, 1, 0)), "infin")

f_fin_1_dirac <- form_retype(f_fin_1, "infin", method = "dirac")
f_fin_2_dirac <- form_retype(f_fin_2, "infin", method = "dirac")


# Custom expectations -----------------------------------------------------
mean_sim_prob <- function(f_1, f_2, op, n_rep = 10, n_sim = 1e4) {
  r_f_1 <- as_r(f_1)
  r_f_2 <- as_r(f_2)

  sim_prob <- unlist(lapply(seq_len(n_rep), function(i) {
    is_op_true <- op(r_f_1(n_sim), r_f_2(n_sim))

    mean(is_op_true)
  }))

  mean(sim_prob)
}

expect_equal_probs <- function(f_1, f_2, form_f, sign_f, thres = 1e-2) {
  abs_prob_diff <- abs(form_f(f_1, f_2)(1) - mean_sim_prob(f_1, f_2, sign_f))

  expect_true(abs_prob_diff <= thres)
}


# form_geq ----------------------------------------------------------------
test_that("form_geq works", {
  output <- form_geq(f_infin_1, f_fin_2)
  expect_distr_fun(output, get_pdqr_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality with "infin" self returns 0.5
  expect_equal(form_geq(f_infin_1, f_infin_1)(1), 0.5)
})

test_that("form_geq agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_fin_1, f_fin_2, form_geq, `>=`)
  expect_equal_probs(f_fin_1, f_infin_2, form_geq, `>=`)
  expect_equal_probs(f_infin_1, f_fin_2, form_geq, `>=`)
  expect_equal_probs(f_infin_1, f_infin_2, form_geq, `>=`)
})

test_that("form_geq works with dirac-like functions", {
  expect_equal(
    form_geq(f_fin_1, f_infin_2)(1),
    form_geq(f_fin_1_dirac, f_infin_2)(1)
  )
  expect_equal(
    form_geq(f_infin_1, f_fin_2)(1),
    form_geq(f_infin_1, f_fin_2_dirac)(1)
  )
  expect_equal(form_geq(f_fin_1_dirac, f_fin_1_dirac)(1), 0.5)
})

test_that("form_geq returns appropriate pdqr class", {
  expect_is(form_geq(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# form_greater ------------------------------------------------------------
test_that("form_greater works", {
  output <- form_greater(f_infin_1, f_fin_2)
  expect_distr_fun(output, get_pdqr_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality with "infin" self returns 0.5
  expect_equal(form_greater(f_infin_1, f_infin_1)(1), 0.5)
})

test_that("form_greater agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_fin_1, f_fin_2, form_greater, `>`)
  expect_equal_probs(f_fin_1, f_infin_2, form_greater, `>`)
  expect_equal_probs(f_infin_1, f_fin_2, form_greater, `>`)
  expect_equal_probs(f_infin_1, f_infin_2, form_greater, `>`)
})

test_that("form_greater works with dirac-like functions", {
  expect_equal(
    form_greater(f_fin_1, f_infin_2)(1),
    form_greater(f_fin_1_dirac, f_infin_2)(1)
  )
  expect_equal(
    form_greater(f_infin_1, f_fin_2)(1),
    form_greater(f_infin_1, f_fin_2_dirac)(1)
  )
  expect_equal(form_greater(f_fin_1_dirac, f_fin_1_dirac)(1), 0.5)
})

test_that("form_greater returns appropriate pdqr class", {
  expect_is(form_greater(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# form_leq ----------------------------------------------------------------
test_that("form_leq works", {
  output <- form_leq(f_infin_1, f_fin_2)
  expect_distr_fun(output, get_pdqr_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality with "infin" self returns 0.5
  expect_equal(form_leq(f_infin_1, f_infin_1)(1), 0.5)
})

test_that("form_leq agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_fin_1, f_fin_2, form_leq, `<=`)
  expect_equal_probs(f_fin_1, f_infin_2, form_leq, `<=`)
  expect_equal_probs(f_infin_1, f_fin_2, form_leq, `<=`)
  expect_equal_probs(f_infin_1, f_infin_2, form_leq, `<=`)
})

test_that("form_leq works with dirac-like functions", {
  expect_equal(
    form_leq(f_fin_1, f_infin_2)(1),
    form_leq(f_fin_1_dirac, f_infin_2)(1)
  )
  expect_equal(
    form_leq(f_infin_1, f_fin_2)(1),
    form_leq(f_infin_1, f_fin_2_dirac)(1)
  )
  expect_equal(form_leq(f_fin_1_dirac, f_fin_1_dirac)(1), 0.5)
})

test_that("form_leq returns appropriate pdqr class", {
  expect_is(form_leq(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# form_less ---------------------------------------------------------------
test_that("form_less works", {
  output <- form_less(f_infin_1, f_fin_2)
  expect_distr_fun(output, get_pdqr_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality with "infin" self returns 0.5
  expect_equal(form_less(f_infin_1, f_infin_1)(1), 0.5)
})

test_that("form_less agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_fin_1, f_fin_2, form_less, `<`)
  expect_equal_probs(f_fin_1, f_infin_2, form_less, `<`)
  expect_equal_probs(f_infin_1, f_fin_2, form_less, `<`)
  expect_equal_probs(f_infin_1, f_infin_2, form_less, `<`)
})

test_that("form_less works with dirac-like functions", {
  expect_equal(
    form_less(f_fin_1, f_infin_2)(1),
    form_less(f_fin_1_dirac, f_infin_2)(1)
  )
  expect_equal(
    form_less(f_infin_1, f_fin_2)(1),
    form_less(f_infin_1, f_fin_2_dirac)(1)
  )
  expect_equal(form_less(f_fin_1_dirac, f_fin_1_dirac)(1), 0.5)
})

test_that("form_less returns appropriate pdqr class", {
  expect_is(form_less(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# boolean_pdqr ------------------------------------------------------------
# Tested in `form_*()` inequalities


# prob_geq ----------------------------------------------------------------
# Tested in `form_*()` inequalities


# prob_greater ------------------------------------------------------------
# Tested in `form_*()` inequalities


# prob_geq_fin_any --------------------------------------------------------
# Tested in `form_*()` inequalities


# prob_geq_infin_infin ----------------------------------------------------
# Tested in `form_*()` inequalities


# infin_geq_integral ------------------------------------------------------
# Tested in `form_*()` inequalities


# four_powers -------------------------------------------------------------
# Tested in `form_*()` inequalities
