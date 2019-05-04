context("test-form-compare")

set.seed(8888)


# Input data --------------------------------------------------------------
f_fin_1 <- new_d(data.frame(x = 0:2, prob = c(0.2, 0.3, 0.5)), "fin")
f_fin_2 <- new_d(data.frame(x = 1:3, prob = c(0.3, 0.2, 0.5)), "fin")
f_fin_3 <- new_d(data.frame(x = 100:101, prob = c(0.5, 0.5)), "fin")
f_infin_1 <- new_d(data.frame(x = 0:2, y = c(0, 2/3, 2/3)), "infin")
f_infin_2 <- new_d(data.frame(x = 0:2 + 0.5, y = c(0, 1, 0)), "infin")
f_infin_3 <- new_d(data.frame(x = 100:101, y = c(1, 1)), "infin")

f_fin_1_dirac <- form_retype(f_fin_1, "infin", method = "dirac")
f_fin_2_dirac <- form_retype(f_fin_2, "infin", method = "dirac")

# Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "infin")
dirac_winsor <- form_resupport(d_unif, c(0, 0.5), method = "winsor")
dirac_single <- new_d(0.5, "infin")

# Case of "long" "x"s
dirac_single_fin <- form_retype(dirac_single, "fin")


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
  expect_distr_fun(output, meta_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality involving "fin" functions with "long" "x"s
  expect_equal(form_geq(dirac_single_fin, dirac_single_fin)(1), 0.75)

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

test_that("form_geq handles cases of clearly separated supports", {
  expect_equal(form_geq(f_fin_1, f_fin_3)(1), 0)
  expect_equal(form_geq(f_fin_3, f_fin_1)(1), 1)
  expect_equal(form_geq(f_infin_1, f_infin_3)(1), 0)
  expect_equal(form_geq(f_infin_3, f_infin_1)(1), 1)
})

test_that("form_geq works with dirac-like 'infin' functions", {
  expect_equal(
    form_geq(f_fin_1_dirac, f_infin_2)(1),
    form_geq(f_fin_1, f_infin_2)(1)
  )
  expect_equal(
    form_geq(f_infin_1, f_fin_2_dirac)(1),
    form_geq(f_infin_1, f_fin_2)(1)
  )
  expect_equal(form_geq(f_fin_1_dirac, f_fin_1_dirac)(1), 0.5)

  # Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
  expect_equal(form_geq(dirac_winsor, dirac_single)(1), 0.125)
  expect_equal(form_geq(dirac_single, dirac_winsor)(1), 0.875)
})

test_that("form_geq returns appropriate pdqr class", {
  expect_is(form_geq(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# form_greater ------------------------------------------------------------
test_that("form_greater works", {
  output <- form_greater(f_infin_1, f_fin_2)
  expect_distr_fun(output, meta_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality involving "fin" functions with "long" "x"s
  expect_equal(form_greater(dirac_single_fin, dirac_single_fin)(1), 0.25)

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

test_that("form_greater handles cases of clearly separated supports", {
  expect_equal(form_greater(f_fin_1, f_fin_3)(1), 0)
  expect_equal(form_greater(f_fin_3, f_fin_1)(1), 1)
  expect_equal(form_greater(f_infin_1, f_infin_3)(1), 0)
  expect_equal(form_greater(f_infin_3, f_infin_1)(1), 1)
})

test_that("form_greater works with dirac-like 'infin' functions", {
  expect_equal(
    form_greater(f_fin_1_dirac, f_infin_2)(1),
    form_greater(f_fin_1, f_infin_2)(1)
  )
  expect_equal(
    form_greater(f_infin_1, f_fin_2_dirac)(1),
    form_greater(f_infin_1, f_fin_2)(1)
  )
  expect_equal(form_greater(f_fin_1_dirac, f_fin_1_dirac)(1), 0.5)

  # Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
  expect_equal(form_greater(dirac_winsor, dirac_single)(1), 0.125)
  expect_equal(form_greater(dirac_single, dirac_winsor)(1), 0.875)
})

test_that("form_greater returns appropriate pdqr class", {
  expect_is(form_greater(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# form_leq ----------------------------------------------------------------
test_that("form_leq works", {
  output <- form_leq(f_infin_1, f_fin_2)
  expect_distr_fun(output, meta_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality involving "fin" functions with "long" "x"s
  expect_equal(form_leq(dirac_single_fin, dirac_single_fin)(1), 0.75)

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

test_that("form_leq handles cases of clearly separated supports", {
  expect_equal(form_leq(f_fin_1, f_fin_3)(1), 1)
  expect_equal(form_leq(f_fin_3, f_fin_1)(1), 0)
  expect_equal(form_leq(f_infin_1, f_infin_3)(1), 1)
  expect_equal(form_leq(f_infin_3, f_infin_1)(1), 0)
})

test_that("form_leq works with dirac-like 'infin' functions", {
  expect_equal(
    form_leq(f_fin_1_dirac, f_infin_2)(1),
    form_leq(f_fin_1, f_infin_2)(1)
  )
  expect_equal(
    form_leq(f_infin_1, f_fin_2_dirac)(1),
    form_leq(f_infin_1, f_fin_2)(1)
  )
  expect_equal(form_leq(f_fin_1_dirac, f_fin_1_dirac)(1), 0.5)

  # Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
  expect_equal(form_leq(dirac_winsor, dirac_single)(1), 0.875)
  expect_equal(form_leq(dirac_single, dirac_winsor)(1), 0.125)
})

test_that("form_leq returns appropriate pdqr class", {
  expect_is(form_leq(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# form_less ---------------------------------------------------------------
test_that("form_less works", {
  output <- form_less(f_infin_1, f_fin_2)
  expect_distr_fun(output, meta_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality involving "fin" functions with "long" "x"s
  expect_equal(form_less(dirac_single_fin, dirac_single_fin)(1), 0.25)

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

test_that("form_less handles cases of clearly separated supports", {
  expect_equal(form_less(f_fin_1, f_fin_3)(1), 1)
  expect_equal(form_less(f_fin_3, f_fin_1)(1), 0)
  expect_equal(form_less(f_infin_1, f_infin_3)(1), 1)
  expect_equal(form_less(f_infin_3, f_infin_1)(1), 0)
})

test_that("form_less works with dirac-like 'infin' functions", {
  expect_equal(
    form_less(f_fin_1_dirac, f_infin_2)(1),
    form_less(f_fin_1, f_infin_2)(1)
  )
  expect_equal(
    form_less(f_infin_1, f_fin_2_dirac)(1),
    form_less(f_infin_1, f_fin_2)(1)
  )
  expect_equal(form_less(f_fin_1_dirac, f_fin_1_dirac)(1), 0.5)

  # Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
  expect_equal(form_less(dirac_winsor, dirac_single)(1), 0.875)
  expect_equal(form_less(dirac_single, dirac_winsor)(1), 0.125)
})

test_that("form_less returns appropriate pdqr class", {
  expect_is(form_less(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# form_equal --------------------------------------------------------------
test_that("form_equal works", {
  output <- form_equal(f_infin_1, f_fin_2)
  expect_distr_fun(output, meta_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Equality involving "fin" functions with "long" "x"s
  expect_equal(
    form_equal(dirac_single_fin, dirac_single_fin)(1),
    sum(meta_x_tbl(dirac_single_fin)[["prob"]]^2)
  )
})

test_that("form_equal agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_fin_1, f_fin_2, form_equal, `==`)
  expect_equal_probs(f_fin_1, f_infin_2, form_equal, `==`)
  expect_equal_probs(f_infin_1, f_fin_2, form_equal, `==`)
  expect_equal_probs(f_infin_1, f_infin_2, form_equal, `==`)
})

test_that("form_equal works with dirac-like 'infin' functions", {
  expect_equal(
    form_equal(f_fin_1_dirac, f_infin_2)(1),
    form_equal(f_fin_1, f_infin_2)(1)
  )
  expect_equal(
    form_equal(f_infin_1, f_fin_2_dirac)(1),
    form_equal(f_infin_1, f_fin_2)(1)
  )

  expect_equal(form_equal(dirac_single, dirac_winsor)(1), 0)
})

test_that("form_equal returns appropriate pdqr class", {
  expect_is(form_equal(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# form_not_equal ----------------------------------------------------------
test_that("form_not_equal works", {
  output <- form_not_equal(f_infin_1, f_fin_2)
  expect_distr_fun(output, meta_class(f_infin_1), "fin")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Not equality involving "fin" functions with "long" "x"s
  expect_equal(
    form_not_equal(dirac_single_fin, dirac_single_fin)(1),
    1 - sum(meta_x_tbl(dirac_single_fin)[["prob"]]^2)
  )
})

test_that("form_not_equal agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_fin_1, f_fin_2, form_not_equal, `!=`)
  expect_equal_probs(f_fin_1, f_infin_2, form_not_equal, `!=`)
  expect_equal_probs(f_infin_1, f_fin_2, form_not_equal, `!=`)
  expect_equal_probs(f_infin_1, f_infin_2, form_not_equal, `!=`)
})

test_that("form_not_equal works with dirac-like 'infin' functions", {
  expect_equal(
    form_not_equal(f_fin_1_dirac, f_infin_2)(1),
    form_not_equal(f_fin_1, f_infin_2)(1)
  )
  expect_equal(
    form_not_equal(f_infin_1, f_fin_2_dirac)(1),
    form_not_equal(f_infin_1, f_fin_2)(1)
  )

  expect_equal(form_not_equal(dirac_single, dirac_winsor)(1), 1)
})

test_that("form_not_equal returns appropriate pdqr class", {
  expect_is(form_not_equal(as_p(f_fin_1), as_q(f_fin_2)), "p")
})


# prob_geq ----------------------------------------------------------------
# Tested in `form_*()` comparisons


# prob_equal --------------------------------------------------------------
# Tested in `form_*()` comparisons


# prob_greater ------------------------------------------------------------
# Tested in `form_*()` comparisons


# prob_geq_fin_any --------------------------------------------------------
# Tested in `form_*()` comparisons


# prob_geq_infin_infin ----------------------------------------------------
# Tested in `form_*()` comparisons


# infin_geq_integral ------------------------------------------------------
# Tested in `form_*()` comparisons
