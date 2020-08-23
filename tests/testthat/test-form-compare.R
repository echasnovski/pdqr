context("test-form-compare")

set.seed(8888)


# Input data --------------------------------------------------------------
f_dis_1 <- new_d(data.frame(x = 0:2, prob = c(0.2, 0.3, 0.5)), "discrete")
f_dis_2 <- new_d(data.frame(x = 1:3, prob = c(0.3, 0.2, 0.5)), "discrete")
f_dis_3 <- new_d(data.frame(x = 100:101, prob = c(0.5, 0.5)), "discrete")
f_con_1 <- new_d(data.frame(x = 0:2, y = c(0, 2 / 3, 2 / 3)), "continuous")
f_con_2 <- new_d(data.frame(x = 0:2 + 0.5, y = c(0, 1, 0)), "continuous")
f_con_3 <- new_d(data.frame(x = 100:101, y = c(1, 1)), "continuous")

f_dis_1_dirac <- form_retype(f_dis_1, "continuous", method = "dirac")
f_dis_2_dirac <- form_retype(f_dis_2, "continuous", method = "dirac")

# Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")
dirac_winsor <- form_resupport(d_unif, c(0, 0.5), method = "winsor")
dirac_single <- new_d(0.5, "continuous")

# Case of "long" "x"s
dirac_single_dis <- form_retype(dirac_single, "discrete", method = "piecelin")


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
  output <- form_geq(f_con_1, f_dis_2)
  expect_distr_fun(output, meta_class(f_con_1), "discrete")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality involving "discrete" functions with "long" "x"s
  expect_equal(form_geq(dirac_single_dis, dirac_single_dis)(1), 0.75)

  # Inequality with "continuous" self returns 0.5
  expect_equal(form_geq(f_con_1, f_con_1)(1), 0.5)
})

test_that("form_geq agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_dis_1, f_dis_2, form_geq, `>=`)
  expect_equal_probs(f_dis_1, f_con_2, form_geq, `>=`)
  expect_equal_probs(f_con_1, f_dis_2, form_geq, `>=`)
  expect_equal_probs(f_con_1, f_con_2, form_geq, `>=`)
})

test_that("form_geq handles cases of separated supports", {
  # Clear separation
  expect_equal(form_geq(f_dis_1, f_dis_3)(1), 0)
  expect_equal(form_geq(f_dis_3, f_dis_1)(1), 1)
  expect_equal(form_geq(f_con_1, f_con_3)(1), 0)
  expect_equal(form_geq(f_con_3, f_con_1)(1), 1)

  # "Touching" supports
  expect_equal(
    form_geq(new_d(1:2, "discrete"), new_d(2:3, "discrete"))(1), 0.25
  )
})

test_that("form_geq works with dirac-like 'continuous' functions", {
  expect_equal(
    form_geq(f_dis_1_dirac, f_con_2)(1),
    form_geq(f_dis_1, f_con_2)(1)
  )
  expect_equal(
    form_geq(f_con_1, f_dis_2_dirac)(1),
    form_geq(f_con_1, f_dis_2)(1)
  )
  expect_equal(form_geq(f_dis_1_dirac, f_dis_1_dirac)(1), 0.5)

  # Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
  expect_equal(form_geq(dirac_winsor, dirac_single)(1), 0.125)
  expect_equal(form_geq(dirac_single, dirac_winsor)(1), 0.875)
})

test_that("form_geq returns appropriate pdqr class", {
  expect_is(form_geq(as_p(f_dis_1), as_q(f_dis_2)), "p")
})


# form_greater ------------------------------------------------------------
test_that("form_greater works", {
  output <- form_greater(f_con_1, f_dis_2)
  expect_distr_fun(output, meta_class(f_con_1), "discrete")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality involving "discrete" functions with "long" "x"s
  expect_equal(form_greater(dirac_single_dis, dirac_single_dis)(1), 0.25)

  # Inequality with "continuous" self returns 0.5
  expect_equal(form_greater(f_con_1, f_con_1)(1), 0.5)
})

test_that("form_greater agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_dis_1, f_dis_2, form_greater, `>`)
  expect_equal_probs(f_dis_1, f_con_2, form_greater, `>`)
  expect_equal_probs(f_con_1, f_dis_2, form_greater, `>`)
  expect_equal_probs(f_con_1, f_con_2, form_greater, `>`)
})

test_that("form_greater handles cases of separated supports", {
  # Clear separation
  expect_equal(form_greater(f_dis_1, f_dis_3)(1), 0)
  expect_equal(form_greater(f_dis_3, f_dis_1)(1), 1)
  expect_equal(form_greater(f_con_1, f_con_3)(1), 0)
  expect_equal(form_greater(f_con_3, f_con_1)(1), 1)

  # "Touching" supports
  expect_equal(
    form_greater(new_d(1:2, "discrete"), new_d(2:3, "discrete"))(1), 0
  )
})

test_that("form_greater works with dirac-like 'continuous' functions", {
  expect_equal(
    form_greater(f_dis_1_dirac, f_con_2)(1),
    form_greater(f_dis_1, f_con_2)(1)
  )
  expect_equal(
    form_greater(f_con_1, f_dis_2_dirac)(1),
    form_greater(f_con_1, f_dis_2)(1)
  )
  expect_equal(form_greater(f_dis_1_dirac, f_dis_1_dirac)(1), 0.5)

  # Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
  expect_equal(form_greater(dirac_winsor, dirac_single)(1), 0.125)
  expect_equal(form_greater(dirac_single, dirac_winsor)(1), 0.875)
})

test_that("form_greater returns appropriate pdqr class", {
  expect_is(form_greater(as_p(f_dis_1), as_q(f_dis_2)), "p")
})


# form_leq ----------------------------------------------------------------
test_that("form_leq works", {
  output <- form_leq(f_con_1, f_dis_2)
  expect_distr_fun(output, meta_class(f_con_1), "discrete")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality involving "discrete" functions with "long" "x"s
  expect_equal(form_leq(dirac_single_dis, dirac_single_dis)(1), 0.75)

  # Inequality with "continuous" self returns 0.5
  expect_equal(form_leq(f_con_1, f_con_1)(1), 0.5)
})

test_that("form_leq agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_dis_1, f_dis_2, form_leq, `<=`)
  expect_equal_probs(f_dis_1, f_con_2, form_leq, `<=`)
  expect_equal_probs(f_con_1, f_dis_2, form_leq, `<=`)
  expect_equal_probs(f_con_1, f_con_2, form_leq, `<=`)
})

test_that("form_leq handles cases of separated supports", {
  # Clear separation
  expect_equal(form_leq(f_dis_1, f_dis_3)(1), 1)
  expect_equal(form_leq(f_dis_3, f_dis_1)(1), 0)
  expect_equal(form_leq(f_con_1, f_con_3)(1), 1)
  expect_equal(form_leq(f_con_3, f_con_1)(1), 0)

  # "Touching" supports
  expect_equal(form_leq(new_d(1:2, "discrete"), new_d(2:3, "discrete"))(1), 1)
})

test_that("form_leq works with dirac-like 'continuous' functions", {
  expect_equal(
    form_leq(f_dis_1_dirac, f_con_2)(1),
    form_leq(f_dis_1, f_con_2)(1)
  )
  expect_equal(
    form_leq(f_con_1, f_dis_2_dirac)(1),
    form_leq(f_con_1, f_dis_2)(1)
  )
  expect_equal(form_leq(f_dis_1_dirac, f_dis_1_dirac)(1), 0.5)

  # Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
  expect_equal(form_leq(dirac_winsor, dirac_single)(1), 0.875)
  expect_equal(form_leq(dirac_single, dirac_winsor)(1), 0.125)
})

test_that("form_leq returns appropriate pdqr class", {
  expect_is(form_leq(as_p(f_dis_1), as_q(f_dis_2)), "p")
})


# form_less ---------------------------------------------------------------
test_that("form_less works", {
  output <- form_less(f_con_1, f_dis_2)
  expect_distr_fun(output, meta_class(f_con_1), "discrete")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Inequality involving "discrete" functions with "long" "x"s
  expect_equal(form_less(dirac_single_dis, dirac_single_dis)(1), 0.25)

  # Inequality with "continuous" self returns 0.5
  expect_equal(form_less(f_con_1, f_con_1)(1), 0.5)
})

test_that("form_less agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_dis_1, f_dis_2, form_less, `<`)
  expect_equal_probs(f_dis_1, f_con_2, form_less, `<`)
  expect_equal_probs(f_con_1, f_dis_2, form_less, `<`)
  expect_equal_probs(f_con_1, f_con_2, form_less, `<`)
})

test_that("form_less handles cases of separated supports", {
  # Clear separation
  expect_equal(form_less(f_dis_1, f_dis_3)(1), 1)
  expect_equal(form_less(f_dis_3, f_dis_1)(1), 0)
  expect_equal(form_less(f_con_1, f_con_3)(1), 1)
  expect_equal(form_less(f_con_3, f_con_1)(1), 0)

  # "Touching" supports
  expect_equal(
    form_less(new_d(1:2, "discrete"), new_d(2:3, "discrete"))(1), 0.75
  )
})

test_that("form_less works with dirac-like 'continuous' functions", {
  expect_equal(
    form_less(f_dis_1_dirac, f_con_2)(1),
    form_less(f_dis_1, f_con_2)(1)
  )
  expect_equal(
    form_less(f_con_1, f_dis_2_dirac)(1),
    form_less(f_con_1, f_dis_2)(1)
  )
  expect_equal(form_less(f_dis_1_dirac, f_dis_1_dirac)(1), 0.5)

  # Case of symmetrical dirac-like entry and asymmetrical one after winsorizing
  expect_equal(form_less(dirac_winsor, dirac_single)(1), 0.875)
  expect_equal(form_less(dirac_single, dirac_winsor)(1), 0.125)
})

test_that("form_less returns appropriate pdqr class", {
  expect_is(form_less(as_p(f_dis_1), as_q(f_dis_2)), "p")
})


# form_equal --------------------------------------------------------------
test_that("form_equal works", {
  output <- form_equal(f_con_1, f_dis_2)
  expect_distr_fun(output, meta_class(f_con_1), "discrete")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Equality involving "discrete" functions with "long" "x"s
  expect_equal(
    form_equal(dirac_single_dis, dirac_single_dis)(1),
    sum(meta_x_tbl(dirac_single_dis)[["prob"]]^2)
  )
})

test_that("form_equal agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_dis_1, f_dis_2, form_equal, `==`)
  expect_equal_probs(f_dis_1, f_con_2, form_equal, `==`)
  expect_equal_probs(f_con_1, f_dis_2, form_equal, `==`)
  expect_equal_probs(f_con_1, f_con_2, form_equal, `==`)
})

test_that("form_equal works with dirac-like 'continuous' functions", {
  expect_equal(
    form_equal(f_dis_1_dirac, f_con_2)(1),
    form_equal(f_dis_1, f_con_2)(1)
  )
  expect_equal(
    form_equal(f_con_1, f_dis_2_dirac)(1),
    form_equal(f_con_1, f_dis_2)(1)
  )

  expect_equal(form_equal(dirac_single, dirac_winsor)(1), 0)
})

test_that("form_equal returns appropriate pdqr class", {
  expect_is(form_equal(as_p(f_dis_1), as_q(f_dis_2)), "p")
})


# form_not_equal ----------------------------------------------------------
test_that("form_not_equal works", {
  output <- form_not_equal(f_con_1, f_dis_2)
  expect_distr_fun(output, meta_class(f_con_1), "discrete")
  expect_equal(meta_x_tbl(output)[["x"]], c(0, 1))

  # Not equality involving "discrete" functions with "long" "x"s
  expect_equal(
    form_not_equal(dirac_single_dis, dirac_single_dis)(1),
    1 - sum(meta_x_tbl(dirac_single_dis)[["prob"]]^2)
  )
})

test_that("form_not_equal agrees with simulation", {
  skip_on_cran()

  expect_equal_probs(f_dis_1, f_dis_2, form_not_equal, `!=`)
  expect_equal_probs(f_dis_1, f_con_2, form_not_equal, `!=`)
  expect_equal_probs(f_con_1, f_dis_2, form_not_equal, `!=`)
  expect_equal_probs(f_con_1, f_con_2, form_not_equal, `!=`)
})

test_that("form_not_equal works with dirac-like 'continuous' functions", {
  expect_equal(
    form_not_equal(f_dis_1_dirac, f_con_2)(1),
    form_not_equal(f_dis_1, f_con_2)(1)
  )
  expect_equal(
    form_not_equal(f_con_1, f_dis_2_dirac)(1),
    form_not_equal(f_con_1, f_dis_2)(1)
  )

  expect_equal(form_not_equal(dirac_single, dirac_winsor)(1), 1)
})

test_that("form_not_equal returns appropriate pdqr class", {
  expect_is(form_not_equal(as_p(f_dis_1), as_q(f_dis_2)), "p")
})


# prob_geq ----------------------------------------------------------------
# Tested in `form_*()` comparisons


# prob_equal --------------------------------------------------------------
# Tested in `form_*()` comparisons


# prob_greater ------------------------------------------------------------
# Tested in `form_*()` comparisons


# prob_geq_dis_any --------------------------------------------------------
# Tested in `form_*()` comparisons


# prob_geq_con_con --------------------------------------------------------
# Tested in `form_*()` comparisons


# con_geq_integral --------------------------------------------------------
# Tested in `form_*()` comparisons
