context("test-form_trans")

set.seed(7777)


# Input data --------------------------------------------------------------
sq <- function(x) {
  x * x
}

# Analytical form of random variable `X^2`
p_custom_ref <- structure(
  function(q) {
    user_p(sqrt(q)) - user_p(-sqrt(q))
  },
  class = c("p", "pdqr", "function")
)
assign("type", "continuous", environment(p_custom_ref))
assign("support", c(0, 1), environment(p_custom_ref))

x_norm_seq <- seq(-10, 10, by = 0.01)


# form_trans --------------------------------------------------------------
test_that("form_trans works with `method = 'random'`", {
  output_custom <- form_trans(list(p_custom), sq, method = "random")
  expect_distr_fun(output_custom, "p", "continuous")
  expect_equal_distr(
    output_custom, p_custom_ref,
    grid = x_custom_trunc, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )

  # Normal distribution multiplied by 2
  d_norm_input <- new_d(rnorm(10000, mean = 0, sd = 1), "continuous")

  norm_seq <- seq(-20, 20, by = 0.001)
  d_norm_ref <- new_d(
    data.frame(x = norm_seq, y = dnorm(norm_seq, mean = 0, sd = 2)),
    type = "continuous"
  )

  output_norm <- form_trans(list(2, d_norm_input), `*`, method = "random")
  expect_distr_fun(output_norm, "d", "continuous")
  expect_equal_distr(
    output_norm, d_norm_ref,
    grid = x_norm_seq, thres = 0.02,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that("form_trans works with `method = 'bruteforce'`", {
  # Output type "discrete"
  ## Single input
  add_one <- function(x) {
    x + 1
  }
  cur_dis <- new_d(data.frame(x = 0:2, prob = c(0.1, 0.2, 0.7)), "discrete")
  expect_ref_x_tbl(
    form_trans(list(cur_dis), add_one, method = "bruteforce"),
    data.frame(x = 1:3, prob = c(0.1, 0.2, 0.7))
  )

  ## Multiple input
  cur_dis_2 <- new_p(data.frame(x = 1:2, prob = c(0.4, 0.6)), "discrete")
  expect_ref_x_tbl(
    form_trans(list(cur_dis, cur_dis_2), `+`, method = "bruteforce"),
    data.frame(
      x    = c(1,         2,                     3,
        4),
      prob = c(0.1 * 0.4, 0.1 * 0.6 + 0.2 * 0.4, 0.2 * 0.6 + 0.7 * 0.4,
        0.7 * 0.6)
    )
  )

  ## Output of `trans` is logical
  d_geq <- form_trans(list(0, d_con), `>=`, method = "bruteforce")
  expect_true(abs(d_geq(1) - p_con(0)) <= 5e-3)

  # Output type "continuous"
  output <- form_trans(list(d_con, 1), `+`, method = "bruteforce")
  expect_distr_fun(output, "d", "continuous")
  output_support <- meta_support(output)
  expect_true(all(abs(output_support - (x_con_support + 1)) < 1e-3))
})

test_that("form_trans returns boolean pdqr-function for 'lgl' `trans` output", {
  d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")
  bool_random <- form_trans(list(d_unif, 100), `>`, method = "random")
  expect_true(is_boolean_pdqr_fun(bool_random))
  bool_brute <- form_trans(list(d_unif, 100), `>`, method = "bruteforce")
  expect_true(is_boolean_pdqr_fun(bool_brute))
})

test_that("form_trans handles `NA` in case of logical `trans` output", {
  d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")
  na_geq <- function(x) {
    res <- x >= 0.25
    res[x < 0.75] <- NA

    res
  }

  bool_random <- form_trans(list(d_unif), na_geq, method = "random")
  expect_true(is_boolean_pdqr_fun(bool_random))
  # Probability of being true is 1 here because it is estimated only on non-`NA`
  # logical values. Here they are not `NA` if `x >= 0.75` it true. Exact logical
  # value is then computed as `x >= 0.25` which is always `TRUE`.
  expect_equal(summ_prob_true(bool_random), 1)

  bool_brute <- form_trans(list(d_unif), na_geq, method = "bruteforce")
  expect_true(is_boolean_pdqr_fun(bool_brute))
  # Here probability of being true is 0 becuase it is estimated only on center
  # of single interval, whcih is 0.5. For it `na_geq()` returns `NA`, which
  # means that output probability is `mean(NA, na.rm = TRUE)` which is zero.
  expect_equal(summ_prob_true(bool_brute), 0)
})

test_that("form_trans produces correct 'pdqr' class", {
  expect_is(form_trans(list(1, p_dis), `+`, method = "random"), "p")
  expect_is(form_trans(list(p_dis, 1), `+`, method = "random"), "p")
  expect_is(form_trans(list(d_dis, p_dis), `+`, method = "random"), "d")
  expect_is(form_trans(list(p_dis, d_dis), `+`, method = "random"), "p")

  expect_is(form_trans(list(1, p_dis), `+`, method = "bruteforce"), "p")
  expect_is(form_trans(list(p_dis, 1), `+`, method = "bruteforce"), "p")
  expect_is(form_trans(list(d_dis, p_dis), `+`, method = "bruteforce"), "d")
  expect_is(form_trans(list(p_dis, d_dis), `+`, method = "bruteforce"), "p")
})

test_that("form_trans produces correct 'pdqr' type", {
  expect_equal(
    meta_type(form_trans(list(1, p_dis), `+`, method = "random")), "discrete"
  )
  expect_equal(
    meta_type(form_trans(list(p_dis, q_dis), `+`, method = "random")),
    "discrete"
  )
  expect_equal(
    meta_type(form_trans(list(p_con, q_dis), `+`, method = "random")),
    "continuous"
  )
  expect_equal(
    meta_type(form_trans(list(d_dis, p_con), `+`, method = "random")),
    "continuous"
  )
  expect_equal(
    meta_type(form_trans(list(p_con, d_con), `+`, method = "random")),
    "continuous"
  )

  expect_equal(
    meta_type(form_trans(list(1, p_dis), `+`, method = "bruteforce")),
    "discrete"
  )
  expect_equal(
    meta_type(form_trans(list(p_dis, q_dis), `+`, method = "bruteforce")),
    "discrete"
  )
  expect_equal(
    meta_type(form_trans(list(p_con, q_dis), `+`, method = "bruteforce")),
    "continuous"
  )

  # If `trans` produces logical output, type should be "discrete"
  expect_equal(
    meta_type(form_trans(list(p_con, q_dis), `>=`, method = "random")),
    "discrete"
  )
  expect_equal(
    meta_type(form_trans(list(p_con, q_dis), `>=`, method = "bruteforce")),
    "discrete"
  )
})

test_that("form_trans throws error if `trans` produces bad output",  {
  bad_trans <- function(x) {
    rep("a", length(x))
  }
  expect_error(
    form_trans(list(p_dis), bad_trans, method = "random"),
    "transformation.*numeric.*logical"
  )
  expect_error(
    form_trans(list(p_dis), bad_trans, method = "bruteforce"),
    "transformation.*numeric.*logical"
  )
})

test_that("form_trans uses `...` as `trans` arguments",  {
  adder <- function(x, y) {
    x + y
  }

  output_random <- form_trans(list(p_dis), adder, y = 100, method = "random")
  expect_true(meta_support(p_dis)[1] == meta_support(output_random)[1] - 100)

  output_brute <- form_trans(list(p_dis), adder, y = 100, method = "bruteforce")
  expect_true(meta_support(p_dis)[1] == meta_support(output_brute)[1] - 100)
})

test_that("form_trans uses `n_sample` argument",  {
  # It is used with "random" method
  output_random <- form_trans(list(p_dis), sq, method = "random", n_sample = 1)
  expect_equal(nrow(meta_x_tbl(output_random)), 1)

  # It is not used with "bruteforce" method
  output_brute <- form_trans(
    list(p_dis), sq, method = "bruteforce", n_sample = 1
  )
  expect_equal(nrow(meta_x_tbl(output_brute)), nrow(meta_x_tbl(p_dis)))
})

test_that("form_trans uses `args_new` as arguments for `new_*()`",  {
  output <- form_trans(
    list(p_con), sq, method = "random", args_new = list(n = 3)
  )
  expect_equal(nrow(meta_x_tbl(output)), 3)

  # Currently there is no effect in using `args_new` in case `method =
  # "bruteforce"` because it computes data frame input for `new_*()` functions,
  # in which case extra arguments are currently not used. However, `args_new`
  # are passed to `new_*()` in the code.
  expect_equal_meta(
    form_trans(list(p_dis), sq, method = "bruteforce", args_new = list(n = 3)),
    form_trans(list(p_dis), sq, method = "bruteforce")
  )

  # Supplying `x` or `type` doesn't affect the output
  set.seed(101)
  f <- form_trans(
    list(p_con),
    sq,
    method = "random",
    args_new = list(x = 1, type = "discrete")
  )
  set.seed(101)
  g <- form_trans(list(p_con), sq, method = "random")
  expect_equal_meta(f, g)

  expect_equal_meta(
    form_trans(
      list(p_dis), sq, method = "bruteforce",
      args_new = list(x = 1, type = "continuous")
    ),
    form_trans(list(p_dis), sq, method = "bruteforce")
  )
})

test_that("form_trans validates input", {
  expect_error(form_trans(trans = sq), "`f_list`.*missing.*list of")
  expect_error(form_trans(p_dis, sq), "`f_list`.*list")
  expect_error(form_trans(list(), sq), "`f_list`.*empty")
  expect_error(form_trans(list("a"), sq), "`f_list`.*pdqr-function.*number")
  expect_error(form_trans(list(1), sq), "`f_list`.*one.*pdqr-function")
  expect_error(form_trans(list(p_dis)), "`trans`.*missing.*transformation")
  expect_error(form_trans(list(p_dis), 1), "`trans`.*function")
  expect_error(form_trans(list(p_dis), sq, method = 1), "`method`.*string")
  expect_error(form_trans(list(p_dis), sq, method = "a"), "`method`.*one of")
  expect_error(
    form_trans(list(p_dis), sq, n_sample = "a"), "`n_sample`.*single number"
  )
  expect_error(
    form_trans(list(p_dis), sq, n_sample = 1:2), "`n_sample`.*single number"
  )
  expect_error(
    form_trans(list(p_dis), sq, args_new = "a"), "`args_new`.*list"
  )
})


# form_trans_self ---------------------------------------------------------
# This is a thin wrapper for `form_trans()` so main tests are done in
# `form_trans()`
test_that("form_trans_self works",  {
  expect_equal_meta(
    form_trans_self(p_dis, sq, method = "bruteforce"),
    form_trans(list(p_dis), sq, method = "bruteforce")
  )

  # Default method should be "random"
  cur_dis <- new_d(
    data.frame(x = c(-2, 0, 1), prob = c(0.1, 0.7, 0.2)), "discrete"
  )
  output <- form_trans_self(cur_dis, `-`)
  output_x_tbl <- meta_x_tbl(output)
  expect_equal(output_x_tbl[["x"]], c(-1, 0, 2))
  expect_true(max(abs(output_x_tbl[["prob"]] - c(0.2, 0.7, 0.1))) < 2e-2)
})

test_that("form_trans_self validates input", {
  expect_error(form_trans_self("a", sq), "f.*not pdqr-function")
  expect_error(
    form_trans_self(d_dis), "`trans`.*missing.*transformation function"
  )
  # Validation of other arguments is tested in `form_trans()`
})


# trans_random ------------------------------------------------------------
# Tested in `form_trans()`


# trans_bruteforce --------------------------------------------------------
# Tested in `form_trans()`


# list_grid ---------------------------------------------------------------
# Tested in `form_trans()`


# assert_trans_output -----------------------------------------------------
# Tested in `form_trans()`
