context("test-form_trans")

set.seed(7777)


# Input data --------------------------------------------------------------
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
test_that("form_trans works with `method = 'random'`", {
  output_custom <- form_trans(list(p_custom), sq, method = "random")
  expect_distr_fun(output_custom, "p", "infin")
  expect_equal_distr(
    output_custom, p_custom_ref,
    grid = x_custom_trunc, thres = 0.05,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )

  # Normal distribution multiplied by 2
  d_norm_input <- new_d(rnorm(10000, mean = 0, sd = 1), "infin")

  norm_seq <- seq(-20, 20, by = 0.001)
  d_norm_ref <- new_d(
    data.frame(x = norm_seq, y = dnorm(norm_seq, mean = 0, sd = 2)), "infin"
  )

  output_norm <- form_trans(list(2, d_norm_input), `*`, method = "random")
  expect_distr_fun(output_norm, "d", "infin")
  expect_equal_distr(
    output_norm, d_norm_ref,
    grid = x_norm_seq, thres = 0.02,
    # Support and "x_tbl" shouldn't be the same as random sampling is done
    meta_not_check = c("x_tbl", "support")
  )
})

test_that("form_trans works with `method = 'bruteforce'`", {
  # Output type "fin"
    # Single input
  add_one <- function(x) {x + 1}
  cur_fin <- new_d(data.frame(x = 0:2, prob = c(0.1, 0.2, 0.7)), "fin")
  expect_ref_x_tbl(
    form_trans(list(cur_fin), add_one, method = "bruteforce"),
    data.frame(x = 1:3, prob = c(0.1, 0.2, 0.7))
  )

    # Multiple input
  cur_fin_2 <- new_p(data.frame(x = 1:2, prob = c(0.4, 0.6)), "fin")
  expect_ref_x_tbl(
    form_trans(list(cur_fin, cur_fin_2), `+`, method = "bruteforce"),
    data.frame(
      x =    c(      1,               2,               3,       4),
      prob = c(0.1*0.4, 0.1*0.6+0.2*0.4, 0.2*0.6+0.7*0.4, 0.7*0.6)
    )
  )

    # Output of `trans` is logical
  d_geq <- form_trans(list(0, d_infin), `>=`, method = "bruteforce")
  expect_true(abs(d_geq(1) - p_infin(0)) <= 5e-3)

  # Output type "infin"
  output <- form_trans(list(d_infin, 1), `+`, method = "bruteforce")
  expect_distr_fun(output, "d", "infin")
  output_support <- meta_support(output)
  expect_true(all(abs(output_support - (x_infin_support+1)) < 1e-3))
})

test_that("form_trans returns boolean pdqr-function for 'lgl' `trans` output", {
  d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "infin")
  bool_random <- form_trans(list(d_unif, 100), `>`, method = "random")
  expect_true(is_boolean_pdqr_fun(bool_random))
  bool_brute <- form_trans(list(d_unif, 100), `>`, method = "bruteforce")
  expect_true(is_boolean_pdqr_fun(bool_brute))
})

test_that("form_trans produces correct 'pdqr' class", {
  expect_is(form_trans(list(1, p_fin), `+`, method = "random"), "p")
  expect_is(form_trans(list(p_fin, 1), `+`, method = "random"), "p")
  expect_is(form_trans(list(d_fin, p_fin), `+`, method = "random"), "d")
  expect_is(form_trans(list(p_fin, d_fin), `+`, method = "random"), "p")

  expect_is(form_trans(list(1, p_fin), `+`, method = "bruteforce"), "p")
  expect_is(form_trans(list(p_fin, 1), `+`, method = "bruteforce"), "p")
  expect_is(form_trans(list(d_fin, p_fin), `+`, method = "bruteforce"), "d")
  expect_is(form_trans(list(p_fin, d_fin), `+`, method = "bruteforce"), "p")
})

test_that("form_trans produces correct 'pdqr' type", {
  expect_equal(
    meta_type(form_trans(list(1, p_fin), `+`, method = "random")), "fin"
  )
  expect_equal(
    meta_type(form_trans(list(p_fin, q_fin), `+`, method = "random")), "fin"
  )
  expect_equal(
    meta_type(form_trans(list(p_infin, q_fin), `+`, method = "random")), "infin"
  )
  expect_equal(
    meta_type(form_trans(list(d_fin, p_infin), `+`, method = "random")), "infin"
  )
  expect_equal(
    meta_type(form_trans(list(p_infin, d_infin), `+`, method = "random")),
    "infin"
  )

  expect_equal(
    meta_type(form_trans(list(1, p_fin), `+`, method = "bruteforce")), "fin"
  )
  expect_equal(
    meta_type(form_trans(list(p_fin, q_fin), `+`, method = "bruteforce")), "fin"
  )
  expect_equal(
    meta_type(form_trans(list(p_infin, q_fin), `+`, method = "bruteforce")),
    "infin"
  )

  # If `trans` produces logical output, type should be "fin"
  expect_equal(
    meta_type(form_trans(list(p_infin, q_fin), `>=`, method = "random")),
    "fin"
  )
  expect_equal(
    meta_type(form_trans(list(p_infin, q_fin), `>=`, method = "bruteforce")),
    "fin"
  )
})

test_that("form_trans throws error if `trans` produces bad output",  {
  bad_trans <- function(x) {rep("a", length(x))}
  expect_error(
    form_trans(list(p_fin), bad_trans, method = "random"),
    "transformation.*numeric.*logical"
  )
  expect_error(
    form_trans(list(p_fin), bad_trans, method = "bruteforce"),
    "transformation.*numeric.*logical"
  )
})

test_that("form_trans uses `...` as `trans` arguments",  {
  adder <- function(x, y) {x + y}

  output_random <- form_trans(list(p_fin), adder, y = 100, method = "random")
  expect_true(meta_support(p_fin)[1] == meta_support(output_random)[1] - 100)

  output_brute <- form_trans(list(p_fin), adder, y = 100, method = "bruteforce")
  expect_true(meta_support(p_fin)[1] == meta_support(output_brute)[1] - 100)
})

test_that("form_trans uses `n_sample` argument",  {
  # It is used with "random" method
  output_random <- form_trans(list(p_fin), sq, method = "random", n_sample = 1)
  expect_equal(nrow(meta_x_tbl(output_random)), 1)

  # It is not used with "bruteforce" method
  output_brute <- form_trans(
    list(p_fin), sq, method = "bruteforce", n_sample = 1
  )
  expect_equal(nrow(meta_x_tbl(output_brute)), nrow(meta_x_tbl(p_fin)))
})

test_that("form_trans uses `args_new` as arguments for `new_*()`",  {
  output <- form_trans(
    list(p_infin), sq, method = "random", args_new = list(n = 3)
  )
  expect_equal(nrow(meta_x_tbl(output)), 3)

  # Currently there is no effect in using `args_new` in case `method =
  # "bruteforce"` because in computes data frame input for `new_*()` functions,
  # in which case extra arguments are currently not used. However, `args_new`
  # are passed to `new_*()` in the code.
  expect_equal(
    form_trans(list(p_fin), sq, method = "bruteforce", args_new = list(n = 3)),
    form_trans(list(p_fin), sq, method = "bruteforce")
  )

  # Supplying `x` or `type` doesn't affect the output
  expect_equal(
    form_trans(
      list(p_infin), sq, method = "random", args_new = list(x = 1, type = "fin")
    ),
    form_trans(list(p_infin), sq, method = "random")
  )
  expect_equal(
    form_trans(
      list(p_fin), sq, method = "bruteforce",
      args_new = list(x = 1, type = "infin")
    ),
    form_trans(list(p_fin), sq, method = "bruteforce")
  )
})

test_that("form_trans validates input", {
  expect_error(form_trans(p_fin, sq), "`f_list`.*list")
  expect_error(form_trans(list(), sq), "`f_list`.*empty")
  expect_error(form_trans(list("a"), sq), "`f_list`.*pdqr-function.*number")
  expect_error(form_trans(list(1), sq), "`f_list`.*one.*pdqr-function")
  expect_error(form_trans(list(p_fin), 1), "`trans`.*function")
  expect_error(form_trans(list(p_fin), sq, method = 1), "`method`.*string")
  expect_error(form_trans(list(p_fin), sq, method = "a"), "`method`.*one of")
  expect_error(
    form_trans(list(p_fin), sq, n_sample = "a"), "`n_sample`.*single number"
  )
  expect_error(
    form_trans(list(p_fin), sq, n_sample = 1:2), "`n_sample`.*single number"
  )
  expect_error(
    form_trans(list(p_fin), sq, args_new = "a"), "`args_new`.*list"
  )
})


# form_trans_self ---------------------------------------------------------
# This is a thin wrapper for `form_trans()` so main tests are done in
# `form_trans()`
test_that("form_trans_self works",  {
  expect_equal(
    form_trans_self(p_fin, sq),
    form_trans(list(p_fin), sq, method = "bruteforce")
  )

  # Default method should be "random"
  cur_fin <- new_d(data.frame(x = c(-2, 0, 1), prob = c(0.1, 0.7, 0.2)), "fin")
  output <- form_trans_self(cur_fin, `-`)
  output_x_tbl <- meta_x_tbl(output)
  expect_equal(output_x_tbl[["x"]], c(-1, 0, 2))
  expect_true(max(abs(output_x_tbl[["prob"]] - c(0.2, 0.7, 0.1))) < 2e-2)
})


# trans_random ------------------------------------------------------------
# Tested in `form_trans()`


# trans_bruteforce --------------------------------------------------------
# Tested in `form_trans()`


# list_grid ---------------------------------------------------------------
# Tested in `form_trans()`


# assert_trans_output -----------------------------------------------------
# Tested in `form_trans()`
