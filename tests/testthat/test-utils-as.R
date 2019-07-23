context("test-utils-as")


# as_honored_distr --------------------------------------------------------
test_that("as_honored_distr works", {
  out <- as_honored_distr("p", "punif", stats::punif, c(0, 1), n_grid = 10000)
  expect_distr_fun(out, "p", "continuous")
  expect_equal(meta_support(out), c(0, 1))
})

test_that("as_honored_distr warns about mistyped function type", {
  expect_warning(
    as_honored_distr("d", "punif", stats::punif, c(0, 1), n_grid = 10000),
    "p-function.*`punif`.*`as_d\\(\\)`.*d-function"
  )
})


# honored_distr_info ------------------------------------------------------
# Tested in `as_honored_distr()` and `as_*.default()`


# honored_distr_supp ------------------------------------------------------
test_that("honored_distr_supp supports all honored distributions", {
  expect_honored <- function(distr, q_fun, supp_quans, ...) {
    expect_equal(
      honored_distr_supp(distr, q_fun, ...),
      q_fun(supp_quans, ...)
    )
  }

  p <- 1e-6
  big_p <- 1e3 * p

  # "discrete"
  expect_honored("binom", qbinom, c(0, 1), size = 10, prob = 0.1)
  expect_honored("geom", qgeom, c(0, 1-p), prob = 0.1)
  expect_honored("hyper", qhyper, c(0, 1), m = 3, n = 3, k = 2)
  expect_honored("nbinom", qnbinom, c(p, 1 - p), size = 10, prob = 0.1)
  expect_honored("pois", qpois, c(p, 1 - p), lambda = 5)

  # "continuous"
  expect_honored("beta", qbeta, c(p, 1 - p), shape1 = 20, shape2 = 20)
  expect_honored("cauchy", qcauchy, c(big_p, 1 - big_p), location = 100)
  expect_honored("chisq", qchisq, c(p, 1 - p), df = 100)
  expect_honored("exp", qexp, c(0, 1 - p), rate = 0.01)
  expect_honored("f", qf, c(p, 1 - p), df1 = 100, df2 = 100)
  expect_honored("gamma", qgamma, c(p, 1 - p), shape = 10)
  expect_honored("lnorm", qlnorm, c(p, 1 - p), meanlog = 2, sdlog = 0.1)
  expect_honored("norm", qnorm, c(p, 1 - p), mean = 100, sd = 0.1)
  expect_honored("t", qt, c(p, 1 - p), df = 100)
  expect_honored("unif", qunif, c(0, 1), min = -10, max = 100)
  expect_honored("weibull", qweibull, c(p, 1 - p), shape = 10)
})

test_that("honored_distr_supp stretches to total support", {
  expect_identical(
    honored_distr_supp("beta", qbeta, shape1 = 1, shape2 = 1), c(0, 1)
  )
  expect_identical(honored_distr_supp("chisq", qchisq, df = 1)[1], 0)
  expect_identical(honored_distr_supp("gamma", qgamma, shape = 0.5)[1], 0)
})

test_that("honored_distr_supp stops on not honored properly distribution", {
  expect_error(honored_distr_supp("hello", qunif), '"hello".*not honored')
})


# stretch_to_total_supp ---------------------------------------------------
# Tested in `honored_distr_supp()`


# y_from_p_grid -----------------------------------------------------------
# Tested in `as_p.default()`


# assert_as_def_args ------------------------------------------------------
# Tested in `as_*.default()` functions


# assert_tot_prob ---------------------------------------------------------
test_that("assert_tot_prob works", {
  expect_error(assert_tot_prob(0), "probability.*positive")
  expect_silent(assert_tot_prob(0.1))
})


# remove_zero_edge_y ------------------------------------------------------
test_that("remove_zero_edge_y works", {
  input_x_tbl <- data.frame(x = 1:8, y = c(0, 0, 1, 0, 0, 1, 0, 0))
  n <- nrow(input_x_tbl)
  output_ref <- input_x_tbl[2:7, ]

  expect_equal(remove_zero_edge_y(input_x_tbl), output_ref)
  expect_equal(remove_zero_edge_y(input_x_tbl[-1, ]), output_ref)
  expect_equal(remove_zero_edge_y(input_x_tbl[-n, ]), output_ref)
  expect_equal(remove_zero_edge_y(output_ref), output_ref)
})


# is_zero_tail ------------------------------------------------------------
test_that("is_zero_tail works", {
  # Type "discrete"
  expect_equal(is_zero_tail(c(1, 0, 0, 0, 1), "discrete"), rep(FALSE, 5))
  expect_equal(
    is_zero_tail(c(0, 0, 0, 0, 1), "discrete"), c(rep(TRUE, 4), FALSE)
  )
  expect_equal(
    is_zero_tail(c(1, 0, 0, 0, 0), "discrete"), c(FALSE, rep(TRUE, 4))
  )
  expect_equal(
    is_zero_tail(c(0, 0, 1, 0, 0), "discrete"), c(TRUE, TRUE, FALSE, TRUE, TRUE)
  )

  # Type "continuous"
  expect_equal(is_zero_tail(c(1, 0, 0, 0, 1), "continuous"), rep(FALSE, 5))
  expect_equal(
    is_zero_tail(c(0, 0, 0, 0, 1), "continuous"), c(rep(TRUE, 3), FALSE, FALSE)
  )
  expect_equal(
    is_zero_tail(c(1, 0, 0, 0, 0), "continuous"), c(FALSE, FALSE, rep(TRUE, 3))
  )
  expect_equal(
    is_zero_tail(c(0, 0, 1, 0, 0), "continuous"),
    c(TRUE, FALSE, FALSE, FALSE, TRUE)
  )
})


# format_support ----------------------------------------------------------
test_that("format_support works", {
  expect_equal(format_support(NULL), c(NA_real_, NA_real_))
  expect_equal(format_support(c(1, NA)), c(1, NA))
})
