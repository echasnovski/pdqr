context("test-form-other")

set.seed(9999)


# Input data --------------------------------------------------------------
cur_f_list <- list(
  new_d(data.frame(x = 0:1, prob = c(0.4, 0.6)), "discrete"),
  new_q(data.frame(x = 1:2, prob = c(0.5, 0.5)), "discrete"),
  new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
  new_p(data.frame(x = c(0.5, 0.75), y = c(4, 4)), "continuous")
)

bad_x_tbl_big <- data.frame(
  x = sort(runif(100)), prob = runif(100), y = runif(100)
)


# Custom functions --------------------------------------------------------
# Measure of "smoothness" of pdqr-function
median_abs_deriv <- function(f) {
  x_tbl <- meta_x_tbl(f)

  median(abs(diff(x_tbl[[2]])) / diff(x_tbl[[1]]))
}


# form_mix ----------------------------------------------------------------
test_that("form_mix works when input is all 'discrete'", {
  expect_ref_x_tbl(
    # By default equal-weight mix is done
    form_mix(cur_f_list[1:2]),
    data.frame(x = 0:2, prob = c(0.5*0.4, 0.5*0.6+0.5*0.5, 0.5*0.5))
  )
  expect_ref_x_tbl(
    form_mix(cur_f_list[1:2], weights = c(0.25, 0.75)),
    data.frame(x = 0:2, prob = c(0.25*0.4, 0.25*0.6+0.75*0.5, 0.75*0.5))
  )
})

test_that("form_mix works when input is all 'continuous'", {
  h <- 1e-8

  expect_ref_x_tbl(
    # By default equal-weight mix is done
    form_mix(cur_f_list[3:4]),
    data.frame(
      x = c(  0, 0.5-h, 0.5, 0.75, 0.75+h,   1),
      # Extra values are subtracted due to adding small grounding "triangles" on
      # edges of mixed distributions which affect normalization during call to
      # `new_*()` (result of stacking hasn't total integral of 1, but ~ 1+2*h).
      y = c(0.5,   0.5, 2.5,  2.5,    0.5, 0.5) - h*c(1, 1, 5, 5, 1, 1)
    )
  )
  expect_ref_x_tbl(
    form_mix(cur_f_list[3:4], weights = c(0.25, 0.75)),
    data.frame(
      x = c(   0, 0.5-h,  0.5, 0.75, 0.75+h,   1),
      # Extra values have the same meaning (total integral is ~ 1+3*h).
      y = c(0.25,  0.25, 3.25, 3.25,   0.25, 0.25) -
        h*c(0.75,  0.75, 9.75, 9.75,   0.75, 0.75)
    )
  )
})

test_that("form_mix works when input has both pdqr types", {
  h <- 1e-8

  expect_ref_x_tbl(
    # By default equal-weight mix is done
    form_mix(cur_f_list[c(1, 4)]),
    data.frame(
      x = c(-h,       0, h, 0.5-h, 0.5, 0.75, 0.75+h, 1-h,       1, 1+h),
      # Extra values are subtracted due to adding small grounding "triangles" on
      # edges of mixed distributions which affect normalization during call to
      # `new_*()` (result of stacking hasn't total integral of 1, but ~ 1+2*h).
      y = c( 0,   0.2/h, 0,     0,   2,    2,      0,   0,   0.3/h,   0) -
          c( 0,     0.4, 0,     0, 4*h,  4*h,      0,   0,     0.6,   0)
    )
  )
  expect_ref_x_tbl(
    form_mix(cur_f_list[c(1, 4)], weights = c(0.25, 0.75)),
    # Extra values have the same meaning (total integral is ~ 1+3*h).
    data.frame(
      x = c(-h,        0, h, 0.5-h, 0.5, 0.75, 0.75+h, 1-h,      1, 1+h),
      y = c( 0,    0.1/h, 0,     0,   3,    3,      0,   0, 0.15/h,   0) -
          c( 0,      0.3, 0,     0, 9*h,  9*h,      0,   0,   0.45,   0)
    )
  )
})

test_that("form_mix normalizes `weights` argument", {
  n <- length(cur_f_list)
  expect_equal(
    form_mix(cur_f_list, weights = rep(1, n) / n),
    form_mix(cur_f_list, weights = rep(1, n))
  )
})

test_that("form_mix handles length-one list", {
  expect_equal(form_mix(cur_f_list[1]), cur_f_list[[1]])
  expect_equal(form_mix(cur_f_list[3]), cur_f_list[[3]])
})

test_that("form_mix returns pdqr-function of correct class", {
  expect_is(form_mix(cur_f_list[3:4]), meta_class(cur_f_list[[1]]))
  expect_is(form_mix(cur_f_list[2:1]), meta_class(cur_f_list[[2]]))
  expect_is(form_mix(cur_f_list[4:1]), meta_class(cur_f_list[[4]]))
})

test_that("form_mix strictly treats supports as closed intervals", {
  # This is mainly a regression test to demonstrate current position about the
  # following matter: what should be a value of density mixture at point which
  # is a support edge for multiple pdqr-functions? For example, what is a
  # density value of equiweighted mixture of U(0; 1) and U(1; 2) at point 1?
  # Current position is as follows: as distributions are defined on **closed**
  # support intervals, value should be a weighted sum of separate density
  # values. The problem with this approach is that it in some cases leads to
  # discontinuity of "true" density: mixture of previous example is equal to 0.5
  # on [0; 1) and (1; 2], but at point 1 density is 1 (0.5*1 + 0.5*1).
  # In 'pdqr' implementation, this (consistently with other cases) means
  # presence of dirac-like "spikes" (see test).

  d_unif_1 <- new_d(data.frame(x = c(0, 1), y = c(1, 1)), "continuous")
  d_unif_2 <- new_d(data.frame(x = c(1, 2), y = c(1, 1)), "continuous")

  expect_ref_x_tbl(
    form_mix(list(d_unif_1, d_unif_2)),
    data.frame(
      x = c(  0, 1-1e-8, 1, 1+1e-8,   2),
      y = c(0.5,    0.5, 1,    0.5, 0.5)
    )
  )
})

test_that("form_mix validates input", {
  expect_error(form_mix(), "`f_list`.*missing.*list of")
  expect_error(form_mix("a"), "`f_list`.*list")
  expect_error(form_mix(list()), "`f_list`.*empty")
  expect_error(form_mix(list(1)), "`f_list`.*pdqr-functions")
  expect_error(form_mix(list(function(x) {x})), "`f_list`.*pdqr-functions")
  expect_error(form_mix(list(cur_f_list[[1]], 1)), "`f_list`.*pdqr-functions")

  expect_error(form_mix(cur_f_list[1:2], weights = "a"), "`weights`.*numeric")
  expect_error(form_mix(cur_f_list[1:2], weights = 1:3), "`weights`.*length")
  expect_error(
    form_mix(cur_f_list[1:2], weights = c(-1, 2)), "`weights`.*negative"
  )
  expect_error(
    form_mix(cur_f_list[1:2], weights = c(0, 0)), "`weights`.*positive sum"
  )
})


# impute_weights ----------------------------------------------------------
# Tested in `form_mix()`


# form_smooth -------------------------------------------------------------
test_that("form_smooth works with 'discrete' functions", {
  bad_dis <- new_d(bad_x_tbl_big[, c("x", "prob")], "discrete")

  output <- form_smooth(bad_dis)
  expect_is(output, "d")
  expect_equal(meta_x_tbl(output)[["x"]], meta_x_tbl(bad_dis)[["x"]])
  expect_true(median_abs_deriv(output) < median_abs_deriv(bad_dis))

  # Handling of one-point edge case
  d_one_point <- new_d(0.37, "discrete")
  expect_equal(form_smooth(d_one_point), d_one_point)
})

test_that("form_smooth works with 'continuous' functions", {
  bad_con <- new_d(bad_x_tbl_big[, c("x", "y")], "continuous")

  output <- form_smooth(bad_con)
  expect_is(output, "d")
  expect_equal(meta_x_tbl(output)[["x"]], meta_x_tbl(bad_con)[["x"]])
  expect_true(median_abs_deriv(output) < median_abs_deriv(bad_con))
})

test_that("form_smooth uses `n_sample` argument", {
  bad_dis <- new_d(data.frame(x = 0:2, prob = c(0.05, 0.9, 0.05)), "discrete")

  smooth_d_1 <- form_smooth(bad_dis, n_sample = 1e4)
  smooth_d_2 <- form_smooth(bad_dis, n_sample = 2)

  # Usage of extremely small `n_sample` should result here into "less spiked"
  # `density()` output
  expect_true(smooth_d_1(1) > smooth_d_2(1))
})

test_that("form_smooth uses `args_new` as arguments for `new_*()`", {
  bad_con <- new_d(bad_x_tbl_big[, c("x", "y")], "continuous")

  # Using more wide bandwidth results into smoother output
  set.seed(333)
  output_1 <- form_smooth(bad_con)
  output_2 <- form_smooth(bad_con, args_new = list(adjust = 10))

  expect_true(median_abs_deriv(output_2) < median_abs_deriv(output_1))

  # Supplied `x` and `type` in `args_new` is ignored
  set.seed(333)
  output_3 <- form_smooth(bad_con, args_new = list(x = 1:10, type = "discrete"))

  expect_equal_x_tbl(output_1, output_3)
})

d_dis <- new_d(data.frame(x = 0:1, prob = 0:1), "discrete")

test_that("form_smooth validates input", {
  expect_error(form_smooth("a"), "`f`.*not pdqr-function")
  expect_error(form_smooth(d_dis, n_sample = "a"), "`n_sample`.*single number")
  expect_error(form_smooth(d_dis, n_sample = 1), "`n_sample`.*more than 1")
  expect_error(form_smooth(d_dis, args_new = "a"), "`args_new`.*list")
})


# form_estimate -----------------------------------------------------------
test_that("form_estimate works", {
  # From Central limit theorem mean estimate of n points should have mean =
  # `mean_input` and sd = `sd_input / sqrt(n)` (here `*_input` are moments of
  # input distribution and `sqrt(n)` - square root of estimator's sample size).

  # Type "discrete"
  cur_d <- new_d(data.frame(x = 0:2, prob = c(0.3, 0.4, 0.3)), "discrete")
  mean_cur_d <- 0.4*1 + 0.3*2
  sd_cur_d <- sqrt((0.4*1^2 + 0.3*2^2) - (mean_cur_d)^2)

  dis_mean_est <- form_estimate(cur_d, mean, sample_size = 16, n_sample = 1000)
  expect_is(dis_mean_est, "d")
  expect_true(meta_type(dis_mean_est) == "discrete")

    # Testing Central limit theorem
  expect_true(abs(summ_mean(dis_mean_est) - mean_cur_d) <= 2e-2)
  expect_true(abs(summ_sd(dis_mean_est) - sd_cur_d / 4) <= 1e-3)

  # Type "continuous"
  d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")
  mean_d_unif <- 0.5
  sd_d_unif <- 1 / sqrt(12)

  con_mean_est <- form_estimate(d_unif, mean, 16, n_sample = 1000)
  expect_is(con_mean_est, "d")
  expect_true(meta_type(con_mean_est) == "continuous")

    # Testing Central limit theorem
  expect_true(abs(summ_mean(con_mean_est) - mean_d_unif) <= 1e-2)
  expect_true(abs(summ_sd(con_mean_est) - sd_d_unif / 4) <= 4e-3)
})

test_that("form_estimate works with logical output of `stat`", {
  d_unif <- new_d(data.frame(x = c(-1, 1), y = c(1, 1)/2), "continuous")

  all_positive <- function(x) {all(x > 0)}
  estim <- form_estimate(d_unif, all_positive, sample_size = 3, n_sample = 1000)
  expect_true(is_boolean_pdqr_fun(estim))
  expect_equal(summ_prob_true(estim), 0.5^3, tolerance = 1e-2)

  # Handles `NA`s in logical output
  na_lgl_stat <- function(x) {
    if (any(x < 0.5)) {
      NA
    } else {
      all(x > 0)
    }
  }
  na_estim <- form_estimate(
    d_unif, na_lgl_stat, sample_size = 3, n_sample = 1000
  )
  expect_true(is_boolean_pdqr_fun(na_estim))
  # Output here is 1 because probability is estimated using those values which
  # are not `NA`. Not `NA` is returned only if `all(x >= 0.5)`. The exact
  # logical value is then computed as `all(x > 0)` which is now always true.
  expect_equal(summ_prob_true(na_estim), 1)
})

test_that("form_estimate uses `...` as arguments to `stat`", {
  dummy_stat <- function(x, y) {y}

  est <- form_estimate(d_dis, dummy_stat, 10, y = 10)
  expect_ref_x_tbl(est, data.frame(x = 10, prob = 1))
})

test_that("form_estimate uses `n_sample` argument", {
  cur_d <- new_d(data.frame(x = 0:2, prob = c(0.3, 0.4, 0.3)), "discrete")

  mean_est <- form_estimate(cur_d, mean, 10, n_sample = 2)
  expect_true(nrow(meta_x_tbl(mean_est)) <= 2)
})

test_that("form_estimate uses `args_new` as arguments to `new_*()`", {
  d_unif <- new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous")

  con_mean_est <- form_estimate(
    d_unif, mean, 16, n_sample = 100, args_new = list(n = 100)
  )

  expect_true(nrow(meta_x_tbl(con_mean_est)) == 100)
})

test_that("form_estimate allows `type` in `args_new`", {
  cur_d <- new_d(data.frame(x = 0:2, prob = c(0.3, 0.4, 0.3)), "discrete")

  mean_est <- form_estimate(
    cur_d, stat = mean, sample_size = 10,
    n_sample = 10, args_new = list(type = "continuous")
  )
  expect_equal(meta_type(mean_est), "continuous")
})

test_that("form_estimate checks that `stat` returns single num or lgl", {
  expect_error(
    form_estimate(d_dis, function(x) {"a"}, 10),
    "output.*`stat`.*single.*numeric.*logical"
  )
  expect_error(
    form_estimate(d_dis, function(x) {1:3}, 10),
    "output.*`stat`.*single.*numeric.*logical"
  )
})

test_that("form_estimate validates input", {
  expect_error(form_estimate("a", mean, 10), "`f`.*not pdqr-function")
  expect_error(
    form_estimate(d_dis, sample_size = 10),
    "`stat`.*missing.*statistic function"
  )
  expect_error(form_estimate(d_dis, "a", 10), "`stat`.*function")
  expect_error(
    form_estimate(d_dis, mean), "`sample_size`.*missing.*size of sample"
  )
  expect_error(form_estimate(d_dis, mean, "a"), "`sample_size`.*single.*number")
  expect_error(form_estimate(d_dis, mean, 0), "`sample_size`.*positive")
  expect_error(
    form_estimate(d_dis, mean, 10, n_sample = "a"), "`n_sample`.*single.*number"
  )
  expect_error(
    form_estimate(d_dis, mean, 10, n_sample = 0), "`n_sample`.*positive"
  )
  expect_error(
    form_estimate(d_dis, mean, 10, args_new = "a"), "`args_new`.*list"
  )
})


# form_recenter -----------------------------------------------------------
test_that("form_recenter works", {
  expect_recenter_works <- function(f, to) {
    for (meth in c("mean", "median", "mode")) {
      out <- form_recenter(f, to = to, method = meth)

      expect_equal(summ_center(out, method = meth), to)
    }
  }

  # Type "discrete"
  cur_d_dis <- new_d(
    data.frame(x = c(1, 2, 100), prob = c(0.4, 0.25, 0.35)), "discrete"
  )

  expect_recenter_works(cur_d_dis, to = -10)

  # Type "continuous"
  cur_d_con <- new_d(data.frame(x = c(1, 2, 100), y = c(0, 1, 0)), "continuous")

  expect_recenter_works(cur_d_con, to = -10)
})

test_that("form_recenter validates input", {
  expect_error(form_recenter("a", 1), "`f`.*not pdqr-function")
  expect_error(form_recenter(d_dis, "a"), "`to`.*number")
  expect_error(form_recenter(d_dis, 1:2), "`to`.*single")
  expect_error(form_recenter(d_dis, 1.5, method = 1), "`method`.*string")
  expect_error(form_recenter(d_dis, 1.5, method = "a"), "`method`.*one of")
})


# form_respread -----------------------------------------------------------
test_that("form_respread works", {
  expect_respread_works <- function(f, to) {
    for (center_meth in c("mean", "median", "mode")) {
      f_center <- summ_center(f, method = center_meth)

      for (meth in c("sd", "var", "iqr", "mad", "range")) {
        # Respreading to `to`
        out <- form_respread(
          f, to = to, method = meth, center_method = center_meth
        )

        expect_equal(summ_spread(out, method = meth), to)
        expect_equal(summ_center(out, method = center_meth), f_center)

        # Respreading to zero
        out_zero <- form_respread(
          f, to = 0, method = meth, center_method = center_meth
        )
        expect_equal_x_tbl(out_zero, new_d(f_center, type = meta_type(f)))
      }
    }
  }

  # Type "discrete"
  cur_d_dis <- new_d(
    data.frame(x = c(1, 2, 100), prob = c(0.4, 0.25, 0.35)), "discrete"
  )

  expect_respread_works(cur_d_dis, to = 1.5)

  # Type "continuous"
  cur_d_con <- new_d(data.frame(x = c(1, 2, 100), y = c(0, 1, 0)), "continuous")

  expect_respread_works(cur_d_con, to = 1.5)
})

test_that("form_respread validates input", {
  expect_error(form_respread("a", 1), "`f`.*not pdqr-function")
  expect_error(form_respread(d_dis, "a"), "`to`.*number")
  expect_error(form_respread(d_dis, 1:2), "`to`.*single")
  expect_error(form_respread(d_dis, -1), "`to`.*non-negative")
  expect_error(form_respread(d_dis, 1.5, method = 1), "`method`.*string")
  expect_error(form_respread(d_dis, 1.5, method = "a"), "`method`.*one of")
  expect_error(
    form_respread(d_dis, 1.5, center_method = 1), "`center_method`.*string"
  )
  expect_error(
    form_respread(d_dis, 1.5, center_method = "a"), "`center_method`.*one of"
  )
})
