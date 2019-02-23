context("test-form-other")


# Input data --------------------------------------------------------------
cur_f_list <- list(
  new_d(data.frame(x = 0:1, prob = c(0.4, 0.6)), "fin"),
  new_q(data.frame(x = 1:2, prob = c(0.5, 0.5)), "fin"),
  new_d(data.frame(x = 0:1, y = c(1, 1)), "infin"),
  new_p(data.frame(x = c(0.5, 0.75), y = c(4, 4)), "infin")
)


# form_mix ----------------------------------------------------------------
test_that("form_mix works when input is all 'fin'", {
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

test_that("form_mix works when input is all 'infin'", {
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

test_that("form_mix works when input has both 'fin' and 'infin' functions", {
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
  expect_is(form_mix(cur_f_list[3:4]), get_pdqr_class(cur_f_list[[1]]))
  expect_is(form_mix(cur_f_list[2:1]), get_pdqr_class(cur_f_list[[2]]))
  expect_is(form_mix(cur_f_list[4:1]), get_pdqr_class(cur_f_list[[4]]))
})

test_that("form_mix asserts bad input", {
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
