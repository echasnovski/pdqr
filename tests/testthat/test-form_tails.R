context("test-form_tails")


# Custom expectations -----------------------------------------------------
expect_self_x_tbl <- function(pdqr_f) {
  for (method in c("trim", "winsor")) {
    for (dir in c("both", "left", "right")) {
      expect_equal_x_tbl(form_tails(pdqr_f, 0, method, dir), pdqr_f)
    }
  }
}

expect_dirac <- function(pdqr_f, x_vec) {
  max_levels <- c("both" = 0.5, "left" = 1, "right" = 1)

  for (method in c("trim", "winsor")) {
    for (dir in c("both", "left", "right")) {
      expect_equal_x_tbl(
        form_tails(pdqr_f, max_levels[dir], method, dir),
        new_pdqr_by_ref(pdqr_f)(x_vec[dir], meta_type(pdqr_f))
      )
    }
  }
}

expect_error_negative_level <- function(pdqr_f) {
  for (method in c("trim", "winsor")) {
    for (dir in c("both", "left", "right")) {
      expect_error(
        form_tails(pdqr_f, -0.1, method = method, direction = dir),
        "`level`.*negative"
      )
    }
  }
}


# Input data --------------------------------------------------------------
cur_fin <- new_d(data.frame(x = 1:4, prob = (1:4) / 10), "fin")
cur_infin <- new_d(data.frame(x = 0:1, y = c(1, 1)), "infin")


# form_tails --------------------------------------------------------------
test_that("form_tails works with `type='fin'` and `method='trim'`", {
  # `direction = "both"`
  # This should remove first row (not leave it with value 0 at "prob" column)
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.1, "trim", "both"),
    data.frame(x = 2:4, prob = c(0.2, 0.3, 0.3) / 0.8)
  )
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.15, "trim", "both"),
    data.frame(x = 2:4, prob = c(0.15, 0.3, 0.25) / 0.7)
  )

  # `direction = "left"`
  # This should remove first row (not leave it with value 0 at "prob" column)
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.1, "trim", "left"),
    data.frame(x = 2:4, prob = c(0.2, 0.3, 0.4) / 0.9)
  )
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.15, "trim", "left"),
    data.frame(x = 2:4, prob = c(0.15, 0.3, 0.4) / 0.85)
  )
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.8, "trim", "left"),
    data.frame(x = 4, prob = 1)
  )

  # `direction = "right"`
  # This should remove last row (not leave it with value 0 at "prob" column)
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.4, "trim", "right"),
    data.frame(x = 1:3, prob = c(0.1, 0.2, 0.3) / 0.6)
  )
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.45, "trim", "right"),
    data.frame(x = 1:3, prob = c(0.1, 0.2, 0.25) / 0.55)
  )
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.8, "trim", "right"),
    data.frame(x = 1:2, prob = c(0.5, 0.5))
  )
})

test_that("form_tails works with `type='infin'` and `method='trim'`", {
  # `direction = "both"`
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.05, "trim", "both"),
    data.frame(x = c(0.05, 0.95), y = c(1, 1) / 0.9)
  )
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.4, "trim", "both"),
    data.frame(x = c(0.4, 0.6), y = c(1, 1) / 0.2)
  )

  # `direction = "left"`
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.05, "trim", "left"),
    data.frame(x = c(0.05, 1), y = c(1, 1) / 0.95)
  )
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.8, "trim", "left"),
    data.frame(x = c(0.8, 1), y = c(1, 1) / 0.2)
  )

  # `direction = "right"`
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.05, "trim", "right"),
    data.frame(x = c(0, 0.95), y = c(1, 1) / 0.95)
  )
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.8, "trim", "right"),
    data.frame(x = c(0, 0.2), y = c(1, 1) / 0.2)
  )
})

test_that("form_tails works with `type='fin'` and `method='winsor'`", {
  # `direction = "both"`
  # Here first row ISN'T removed (unlike with "trim") because 10% quantile is 1
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.1, "winsor", "both"),
    data.frame(x = 1:4, prob = (1:4) / 10)
  )
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.11, "winsor", "both"),
    data.frame(x = 2:4, prob = c(0.3, 0.3, 0.4))
  )

  # `direction = "left"`
  # Here first row ISN'T removed (unlike with "trim") because 10% quantile is 1
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.1, "winsor", "left"),
    data.frame(x = 1:4, prob = (1:4) / 10)
  )
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.11, "winsor", "left"),
    data.frame(x = 2:4, prob = c(0.3, 0.3, 0.4))
  )

  # `direction = "right"`
  # Here last row IS removed because 60% (100-40) quantile is 3
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.4, "winsor", "right"),
    data.frame(x = 1:3, prob = c(0.1, 0.2, 0.7))
  )
  expect_ref_x_tbl(
    form_tails(cur_fin, 0.41, "winsor", "right"),
    data.frame(x = 1:3, prob = c(0.1, 0.2, 0.7))
  )
})

test_that("form_tails works with `type='infin'` and `method='winsor'`", {
  # `direction = "both"`
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.1, "winsor", "both"),
    data.frame(
      x = c(  0.1, 0.1+1e-8, 0.9-1e-8,   0.9),
      # Here `2e7+1` is used instead of `2e7` due to (seems like) numerical
      # representation issues
      y = c(2e7+1,        1,        1, 2e7+1)
    )
  )

  # `direction = "left"`
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.1, "winsor", "left"),
    data.frame(
      x = c(  0.1, 0.1+1e-8, 1),
      # Here `2e7+1` is used instead of `2e7` due to (seems like) numerical
      # representation issues
      y = c(2e7+1,        1, 1)
    )
  )

  # `direction = "right"`
  expect_ref_x_tbl(
    form_tails(cur_infin, 0.1, "winsor", "right"),
    data.frame(
      x = c(0, 0.9-1e-8,   0.9),
      # Here `2e7+1` is used instead of `2e7` due to (seems like) numerical
      # representation issues
      y = c(1,        1, 2e7+1)
    )
  )
})

test_that("form_tails returns self when `level = 0`", {
  expect_self_x_tbl(cur_fin)
  expect_self_x_tbl(cur_infin)
})

test_that("form_tails returns dirac distribution at maximum level", {
  expect_dirac(cur_fin, c("both" = 3, "left" = 4, "right" = 1))
  expect_dirac(cur_infin, c("both" = 0.5, "left" = 1, "right" = 0))
})

test_that("form_tails asserts bad input",  {
  expect_error(form_tails("a", 0.1), "`f`.*function")
  expect_error(form_tails(function(x) {x}, 0.1), "`f`.*pdqr")
  expect_error(form_tails(cur_fin, "a"), "`level`.*single number")
  expect_error(form_tails(cur_fin, 0.1, method = 1), "`method`.*string")
  expect_error(form_tails(cur_fin, 0.1, method = "a"), "`method`.*one of")
  expect_error(form_tails(cur_fin, 0.1, direction = 1), "`direction`.*string")
  expect_error(form_tails(cur_fin, 0.1, direction = "a"), "`direction`.*one of")

  # Tests for negative `level`
  expect_error_negative_level(cur_fin)

  # Tests for bigger `level` than maximum for particular `direction`
  expect_error(form_tails(cur_fin, 0.6, "trim", "both"), "`level`.*0.5")
  expect_error(form_tails(cur_fin, 0.6, "winsor", "both"), "`level`.*0.5")
  expect_error(form_tails(cur_fin, 1.2, "trim", "left"), "`level`.*1")
  expect_error(form_tails(cur_fin, 1.2, "winsor", "left"), "`level`.*1")
  expect_error(form_tails(cur_fin, 1.2, "trim", "right"), "`level`.*1")
  expect_error(form_tails(cur_fin, 1.2, "winsor", "right"), "`level`.*1")
})


# tails_trim --------------------------------------------------------------
# Tested in `form_tails()`


# tails_winsor ------------------------------------------------------------
# Tested in `form_tails()`


# tails_trim_fin ----------------------------------------------------------
# Tested in `form_tails()`


# tails_trim_infin --------------------------------------------------------
# Tested in `form_tails()`


# assert_form_tails_args --------------------------------------------------
# Tested in `form_tails()`


# trim_all ----------------------------------------------------------------
# Tested in `form_tails()`


# compute_support_after_remove --------------------------------------------
# Tested in `form_tails()`


# decrease_row_prob -------------------------------------------------------
# Tested in `form_tails()`