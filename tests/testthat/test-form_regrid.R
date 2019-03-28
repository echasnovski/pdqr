context("test-form_regrid")


# Input data --------------------------------------------------------------
cur_fin <- new_d(
  data.frame(x = c(1, 1.1, 2, 2.5, 3.4), prob = (5:1) / 15), "fin"
)
cur_infin <- new_d(
  data.frame(x = 0:3, y = c(0, 0.5, 1, 2) / 2.5), "infin"
)


# Custom expectations -----------------------------------------------------
expect_regrid_to_one <- function(f) {
  med <- as_q(f)(0.5)
  new_pdqr <- new_pdqr_by_ref(f)
  f_type <- meta_type(f)

  expect_equal_x_tbl(form_regrid(f, 1, method = "x"), new_pdqr(med, f_type))
  expect_equal_x_tbl(form_regrid(f, 1, method = "q"), new_pdqr(med, f_type))
}

expect_pdqr_commute <- function(f, n_grid, method) {
  expect_equal(
    form_regrid(as_p(f), n_grid, method),
    as_p(form_regrid(f, n_grid, method))
  )
  expect_equal(
    form_regrid(as_d(f), n_grid, method),
    as_d(form_regrid(f, n_grid, method))
  )
  expect_equal(
    form_regrid(as_q(f), n_grid, method),
    as_q(form_regrid(f, n_grid, method))
  )
  expect_equal(
    form_regrid(as_r(f), n_grid, method),
    as_r(form_regrid(f, n_grid, method))
  )
}


# form_regrid -------------------------------------------------------------
test_that("form_regrid downgrids when `type` 'fin'", {
  # Output `x` are edges with probabilities collapsed according to smallest
  # ordinary distance. Probabilities are sum of respective nearest input `x`.
  expect_ref_x_tbl(
    form_regrid(cur_fin, n_grid = 2, method = "x"),
    data.frame(x = c(1, 3.4), prob = c(5+4+3, 2+1) / 15)
  )
  expect_ref_x_tbl(
    form_regrid(cur_fin, n_grid = 2, method = "q"),
    data.frame(x = c(1, 3.4), prob = c(5+4+3, 2+1) / 15)
  )

  # Output `x` are from rows **matched** to method grids. Output `prob` come
  # from assigning "non-matched" probabilities to the nearest `x` (according to
  # ordinary distance).
  # For `method = "x"` grid is `seq(x[1], x[n], length.out = n_grid)`.
  # For `method = "q"` grid is quantiles at `seq(0, 1, length.out = n_grid)`.

  # Grid is c(1, 2.2, 3.4).
  expect_ref_x_tbl(
    form_regrid(cur_fin, n_grid = 3, method = "x"),
    data.frame(x = c(1, 2, 3.4), prob = c(5+4, 3+2, 1) / 15)
  )
  # Grid is c(1, 1.1, 3.4).
  expect_ref_x_tbl(
    form_regrid(cur_fin, n_grid = 3, method = "q"),
    data.frame(x = c(1, 1.1, 3.4), prob = c(5, 4+3, 2+1) / 15)
  )
  # Grid is c(1, 1.8, 2.6, 3.4).
  expect_ref_x_tbl(
    form_regrid(cur_fin, n_grid = 4, method = "x"),
    data.frame(x = c(1, 2, 2.5, 3.4), prob = c(5+4, 3, 2, 1) / 15)
  )
  # Grid is c(1, 1, 2, 3.4). Note that output `x` is computed by
  # **matching** needed amount of input `x` elements to the c(1, 2, 3.4).
  expect_ref_x_tbl(
    form_regrid(cur_fin, n_grid = 4, method = "q"),
    data.frame(x = c(1, 1.1, 2, 3.4), prob = c(5, 4, 3+2, 1) / 15)
  )
})

test_that("form_regrid returns self when upgridding 'fin' function", {
  expect_equal(form_regrid(cur_fin, 10, method = "x"), cur_fin)
  expect_equal(form_regrid(cur_fin, 10, method = "q"), cur_fin)
})

test_that("form_regrid downgrids when `type` 'infin'", {
  # Output `x` are edges of input "x_tbl" in case `n_grid = 2`
  expect_ref_x_tbl(
    form_regrid(cur_infin, n_grid = 2, method = "x"),
    data.frame(x = c(0, 3), y = c(0, 2/3))
  )
  expect_ref_x_tbl(
    form_regrid(cur_infin, n_grid = 2, method = "q"),
    data.frame(x = c(0, 3), y = c(0, 2/3))
  )

  # Output `x` are from rows **matched** to method grids. Other rows are dropped
  # and `y` is renormalized.
  # For `method = "x"` grid is `seq(x[1], x[n], length.out = n_grid)`.
  # For `method = "q"` grid is quantiles at `seq(0, 1, length.out = n_grid)`.

  # Grid is c(0, 1.5, 3).
  expect_ref_x_tbl(
    form_regrid(cur_infin, n_grid = 3, method = "x"),
    data.frame(
      x = c(0, 1, 3),
      y = c(0, 0.5, 2) / trapez_integral(c(0, 1, 3), c(0, 0.5, 2))
    )
  )
  # Grid is c(0, ~2.224745, 3).
  expect_ref_x_tbl(
    form_regrid(cur_infin, n_grid = 3, method = "q"),
    data.frame(
      x = c(0, 2, 3),
      y = c(0, 1, 2) / trapez_integral(c(0, 2, 3), c(0, 1, 2))
    )
  )
})

test_that("form_regrid upgrids when `type` 'infin'", {
  # Output `x` are combination of input rows and new points with `y` values
  # from input density (which are renormalized). `x` values of new points are
  # taken from elements of method grid that are the most distant from input `x`
  # (as a set).
  # For `method = "x"` grid is `seq(x[1], x[n], length.out = n_grid)`.
  # For `method = "q"` grid is quantiles at `seq(0, 1, length.out = n_grid)`.

  # Grid is c(0, 0.6, 1.2, 1.8, 2.4, 3). The most distant to input `x` are
  # elements c(0.6, 2.4).
  expect_ref_x_tbl(
    form_regrid(cur_infin, n_grid = 6, method = "x"),
    data.frame(x = c(0, 0.6, 1, 2, 2.4, 3), y = c(0, 0.3, 0.5, 1, 1.4, 2) / 2.5)
  )
  # Grid is approximately c(0, 1.414214, 2, 2.414214, 2.732051, 3). The most
  # distant to input `x` are elements c(1.414214, 2.414214) (here 0.414214 is
  # approximation of fractional part of sqrt(2)).
  expect_ref_x_tbl(
    form_regrid(cur_infin, n_grid = 6, method = "q"),
    data.frame(
      x = c(0, 1, sqrt(2), 2, 1 + sqrt(2), 3),
      y = c(0, 0.5, 1 / sqrt(2), 1, sqrt(2), 2) / 2.5)
  )
})

test_that("form_regrid errors if `n_grid=2`, zero edges, `type='infin'`", {
  # Output `x` in case of `n_grid = 2` are edges of input "x_tbl". So if they
  # are 0s then there should be an error.
  infin_zero_edges <- new_d(data.frame(x = 0:2, y = c(0, 1, 0)), "infin")
  expect_error(form_regrid(infin_zero_edges, n_grid = 2, method = "x"))
  expect_error(form_regrid(infin_zero_edges, n_grid = 2, method = "q"))
})

test_that("form_regrid works with different pdqr-functions", {
  expect_pdqr_commute(cur_fin, n_grid = 3, method = "x")
  expect_pdqr_commute(cur_infin, n_grid = 10, method = "q")
})

test_that("form_regrid handles difficult cases", {
  difficult_fin <- new_d(
    data.frame(x = c(0.98, 0.99, 1.01, 2.2, 3.1), prob = 1:5 / 15), "fin"
  )

  expect_ref_x_tbl(
    form_regrid(difficult_fin, n_grid = 3, method = "x"),
    # Output `x` shouldn't be squashed near c(0.98, 0.99, 1.01). At least, that
    # is a current reasoning taking into account speed of computations.
    data.frame(x = c(0.98, 2.2, 3.1), prob = c(1+2+3, 4, 5) / 15)
  )
})

test_that("form_regrid returns dirac-like function at median if `n_grid = 1`", {
  expect_regrid_to_one(cur_fin)
  expect_regrid_to_one(cur_infin)
})

test_that("form_regrid returns self when `n_grid` = number of present points", {
  expect_equal(form_regrid(cur_fin, 5, method = "x"), cur_fin)
  expect_equal(form_regrid(cur_fin, 5, method = "q"), cur_fin)
  expect_equal(form_regrid(cur_infin, 4, method = "x"), cur_infin)
  expect_equal(form_regrid(cur_infin, 4, method = "q"), cur_infin)
})

test_that("form_regrid asserts bad input", {
  expect_error(form_regrid("a", 10), "`f`.*function")
  expect_error(form_regrid(function(x) {x}, 10), "`f`.*pdqr")
  expect_error(form_regrid(cur_fin, "a"), "`n_grid`.*single.*number")
  expect_error(form_regrid(cur_fin, 0), "`n_grid`.*positive")
  expect_error(form_regrid(cur_fin, 10, method = 1), "`method`.*string")
  expect_error(form_regrid(cur_fin, 10, method = "a"), "`method`.*one of")
})


# early_regrid ------------------------------------------------------------
# Tested in `form_regrid()`


# compute_grid ------------------------------------------------------------
# Tested in `form_regrid()`


# compute_grid_x ----------------------------------------------------------
# Tested in `form_regrid()`


# compute_grid_q ----------------------------------------------------------
# Tested in `form_regrid()`


# adjust_to_grid ----------------------------------------------------------
# Tested in `form_regrid()`


# adjust_to_grid_fin ------------------------------------------------------
# Tested in `form_regrid()`


# adjust_to_grid_infin ----------------------------------------------------
# Tested in `form_regrid()`