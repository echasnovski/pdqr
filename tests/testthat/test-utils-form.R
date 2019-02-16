context("test-utils-form")


# new_pdqr_by_class -------------------------------------------------------
test_that("new_pdqr_by_class works",  {
  expect_equal(new_pdqr_by_class("p"), new_p)
  expect_equal(new_pdqr_by_class("d"), new_d)
  expect_equal(new_pdqr_by_class("q"), new_q)
  expect_equal(new_pdqr_by_class("r"), new_r)

  expect_error(new_pdqr_by_class("aaa"), "class")
})


# new_pdqr_by_ref ---------------------------------------------------------
test_that("new_pdqr_by_ref works", {
  expect_equal(new_pdqr_by_ref(p_fin), new_p)
  expect_equal(new_pdqr_by_ref(d_infin), new_d)
  expect_equal(new_pdqr_by_ref(q_fin), new_q)
  expect_equal(new_pdqr_by_ref(r_infin), new_r)

  expect_error(new_pdqr_by_ref(function(x) {x}), "class")
})


# as_pdqr_by_class --------------------------------------------------------
test_that("as_pdqr_by_class works",  {
  expect_equal(as_pdqr_by_class("p"), as_p)
  expect_equal(as_pdqr_by_class("d"), as_d)
  expect_equal(as_pdqr_by_class("q"), as_q)
  expect_equal(as_pdqr_by_class("r"), as_r)

  expect_error(as_pdqr_by_class("aaa"), "class")
})


# as_pdqr_by_ref ----------------------------------------------------------
test_that("as_pdqr_by_ref works", {
  expect_equal(as_pdqr_by_ref(p_fin), as_p)
  expect_equal(as_pdqr_by_ref(d_infin), as_d)
  expect_equal(as_pdqr_by_ref(q_fin), as_q)
  expect_equal(as_pdqr_by_ref(r_infin), as_r)

  expect_error(as_pdqr_by_ref(function(x) {x}), "class")
})


# get_pdqr_class ----------------------------------------------------------
test_that("get_pdqr_class works", {
  expect_equal(get_pdqr_class(structure("a", class = "p")), "p")
  expect_equal(get_pdqr_class(structure("a", class = c("p", "d"))), "p")
  expect_equal(get_pdqr_class(structure("a", class = "bbb")), NA_character_)
})


# point_dirac -------------------------------------------------------------
test_that("point_dirac works", {
  dirac_fin_p <- point_dirac(10, "fin", "p")
  expect_is(dirac_fin_p, "p")
  expect_equal(
    meta_x_tbl(dirac_fin_p),
    data.frame(x = 10, prob = 1, cumprob = 1)
  )

  dirac_infin_r <- point_dirac(-10, "infin", "r")
  expect_is(dirac_infin_r, "r")
  expect_equal(
    meta_x_tbl(dirac_infin_r),
    data.frame(
      # Here `1e8-8` is used instead of 1e8 due to (probably) numerical
      # representation issues
      x = -10 + 1e-8*c(-1, 0, 1), y = c(0, 1e8-8, 0), cumprob = c(0, 0.5, 1)
    )
  )
})

test_that("point_dirac throws errors", {
  expect_error(point_dirac(0, "a", "p"), "`type`")
  expect_error(point_dirac(0, "fin", "a"), "class")
})
