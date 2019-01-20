context("test-utils-form")


# new_pdqr_by_ref ---------------------------------------------------------
test_that("new_pdqr_by_ref works", {
  expect_equal(new_pdqr_by_ref(p_fin), new_p)
  expect_equal(new_pdqr_by_ref(d_infin), new_d)
  expect_equal(new_pdqr_by_ref(q_fin), new_q)
  expect_equal(new_pdqr_by_ref(r_infin), new_r)

  expect_error(new_pdqr_by_ref(function(x) {x}), "class")
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
