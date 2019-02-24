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


# boolean_pdqr ------------------------------------------------------------
test_that("boolean_pdqr works", {
  output_1 <- boolean_pdqr(0.61, "p")
  expect_is(output_1, "p")
  expect_ref_x_tbl(output_1, data.frame(x = 0:1, prob = c(0.39, 0.61)))

  output_2 <- boolean_pdqr(0, "d")
  expect_is(output_2, "d")
  expect_ref_x_tbl(output_2, data.frame(x = 0:1, prob = c(1, 0)))
})


# assert_f_list -----------------------------------------------------------
test_that("assert_f_list works",  {
  expect_silent(assert_f_list(list(d_fin)))
  expect_silent(assert_f_list(list(1, p_infin), allow_numbers = TRUE))

  expect_error(assert_f_list(list()), "empty")

  input <- list("a")
  expect_error(assert_f_list(input), "`input`.*pdqr-function")
  expect_error(
    assert_f_list(input, allow_numbers = TRUE),
    "`input`.*pdqr-function.*number"
  )

  expect_error(
    assert_f_list(list(1, 2), allow_numbers = TRUE),
    "at least one pdqr-function"
  )
})


# compute_f_list_meta -----------------------------------------------------
test_that("compute_f_list_meta works",  {
  # Type is "fin" only if all pdqr-functions are "fin"
  expect_equal(
    compute_f_list_meta(list(p_fin, p_fin, d_fin)),
    list(type = "fin", class = "p")
  )
  expect_equal(
    compute_f_list_meta(list(1, p_fin, 1)),
    list(type = "fin", class = "p")
  )
  expect_equal(
    compute_f_list_meta(list(p_fin, p_fin, d_infin)),
    list(type = "infin", class = "p")
  )

  # Class is based on the first pdqr-function in the list
  expect_equal(
    compute_f_list_meta(list(p_fin, 1)), list(type = "fin", class = "p")
  )
  expect_equal(
    compute_f_list_meta(list(1, d_infin)), list(type = "infin", class = "d")
  )
  expect_equal(
    compute_f_list_meta(list(q_infin, d_infin)),
    list(type = "infin", class = "q")
  )
})
