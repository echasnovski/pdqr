context("test-utils-form")


# new_pdqr_by_class -------------------------------------------------------
test_that("new_pdqr_by_class works",  {
  expect_equal_meta(new_pdqr_by_class("p"), new_p)
  expect_equal_meta(new_pdqr_by_class("d"), new_d)
  expect_equal_meta(new_pdqr_by_class("q"), new_q)
  expect_equal_meta(new_pdqr_by_class("r"), new_r)

  expect_error(new_pdqr_by_class("aaa"), "class")
})


# new_pdqr_by_ref ---------------------------------------------------------
test_that("new_pdqr_by_ref works", {
  expect_equal_meta(new_pdqr_by_ref(p_dis), new_p)
  expect_equal_meta(new_pdqr_by_ref(d_con), new_d)
  expect_equal_meta(new_pdqr_by_ref(q_dis), new_q)
  expect_equal_meta(new_pdqr_by_ref(r_con), new_r)

  expect_error(new_pdqr_by_ref(function(x) {
    x
  }), "class")
})


# as_pdqr_by_class --------------------------------------------------------
test_that("as_pdqr_by_class works",  {
  expect_equal_meta(as_pdqr_by_class("p"), as_p)
  expect_equal_meta(as_pdqr_by_class("d"), as_d)
  expect_equal_meta(as_pdqr_by_class("q"), as_q)
  expect_equal_meta(as_pdqr_by_class("r"), as_r)

  expect_error(as_pdqr_by_class("aaa"), "class")
})


# as_pdqr_by_ref ----------------------------------------------------------
test_that("as_pdqr_by_ref works", {
  expect_equal_meta(as_pdqr_by_ref(p_dis), as_p)
  expect_equal_meta(as_pdqr_by_ref(d_con), as_d)
  expect_equal_meta(as_pdqr_by_ref(q_dis), as_q)
  expect_equal_meta(as_pdqr_by_ref(r_con), as_r)

  expect_error(as_pdqr_by_ref(function(x) {
    x
  }), "class")
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
  expect_silent(assert_f_list(list(d_dis)))
  expect_silent(assert_f_list(list(1, p_con), allow_numbers = TRUE))

  expect_error(assert_f_list(list()), "empty")

  input <- list("a")
  expect_error(assert_f_list(input), "`input`.*pdqr-function")
  expect_error(assert_f_list(input, f_list_name = "AAA"), "AAA")
  expect_error(
    assert_f_list(input, allow_numbers = TRUE),
    "`input`.*pdqr-function.*number"
  )

  expect_error(
    assert_f_list(list(1, 2), allow_numbers = TRUE),
    "at least one pdqr-function"
  )
})

test_that("assert_f_list respects global options", {
  op <- options(pdqr.assert_args = FALSE)
  on.exit(options(op))
  expect_silent(assert_f_list("a"))
})


# compute_f_list_meta -----------------------------------------------------
test_that("compute_f_list_meta works",  {
  # Type is "discrete" only if all pdqr-functions are "discrete"
  expect_equal(
    compute_f_list_meta(list(p_dis, p_dis, d_dis)),
    list(type = "discrete", class = "p")
  )
  expect_equal(
    compute_f_list_meta(list(1, p_dis, 1)),
    list(type = "discrete", class = "p")
  )
  expect_equal(
    compute_f_list_meta(list(p_dis, p_dis, d_con)),
    list(type = "continuous", class = "p")
  )

  # Class is based on the first pdqr-function in the list
  expect_equal(
    compute_f_list_meta(list(p_dis, 1)), list(type = "discrete", class = "p")
  )
  expect_equal(
    compute_f_list_meta(list(1, d_con)), list(type = "continuous", class = "d")
  )
  expect_equal(
    compute_f_list_meta(list(q_con, d_con)),
    list(type = "continuous", class = "q")
  )
})


# intersection_x ----------------------------------------------------------
test_that("intersection_x works", {
  d_1 <- new_d(data.frame(x = c(1, 3, 4), y = c(1, 1, 1)), "continuous")
  d_2 <- new_d(data.frame(x = c(-1, 1, 2, 4, 5), prob = 1:5), "discrete")
  d_3 <- new_d(data.frame(x = c(5, 6), y = c(1, 1)), "continuous")

  expect_equal(intersection_x(d_1, d_2), c(1, 2, 3, 4))
  expect_equal(intersection_x(d_1, d_3), numeric(0))
  expect_equal(intersection_x(d_2, d_3), 5)
})


# union_x -----------------------------------------------------------------
test_that("union_x works", {
  d_1 <- new_d(data.frame(x = c(1, 3, 4), y = c(1, 1, 1)), "continuous")
  d_2 <- new_d(data.frame(x = c(-1, 1, 2, 4, 5), prob = 1:5), "discrete")
  d_3 <- new_d(data.frame(x = c(5, 6), y = c(1, 1)), "continuous")

  expect_equal(union_x(d_1, d_2), c(-1, 1, 2, 3, 4, 5))
  expect_equal(union_x(d_1, d_3), c(1, 3, 4, 5, 6))
  expect_equal(union_x(d_2, d_3), c(-1, 1, 2, 4, 5, 6))
})


# intersection_support ----------------------------------------------------
test_that("intersection_support works", {
  cur_d <- new_d(c(1, 10), "discrete")
  expect_equal(
    intersection_support(cur_d, new_d(c(6, 11), "discrete")), c(6, 10)
  )
  expect_equal(
    intersection_support(cur_d, new_d(c(-1, 5), "discrete")), c(1, 5)
  )
  expect_equal(intersection_support(cur_d, new_d(c(2, 7), "discrete")), c(2, 7))
  expect_equal(intersection_support(cur_d, new_d(c(1, 9), "discrete")), c(1, 9))
  expect_equal(
    intersection_support(cur_d, new_d(c(11, 12), "discrete")), numeric(0)
  )
  expect_equal(
    intersection_support(cur_d, new_d(c(-12, -11), "discrete")), numeric(0)
  )
})


# union_support -----------------------------------------------------------
test_that("union_support works", {
  cur_d <- new_d(c(1, 10), "discrete")
  expect_equal(union_support(cur_d, new_d(c(6, 11), "discrete")), c(1, 11))
  expect_equal(union_support(cur_d, new_d(c(-1, 5), "discrete")), c(-1, 10))
  expect_equal(union_support(cur_d, new_d(c(2, 7), "discrete")), c(1, 10))
  expect_equal(union_support(cur_d, new_d(c(1, 9), "discrete")), c(1, 10))
  expect_equal(union_support(cur_d, new_d(c(11, 12), "discrete")), c(1, 12))
  expect_equal(union_support(cur_d, new_d(c(-12, -11), "discrete")), c(-12, 10))
})
