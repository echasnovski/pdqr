context("test-meta")

# Input data --------------------------------------------------------------
x <- 1
obj <- structure(list(y = 2), meta = list(m = 3))


# meta --------------------------------------------------------------------
test_that("meta works", {
  expect_equal(meta(obj), list(m = 3))
  expect_equal(meta(obj, "m"), 3)
})


# add_meta ----------------------------------------------------------------
test_that("add_meta works", {
  input_1 <- x
  output_1 <- structure(x, meta = list(q = 4, w = 5))
  expect_equal(add_meta(input_1, q = 4, w = 5), output_1)

  input_2 <- obj
  output_2 <- obj
  attr(output_2, "meta") <- list(m = 3, n = 6)
  expect_equal(add_meta(input_2, n = 6), output_2)
})

test_that("add_meta orders elements alphabetically by name", {
  input <- obj
  output <- obj
  attr(output, "meta") <- list(e = 7, m = 3, n = 6, 8)
  expect_equal(add_meta(input, 8, n = 6, e = 7), output)
})


# add_meta_cond -----------------------------------------------------------
test_that("add_meta_cond works", {
  input <- x

  output_1 <- structure(input, meta = list(r = 0))
  expect_equal(add_meta_cond(input, TRUE, r = 0), output_1)

  expect_equal(add_meta_cond(input, FALSE, r = 0), input)
  expect_equal(add_meta_cond(input, NA, r = 0), input)
})


# add_common_meta ---------------------------------------------------------
test_that("add_common_meta works", {
  input <- x
  input_sample <- 1:10

  output_1 <- structure(x, meta = list(type = "smooth"))
  expect_equal(
    add_common_meta(input, sample = input_sample, type = "smooth"),
    output_1
  )

  output_2 <- structure(
    x, meta = list(sample = input_sample, type = "raw")
  )
  expect_equal(
    add_common_meta(
      input, sample = input_sample, type = "raw", attach_sample = TRUE
    ),
    output_2
  )

  output_3 <- structure(
    x, meta = list(sample = input_sample, type = "smooth")
  )
  expect_equal(
    add_common_meta(
      input, sample = input_sample, type = "smooth", attach_sample = TRUE
    ),
    output_3
  )

  output_4 <- structure(
    x, meta = list(extra = list(a = -1), type = "smooth")
  )
  expect_equal(
    add_common_meta(
      input, sample = input_sample, type = "smooth", extra = list(a = -1)
    ),
    output_4
  )
})


# name_sort ---------------------------------------------------------------
test_that("name_sort works", {
  input <- list(c = "a", a = 1, b = list(1), TRUE, z = 0L)
  output <- input[c(2, 3, 1, 5, 4)]
  expect_equal(name_sort(input), output)
})