context("summ_hdr")


# compute_density_height_points -------------------------------------------
test_that("compute_density_height_points works", {
  cur_d_1 <- new_d(data.frame(x = c(0, 2), y = c(0, 1)), "infin")
  expect_equal(compute_density_height_points(cur_d_1, 0), 0)
  expect_equal(compute_density_height_points(cur_d_1, 1/3), 2/3)
  expect_equal(compute_density_height_points(cur_d_1, 1), 2)
  expect_equal(compute_density_height_points(cur_d_1, -1), numeric(0))
  expect_equal(compute_density_height_points(cur_d_1, 2), numeric(0))

  cur_d_2 <- new_d(data.frame(x = 0:2, y = c(0, 1, 0)), "infin")
  expect_equal(compute_density_height_points(cur_d_2, 0), c(0, 2))
  expect_equal(compute_density_height_points(cur_d_2, 0.5), 0.5 + 0:1)
  expect_equal(compute_density_height_points(cur_d_2, 1), 1)

  cur_d_3 <- new_d(data.frame(x = 0:6, y = c(0, 1, 0, 1, 0, 1, 0)/3), "infin")
  expect_equal(compute_density_height_points(cur_d_3, 0), c(0, 2, 4, 6))
  expect_equal(compute_density_height_points(cur_d_3, 1/6), 0.5 + 0:5)
  expect_equal(compute_density_height_points(cur_d_3, 1/3), c(1, 3, 5))

  cur_d_4 <- new_d(data.frame(x = 1:5, y = c(1, 0, 2, 0, 1)/3), "infin")
  expect_equal(compute_density_height_points(cur_d_4, 1/3), c(1, 2.5, 3.5, 5))
})

test_that("compute_density_height_points works with dirac-like intervals", {
  d_dirac_1 <- new_d(1, "infin")
  expect_equal(
    compute_density_height_points(d_dirac_1, 0.5e8), 1 + c(-1, 1)*0.5e-8,
    tolerance = 1e-12
  )

  d_dirac_2 <- form_mix(list(new_d(1, "infin"), new_d(2, "infin")))
  expect_equal(
    compute_density_height_points(d_dirac_2, 0.25e8),
    c(1 + c(-1, 1)*0.5e-8, 2 + c(-1, 1)*0.5e-8),
    tolerance = 1e-12
  )

  d_winsor <- form_tails(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "infin"),
    level = 0.05, method = "winsor", direction = "both"
  )
  expect_equal(
    compute_density_height_points(d_winsor, 1.2), c(0.05, 0.95),
    tolerance = 1e-8
  )
})

test_that("compute_density_height_points handles plateaus", {
  # Only left and right edges of plateaus (that may stretch over several
  # consecutive intervals) should be returned.
  cur_d_1 <- new_d(data.frame(x = 0:1, y = c(1, 1)), "infin")
  expect_equal(compute_density_height_points(cur_d_1, 1), 0:1)

  cur_d_2 <- new_d(data.frame(x = c(0, 1, 10, 11), y = c(1, 0, 0, 1)), "infin")
  expect_equal(compute_density_height_points(cur_d_2, 0), c(1, 10))

  cur_d_3 <- new_d(
    data.frame(
      x = c(0, 1, 1.5, 2, 3, 4, 4.2, 5, 6), y = c(0, 1, 1, 1, 0, 1, 1, 1, 0)/4
    ),
    "infin"
  )
  expect_equal(compute_density_height_points(cur_d_3, 1/4), c(1, 2, 4, 5))
})
