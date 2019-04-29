context("summ_hdr")


# Helper data -------------------------------------------------------------
empty_hdr <- data.frame(left = numeric(0), right = numeric(0))


# compute_hdr_intervals ---------------------------------------------------
test_that("compute_hdr_intervals works", {
  cur_d_1 <- new_d(data.frame(x = c(0, 2), y = c(0, 1)), "infin")
  expect_equal(
    compute_hdr_intervals(cur_d_1, 0), data.frame(left = 0, right = 2)
  )
  expect_equal(
    compute_hdr_intervals(cur_d_1, 1/3), data.frame(left = 2/3, right = 2)
  )
  # Here empty HDR is returned because otherwise output would have zero width
  expect_equal(compute_hdr_intervals(cur_d_1, 1), empty_hdr)
  # Whole support is returned because its every point has density more than `-1`
  expect_equal(
    compute_hdr_intervals(cur_d_1, -1), data.frame(left = 0, right = 2)
  )
  expect_equal(compute_hdr_intervals(cur_d_1, 2), empty_hdr)

  cur_d_2 <- new_d(data.frame(x = 0:2, y = c(0, 1, 0)), "infin")
  expect_equal(
    compute_hdr_intervals(cur_d_2, 0), data.frame(left = 0, right = 2)
  )
  expect_equal(
    compute_hdr_intervals(cur_d_2, 0.5), data.frame(left = 0.5, right = 1.5)
  )
  expect_equal(compute_hdr_intervals(cur_d_2, 1), empty_hdr)

  cur_d_3 <- new_d(data.frame(x = 0:6, y = c(0, 1, 0, 1, 0, 1, 0)/3), "infin")
  # Whole support in single row is returned because consecutive intervals in
  # output rows should be "collapsed" to one. In other words, there should be
  # non-zero distance between consecutive intervals in HDR.
  expect_equal(
    compute_hdr_intervals(cur_d_3, 0), data.frame(left = 0, right = 6)
  )
  expect_equal(
    compute_hdr_intervals(cur_d_3, 1/6),
    data.frame(left = c(0.5, 2.5, 4.5), right = c(1.5, 3.5, 5.5))
  )
  expect_equal(compute_hdr_intervals(cur_d_3, 1/3), empty_hdr)


  cur_d_4 <- new_d(data.frame(x = 1:5, y = c(1, 0, 2, 0, 1)/3), "infin")
  # Although 1 and 5 also cross height `1/3`, they can only contribute a zero
  # width intervals to output
  expect_equal(
    compute_hdr_intervals(cur_d_4, 1/3), data.frame(left = 2.5, right = 3.5)
  )
})

test_that("compute_hdr_intervals works with dirac-like intervals", {
  d_dirac_1 <- new_d(1, "infin")
  expect_equal(
    compute_hdr_intervals(d_dirac_1, 0.5e8),
    data.frame(left = 1 - 0.5e-8, right = 1 + 0.5e-8),
    tolerance = 1e-12
  )

  d_dirac_2 <- form_mix(list(new_d(1, "infin"), new_d(2, "infin")))
  expect_equal(
    compute_hdr_intervals(d_dirac_2, 0.25e8),
    data.frame(left = 1:2 - 0.5e-8, right = 1:2 + 0.5e-8),
    tolerance = 1e-12
  )

  d_winsor <- form_tails(
    new_d(data.frame(x = 0:1, y = c(1, 1)), "infin"),
    level = 0.05, method = "winsor", direction = "both"
  )
  expect_equal(
    compute_hdr_intervals(d_winsor, 1.2),
    data.frame(left = c(0.05, 0.95-1e-8), right = c(0.05+1e-8, 0.95)),
    tolerance = 1e-12
  )
})

test_that("compute_hdr_intervals handles plateaus", {
  cur_d_1 <- new_d(data.frame(x = 0:1, y = c(1, 1)), "infin")
  expect_equal(
    compute_hdr_intervals(cur_d_1, 1), data.frame(left = 0, right = 1)
  )

  cur_d_2 <- new_d(data.frame(x = c(0, 1, 10, 11), y = c(1, 0, 0, 1)), "infin")
  expect_equal(
    compute_hdr_intervals(cur_d_2, 0), data.frame(left = 0, right = 11)
  )

  cur_d_3 <- new_d(
    data.frame(
      x = c(0, 1, 1.5, 2, 3, 4, 4.2, 5, 6), y = c(0, 1, 1, 1, 0, 1, 1, 1, 0)/4
    ),
    "infin"
  )
  # Only left and right edges of plateaus (that may stretch over several
  # consecutive intervals) should be returned.
  expect_equal(
    compute_hdr_intervals(cur_d_3, 1/4),
    data.frame(left = c(1, 4), right = c(2, 5))
  )
})


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
