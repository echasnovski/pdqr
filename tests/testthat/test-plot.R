context("test-plot")

library(grDevices)

set.seed(5555)

# All tested plot calls are wrapped in `grDevices::recordPlot` to avoid printing
# of the result of their calls (`NULL`). This is a result of {vdiffr} approach:
# it `print()`s its input (resulting here with `NULL`), unless wrapped in
# function.

# plot.p --------------------------------------------------------------
test_that("plot.p works", {
  vdiffr::expect_doppelganger("plot-p-fun-1", recordPlot(plot(p_raw_withx)))
  vdiffr::expect_doppelganger(
    "plot-p-fun-2", recordPlot(plot(p_raw_withx, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-p-fun-3", recordPlot(
      plot(p_raw_withx, xlab = "a", ylab = "b", main = "c", type = "p")
    )
  )
  vdiffr::expect_doppelganger(
    "plot-p-fun-4", recordPlot(
      plot(p_raw_withx, xlim = c(0, 2), col = "blue")
    )
  )

  # These are also tests for `lines.p()`
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-1", recordPlot({
      plot(p_smooth_withx)
      lines(p_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-2", recordPlot({
      plot(p_smooth_withx, y = NA)
      lines(p_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-3", recordPlot({
      plot(p_smooth_withx, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(p_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-4", recordPlot({
      plot(p_smooth_withx, xlim = c(0, 2), col = "blue")
      lines(p_custom)
    })
  )
})

test_that("plot.p and lines.p respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-n-grid", recordPlot({
      plot(p_smooth_withx, n_grid = 6)
      lines(p_custom, n_grid = 6)
    })
  )
})

test_that("plot.p throws error with corrupt input", {
  expect_error(
    plot(structure(user_p, class = c("p", "pdqr"))), "x.*proper.*type"
  )
})


# plot.d ------------------------------------------------------------------
test_that("plot.d works", {
  density_warning <- "d-function.*raw.*can miss"

  vdiffr::expect_doppelganger(
    "plot-d-fun-1", recordPlot({
      expect_warning(plot(d_raw_withx), density_warning)
      expect_warning(lines(d_raw_withx, col = "red"), density_warning)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-fun-2", recordPlot(
      expect_warning(plot(d_raw_withx, y = NA), density_warning)
    )
  )
  vdiffr::expect_doppelganger(
    "plot-d-fun-3", recordPlot(
      expect_warning(
        plot(d_raw_withx, xlab = "a", ylab = "b", main = "c", type = "p"),
        density_warning
      )
    )
  )
  vdiffr::expect_doppelganger(
    "plot-d-fun-4", recordPlot(
      expect_warning(
        plot(d_raw_withx, xlim = c(0, 2), col = "blue"),
        density_warning
      )
    )
  )

  # These are also tests for `lines.d()`
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-1", recordPlot({
      plot(d_smooth_withx)
      lines(d_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-2", recordPlot({
      plot(d_smooth_withx, y = NA)
      lines(d_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-3", recordPlot({
      plot(d_smooth_withx, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(d_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-4", recordPlot({
      plot(d_smooth_withx, xlim = c(0, 2), col = "blue")
      lines(d_custom)
    })
  )
})

test_that("plot.d and lines.d respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-n-grid", recordPlot({
      plot(d_smooth_withx, n_grid = 6)
      lines(d_custom, n_grid = 6)
    })
  )
})

test_that("plot.d throws error with corrupt input", {
  expect_error(
    plot(structure(user_d, class = c("d", "pdqr"))), "x.*proper.*type"
  )
})


# plot.q ------------------------------------------------------------------
test_that("plot.q works", {
  vdiffr::expect_doppelganger("plot-q-fun-1", recordPlot(plot(q_raw_withx)))
  vdiffr::expect_doppelganger(
    "plot-q-fun-2", recordPlot(plot(q_raw_withx, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-q-fun-3", recordPlot(
      plot(q_raw_withx, xlab = "a", ylab = "b", main = "c", type = "p")
    )
  )
  vdiffr::expect_doppelganger(
    "plot-q-fun-4", recordPlot(
      plot(q_raw_withx, xlim = c(0, 2), col = "blue")
    )
  )

  # These are also tests for `lines.q()`
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-1", recordPlot({
      plot(q_smooth_withx)
      lines(q_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-2", recordPlot({
      plot(q_smooth_withx, y = NA)
      lines(q_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-3", recordPlot({
      plot(q_smooth_withx, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(q_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-4", recordPlot({
      plot(q_smooth_withx, xlim = c(0, 2), col = "blue")
      lines(q_custom)
    })
  )
})

test_that("plot.q and lines.q respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-n-grid", recordPlot({
      plot(q_smooth_withx, n_grid = 6)
      lines(q_custom, n_grid = 6)
    })
  )
})

test_that("plot.q throws error with corrupt input", {
  expect_error(
    plot(structure(user_q, class = c("q", "pdqr"))), "x.*proper.*type"
  )
})


# plot.r ------------------------------------------------------------------
test_that("plot.r works", {
  vdiffr::expect_doppelganger(
    "plot-r-fun-raw-1", recordPlot(plot(r_raw_withx))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-raw-2", recordPlot(plot(r_raw_withx, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-raw-3", recordPlot(
      plot(r_raw_withx, xlab = "a", main = "c")
    )
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-raw-4", recordPlot(
      plot(r_raw_withx, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )

  vdiffr::expect_doppelganger(
    "plot-r-fun-smooth-1", recordPlot(plot(r_smooth_withx))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-smooth-2", recordPlot(plot(r_smooth_withx, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-smooth-3",
    recordPlot(plot(r_smooth_withx, xlab = "a", main = "c"))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-smooth-4",
    recordPlot(
      plot(r_smooth_withx, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )
})

test_that("plot.r respects `n_sample` argument", {
  vdiffr::expect_doppelganger(
    "plot-r-fun-n-sample", recordPlot(plot(r_smooth_withx, n_sample = 10))
  )
})

test_that("plot.r throws error with corrupt input", {
  expect_error(
    plot(structure(user_r, class = c("r", "pdqr"))), "x.*proper.*type"
  )
})


# plot_impl_pdq -----------------------------------------------------------
# Tested in `plot()` methods


# make_plot_dots ----------------------------------------------------------
# Tested in `plot()` methods


# lines.p -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.p throws error with corrupt input", {
  expect_error(
    lines(structure(user_p, class = c("p", "pdqr"))), "x.*proper.*type"
  )
})


# lines.d -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.d throws error with corrupt input", {
  expect_error(
    lines(structure(user_d, class = c("d", "pdqr"))), "x.*proper.*type"
  )
})


# lines.q -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.q throws error with corrupt input", {
  expect_error(
    lines(structure(user_q, class = c("q", "pdqr"))), "x.*proper.*type"
  )
})

# lines_impl_pdq ----------------------------------------------------------
# Tested in `plot()` methods
