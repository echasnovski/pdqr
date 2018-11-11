context("test-plot")

library(grDevices)

set.seed(5555)

# All tested plot calls are wrapped in `grDevices::recordPlot` to avoid printing
# of the result of their calls (`NULL`). This is a result of {vdiffr} approach:
# it `print()`s its input (resulting here with `NULL`), unless wrapped in
# function.

# plot.p_fun --------------------------------------------------------------
test_that("plot.p_fun works", {
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

  # Thise are also tests for `lines.p_fun()`
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

test_that("plot.p_fun and lines.p_fun respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-n-grid", recordPlot({
      plot(p_smooth_withx, n_grid = 6)
      lines(p_custom, n_grid = 6)
    })
  )
})


# plot.d_fun --------------------------------------------------------------
test_that("plot.d_fun works", {
  density_warning <- "[Dd]ensity.*raw.*can miss"

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

  # Thise are also tests for `lines.d_fun()`
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

test_that("plot.d_fun and lines.d_fun respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-n-grid", recordPlot({
      plot(d_smooth_withx, n_grid = 6)
      lines(d_custom, n_grid = 6)
    })
  )
})


# plot.q_fun --------------------------------------------------------------
test_that("plot.q_fun works", {
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

  # Thise are also tests for `lines.q_fun()`
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

test_that("plot.q_fun and lines.q_fun respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-n-grid", recordPlot({
      plot(q_smooth_withx, n_grid = 6)
      lines(q_custom, n_grid = 6)
    })
  )
})


# plot.r_fun --------------------------------------------------------------
test_that("plot.r_fun works", {
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

test_that("plot.r_fun respects `n_sample` argument", {
  vdiffr::expect_doppelganger(
    "plot-r-fun-n-sample", recordPlot(plot(r_smooth_withx, n_sample = 10))
  )
})


# plot_impl_pdq -----------------------------------------------------------
# Tested in `plot()` methods


# make_plot_dots ----------------------------------------------------------
# Tested in `plot()` methods


# lines.p_fun -------------------------------------------------------------
# Tested in `plot()` methods


# lines.d_fun -------------------------------------------------------------
# Tested in `plot()` methods


# lines.q_fun -------------------------------------------------------------
# Tested in `plot()` methods


# lines_impl_pdq ----------------------------------------------------------
# Tested in `plot()` methods
