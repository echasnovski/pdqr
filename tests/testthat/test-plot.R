context("test-plot")

library(grDevices)

set.seed(5555)

# All tested plot calls are wrapped in `grDevices::recordPlot` to avoid printing
# of the result of their calls (`NULL`). This is a result of {vdiffr} approach:
# it `print()`s its input (resulting here with `NULL`), unless wrapped in
# function.

# plot.p --------------------------------------------------------------
test_that("plot.p works", {
  # These are also tests for `lines.p()`
  p_raw_2 <- new_p(c(1.5, 1.75), "raw")
  vdiffr::expect_doppelganger(
    "plot-p-fun-1", recordPlot({
      plot(p_raw)
      lines(p_raw_2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-fun-2", recordPlot({
      plot(p_raw, y = NA)
      lines(p_raw_2, col = "red")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-fun-3", recordPlot({
      plot(p_raw, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(p_raw_2, xlab = "q")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-fun-4", recordPlot({
      plot(p_raw, xlim = c(0, 2), col = "blue")
      lines(p_raw_2, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-fun-5", recordPlot({
      plot(p_raw, lty = 2, lwd = 2, pch = 17, cex = 2)
      lines(p_raw_2, lty = 3, lwd = 3, pch = 18, cex = 3)
    })
  )

  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-1", recordPlot({
      plot(p_smooth)
      lines(p_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-2", recordPlot({
      plot(p_smooth, y = NA)
      lines(p_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-3", recordPlot({
      plot(p_smooth, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(p_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-4", recordPlot({
      plot(p_smooth, xlim = c(0, 2), col = "blue")
      lines(p_custom)
    })
  )
})

test_that("plot.p and lines.p respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-p-fun-n-grid", recordPlot({
      plot(p_smooth, n_grid = 6)
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

  vdiffr::expect_doppelganger(
    "plot-d-fun-1", recordPlot({
      plot(d_raw)
      lines(d_raw, col = "red")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-fun-2", recordPlot(
      plot(d_raw, y = NA)
    )
  )
  vdiffr::expect_doppelganger(
    "plot-d-fun-3", recordPlot(
      plot(d_raw, xlab = "a", ylab = "b", main = "c", type = "p")
    )
  )
  vdiffr::expect_doppelganger(
    "plot-d-fun-4", recordPlot(plot(d_raw, xlim = c(0, 2), col = "blue"))
  )

  # These are also tests for `lines.d()`
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-1", recordPlot({
      plot(d_smooth)
      lines(d_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-2", recordPlot({
      plot(d_smooth, y = NA)
      lines(d_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-3", recordPlot({
      plot(d_smooth, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(d_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-4", recordPlot({
      plot(d_smooth, xlim = c(0, 2), col = "blue")
      lines(d_custom)
    })
  )
})

test_that("plot.d and lines.d respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-d-fun-n-grid", recordPlot({
      plot(d_smooth, n_grid = 6)
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
  # These are also tests for `lines.p()`
  q_raw_2 <- new_q(c(1.5, 1.75), "raw")
  vdiffr::expect_doppelganger(
    "plot-q-fun-1", recordPlot({
      plot(q_raw)
      lines(q_raw_2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-fun-2", recordPlot({
      plot(q_raw, y = NA)
      lines(q_raw_2, col = "red")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-fun-3", recordPlot({
      plot(q_raw, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(q_raw_2, xlab = "q")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-fun-4", recordPlot({
      plot(q_raw, xlim = c(0, 2), col = "blue")
      lines(q_raw_2, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-fun-5", recordPlot({
      plot(q_raw, lty = 2, lwd = 2, pch = 17, cex = 2)
      lines(q_raw_2, lty = 3, lwd = 3, pch = 18, cex = 3)
    })
  )

  # These are also tests for `lines.q()`
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-1", recordPlot({
      plot(q_smooth)
      lines(q_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-2", recordPlot({
      plot(q_smooth, y = NA)
      lines(q_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-3", recordPlot({
      plot(q_smooth, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(q_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-4", recordPlot({
      plot(q_smooth, xlim = c(0, 2), col = "blue")
      lines(q_custom)
    })
  )
})

test_that("plot.q and lines.q respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-lines-q-fun-n-grid", recordPlot({
      plot(q_smooth, n_grid = 6)
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
    "plot-r-fun-raw-1", recordPlot(plot(r_raw))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-raw-2", recordPlot(plot(r_raw, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-raw-3", recordPlot(
      plot(r_raw, xlab = "a", main = "c")
    )
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-raw-4", recordPlot(
      plot(r_raw, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )

  vdiffr::expect_doppelganger(
    "plot-r-fun-smooth-1", recordPlot(plot(r_smooth))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-smooth-2", recordPlot(plot(r_smooth, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-smooth-3",
    recordPlot(plot(r_smooth, xlab = "a", main = "c"))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fun-smooth-4",
    recordPlot(
      plot(r_smooth, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )
})

test_that("plot.r respects `n_sample` argument", {
  vdiffr::expect_doppelganger(
    "plot-r-fun-n-sample", recordPlot(plot(r_smooth, n_sample = 10))
  )
})

test_that("plot.r throws error with corrupt input", {
  expect_error(
    plot(structure(user_r, class = c("r", "pdqr"))), "x.*proper.*type"
  )
})


# plot_impl_pdq -----------------------------------------------------------
# Tested in `plot()` methods


# add_p_raw_segments ------------------------------------------------------
# Tested in `plot.p()`


# add_q_raw_segments ------------------------------------------------------
# Tested in `plot.q()`


# compute_p_raw_dots ------------------------------------------------------
# Tested in `plot.p()` and `plot.q()`


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
