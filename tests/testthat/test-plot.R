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
  p_fin_2 <- new_p(c(1.5, 1.75), "fin")
  vdiffr::expect_doppelganger(
    "plot-p-fin-1", recordPlot({
      plot(p_fin)
      lines(p_fin_2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-fin-2", recordPlot({
      plot(p_fin, y = NA)
      lines(p_fin_2, col = "red")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-fin-3", recordPlot({
      plot(p_fin, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(p_fin_2, xlab = "q")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-fin-4", recordPlot({
      plot(p_fin, xlim = c(0, 2), col = "blue")
      lines(p_fin_2, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-fin-5", recordPlot({
      plot(p_fin, lty = 2, lwd = 2, pch = 17, cex = 2)
      lines(p_fin_2, lty = 3, lwd = 3, pch = 18, cex = 3)
    })
  )

  vdiffr::expect_doppelganger(
    "plot-p-infin-1", recordPlot({
      plot(p_infin)
      lines(p_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-infin-2", recordPlot({
      plot(p_infin, y = NA)
      lines(p_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-infin-3", recordPlot({
      plot(p_infin, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(p_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-infin-4", recordPlot({
      plot(p_infin, xlim = c(0, 2), col = "blue")
      lines(p_custom)
    })
  )
})

test_that("plot.p and lines.p respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-p-infin-n-grid", recordPlot({
      plot(p_infin, n_grid = 6)
      lines(p_custom, n_grid = 6)
    })
  )
})

test_that("plot.p throws error with corrupt input", {
  f_no_type <- structure(function(q) {user_p(q)}, class = c("p", "pdqr"))
  environment(f_no_type) <- new.env(parent = emptyenv())
  expect_error(plot(f_no_type, "x.*proper.*type"))
})


# plot.d ------------------------------------------------------------------
test_that("plot.d works", {
  # These are also tests for `lines.p()`
  d_fin_2 <- new_d(c(1.5, 1.75), "fin")
  vdiffr::expect_doppelganger(
    "plot-d-fin-1", recordPlot({
      plot(d_fin)
      lines(d_fin_2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-fin-2", recordPlot({
      plot(d_fin, y = NA)
      lines(d_fin_2, col = "red")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-fin-3", recordPlot({
      plot(d_fin, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(d_fin_2, xlab = "q")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-fin-4", recordPlot({
      plot(d_fin, xlim = c(0, 2), col = "blue")
      lines(d_fin_2, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-fin-5", recordPlot({
      plot(d_fin, lty = 2, lwd = 2, pch = 17, cex = 2)
      lines(d_fin_2, lty = 3, lwd = 3, pch = 18, cex = 3)
    })
  )

  # These are also tests for `lines.d()`
  vdiffr::expect_doppelganger(
    "plot-d-infin-1", recordPlot({
      plot(d_infin)
      lines(d_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-infin-2", recordPlot({
      plot(d_infin, y = NA)
      lines(d_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-infin-3", recordPlot({
      plot(d_infin, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(d_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-infin-4", recordPlot({
      plot(d_infin, xlim = c(0, 2), col = "blue")
      lines(d_custom)
    })
  )
})

test_that("plot.d and lines.d respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-d-infin-n-grid", recordPlot({
      plot(d_infin, n_grid = 6)
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
  q_fin_2 <- new_q(c(1.5, 1.75), "fin")
  vdiffr::expect_doppelganger(
    "plot-q-fin-1", recordPlot({
      plot(q_fin)
      lines(q_fin_2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-fin-2", recordPlot({
      plot(q_fin, y = NA)
      lines(q_fin_2, col = "red")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-fin-3", recordPlot({
      plot(q_fin, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(q_fin_2, xlab = "q")
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-fin-4", recordPlot({
      plot(q_fin, xlim = c(0, 2), col = "blue")
      lines(q_fin_2, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-fin-5", recordPlot({
      plot(q_fin, lty = 2, lwd = 2, pch = 17, cex = 2)
      lines(q_fin_2, lty = 3, lwd = 3, pch = 18, cex = 3)
    })
  )

  # These are also tests for `lines.q()`
  vdiffr::expect_doppelganger(
    "plot-q-infin-1", recordPlot({
      plot(q_infin)
      lines(q_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-infin-2", recordPlot({
      plot(q_infin, y = NA)
      lines(q_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-infin-3", recordPlot({
      plot(q_infin, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(q_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-infin-4", recordPlot({
      plot(q_infin, xlim = c(0, 2), col = "blue")
      lines(q_custom)
    })
  )
})

test_that("plot.q and lines.q respect `n_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-q-infin-n-grid", recordPlot({
      plot(q_infin, n_grid = 6)
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
    "plot-r-fin-1", recordPlot(plot(r_fin))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fin-2", recordPlot(plot(r_fin, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-r-fin-3", recordPlot(
      plot(r_fin, xlab = "a", main = "c")
    )
  )
  vdiffr::expect_doppelganger(
    "plot-r-fin-4", recordPlot(
      plot(r_fin, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )

  vdiffr::expect_doppelganger(
    "plot-r-infin-1", recordPlot(plot(r_infin))
  )
  vdiffr::expect_doppelganger(
    "plot-r-infin-2", recordPlot(plot(r_infin, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-r-infin-3",
    recordPlot(plot(r_infin, xlab = "a", main = "c"))
  )
  vdiffr::expect_doppelganger(
    "plot-r-infin-4",
    recordPlot(
      plot(r_infin, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )
})

test_that("plot.r respects `n_sample` argument", {
  vdiffr::expect_doppelganger(
    "plot-r-n-sample", recordPlot(plot(r_infin, n_sample = 10))
  )
})

test_that("plot.r throws error with corrupt input", {
  expect_error(
    plot(structure(user_r, class = c("r", "pdqr"))), "x.*proper.*type"
  )
})


# plot_impl_pdq -----------------------------------------------------------
# Tested in `plot()` methods


# add_p_fin_segments ------------------------------------------------------
# Tested in `plot.p()`


# add_q_fin_segments ------------------------------------------------------
# Tested in `plot.q()`


# compute_p_fin_dots ------------------------------------------------------
# Tested in `plot.p()` and `plot.q()`


# make_plot_dots ----------------------------------------------------------
# Tested in `plot()` methods


# lines.p -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.p throws error with corrupt input", {
  f_no_type <- structure(function(q) {user_p(q)}, class = c("p", "pdqr"))
  environment(f_no_type) <- new.env(parent = emptyenv())
  expect_error(lines(f_no_type), "x.*proper.*type")
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
