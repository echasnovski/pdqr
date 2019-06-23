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
    "plot-p-continuous-1", recordPlot({
      plot(p_con)
      lines(p_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-continuous-2", recordPlot({
      plot(p_con, y = NA)
      lines(p_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-continuous-3", recordPlot({
      plot(p_con, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(p_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-p-continuous-4", recordPlot({
      plot(p_con, xlim = c(0, 2), col = "blue")
      lines(p_custom)
    })
  )
})

test_that("plot.p and lines.p use `n_extra_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-p-continuous-n-extra-grid", recordPlot({
      plot(
        new_p(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
        n_extra_grid = NULL, type = "b", main = "n_extra_grid check"
      )
      lines(
        new_p(data.frame(x = c(0.2, 1), y = c(1, 1)), "continuous"),
        n_extra_grid = 3, type = "b", col = "blue"
      )
    })
  )
})

test_that("plot.p validates input", {
  f_no_type <- structure(function(q) {user_p(q)}, class = c("p", "pdqr"))
  environment(f_no_type) <- new.env(parent = emptyenv())
  expect_error(plot(f_no_type), "`x`.*not pdqr-function")
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
    "plot-d-continuous-1", recordPlot({
      plot(d_con)
      lines(d_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-continuous-2", recordPlot({
      plot(d_con, y = NA)
      lines(d_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-continuous-3", recordPlot({
      plot(d_con, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(d_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-d-continuous-4", recordPlot({
      plot(d_con, xlim = c(0, 2), col = "blue")
      lines(d_custom)
    })
  )
})

test_that("plot.d and lines.d use `n_extra_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-d-continuous-n-extra-grid", recordPlot({
      plot(
        new_d(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
        n_extra_grid = NULL, type = "b", main = "n_extra_grid check"
      )
      lines(
        new_d(data.frame(x = c(0.2, 1), y = c(1, 1)), "continuous"),
        n_extra_grid = 3, type = "b", col = "blue"
      )
    })
  )
})

test_that("plot.d handles dirac-like 'continuous' functions",  {
  vdiffr::expect_doppelganger(
    "dirac-like-1", recordPlot({
      plot(form_retype(d_fin, "continuous", method = "dirac"))
    })
  )
  vdiffr::expect_doppelganger(
    "dirac-like-2", recordPlot({
      plot(form_resupport(d_con, c(-2, 1), method = "winsor"))
    })
  )
  vdiffr::expect_doppelganger(
    "dirac-like-3", recordPlot({
      plot(form_mix(list(d_fin, d_con)))
    })
  )
  vdiffr::expect_doppelganger(
    "dirac-like-4", recordPlot({
      plot(new_d(1, "continuous"))
    })
  )
})

test_that("plot.d validates input", {
  expect_error(
    plot(structure(user_d, class = c("d", "pdqr"))), "`x`.*not pdqr-function"
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
    "plot-q-continuous-1", recordPlot({
      plot(q_con)
      lines(q_custom)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-continuous-2", recordPlot({
      plot(q_con, y = NA)
      lines(q_custom, y = NA)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-continuous-3", recordPlot({
      plot(q_con, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(q_custom, lwd = 2)
    })
  )
  vdiffr::expect_doppelganger(
    "plot-q-continuous-4", recordPlot({
      plot(q_con, xlim = c(0, 2), col = "blue")
      lines(q_custom)
    })
  )
})

test_that("plot.q and lines.q use `n_extra_grid` argument", {
  vdiffr::expect_doppelganger(
    "plot-q-continuous-n-extra-grid", recordPlot({
      plot(
        new_q(data.frame(x = 0:1, y = c(1, 1)), "continuous"),
        n_extra_grid = NULL, type = "b", main = "n_extra_grid check"
      )
      lines(
        new_q(data.frame(x = c(0.2, 1), y = c(1, 1)), "continuous"),
        n_extra_grid = 3, type = "b", col = "blue"
      )
    })
  )
})

test_that("plot.q validates input", {
  expect_error(
    plot(structure(user_q, class = c("q", "pdqr"))), "`x`.*not pdqr-function"
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
    "plot-r-continuous-1", recordPlot(plot(r_con))
  )
  vdiffr::expect_doppelganger(
    "plot-r-continuous-2", recordPlot(plot(r_con, y = NA))
  )
  vdiffr::expect_doppelganger(
    "plot-r-continuous-3",
    recordPlot(plot(r_con, xlab = "a", main = "c"))
  )
  vdiffr::expect_doppelganger(
    "plot-r-continuous-4",
    recordPlot(
      plot(r_con, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )
})

test_that("plot.r uses `n_sample` argument", {
  vdiffr::expect_doppelganger(
    "plot-r-n-sample", recordPlot(plot(r_con, n_sample = 10))
  )
})

test_that("plot.r validates input", {
  expect_error(
    plot(structure(user_r, class = c("r", "pdqr"))), "`x`.*not pdqr-function"
  )
})


# plot_impl_pdq -----------------------------------------------------------
# Tested in `plot()` methods


# compute_plot_grid -------------------------------------------------------
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
test_that("lines.p validates input", {
  f_no_type <- structure(function(q) {user_p(q)}, class = c("p", "pdqr"))
  environment(f_no_type) <- new.env(parent = emptyenv())
  expect_error(lines(f_no_type), "`x`.*not pdqr-function")
})


# lines.d -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.d validates input", {
  expect_error(
    lines(structure(user_d, class = c("d", "pdqr"))), "`x`.*not pdqr-function"
  )
})


# lines.q -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.q validates input", {
  expect_error(
    lines(structure(user_q, class = c("q", "pdqr"))), "`x`.*not pdqr-function"
  )
})

# lines_impl_pdq ----------------------------------------------------------
# Tested in `plot()` methods
