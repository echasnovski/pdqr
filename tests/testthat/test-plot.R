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
  p_dis_2 <- new_p(c(1.5, 1.75), "discrete")
  expect_doppelganger_2(
    "plot-p-discrete-1", recordPlot({
      plot(p_dis)
      lines(p_dis_2)
    })
  )
  expect_doppelganger_2(
    "plot-p-discrete-2", recordPlot({
      plot(p_dis, y = NA)
      lines(p_dis_2, col = "red")
    })
  )
  expect_doppelganger_2(
    "plot-p-discrete-3", recordPlot({
      plot(p_dis, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(p_dis_2, xlab = "q")
    })
  )
  expect_doppelganger_2(
    "plot-p-discrete-4", recordPlot({
      plot(p_dis, xlim = c(0, 2), col = "blue")
      lines(p_dis_2, lwd = 2)
    })
  )
  expect_doppelganger_2(
    "plot-p-discrete-5", recordPlot({
      plot(p_dis, lty = 2, lwd = 2, pch = 17, cex = 2)
      lines(p_dis_2, lty = 3, lwd = 3, pch = 18, cex = 3)
    })
  )

  expect_doppelganger_2(
    "plot-p-continuous-1", recordPlot({
      plot(p_con)
      lines(p_custom)
    })
  )
  expect_doppelganger_2(
    "plot-p-continuous-2", recordPlot({
      plot(p_con, y = NA)
      lines(p_custom, y = NA)
    })
  )
  expect_doppelganger_2(
    "plot-p-continuous-3", recordPlot({
      plot(p_con, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(p_custom, lwd = 2)
    })
  )
  expect_doppelganger_2(
    "plot-p-continuous-4", recordPlot({
      plot(p_con, xlim = c(0, 2), col = "blue")
      lines(p_custom)
    })
  )
})

test_that("plot.p and lines.p use `n_extra_grid` argument", {
  expect_doppelganger_2(
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
  f_no_type <- structure(function(q) {
    user_p(q)
  }, class = c("p", "pdqr"))
  environment(f_no_type) <- new.env(parent = emptyenv())
  expect_error(plot(f_no_type), "`x`.*not pdqr-function")

  expect_error(plot(p_con, n_extra_grid = "a"), "`n_extra_grid`.*single number")
  expect_error(plot(p_con, n_extra_grid = 1:2), "`n_extra_grid`.*single number")
})


# plot.d ------------------------------------------------------------------
test_that("plot.d works", {
  # These are also tests for `lines.p()`
  d_dis_2 <- new_d(c(1.5, 1.75), "discrete")
  expect_doppelganger_2(
    "plot-d-discrete-1", recordPlot({
      plot(d_dis)
      lines(d_dis_2)
    })
  )
  expect_doppelganger_2(
    "plot-d-discrete-2", recordPlot({
      plot(d_dis, y = NA)
      lines(d_dis_2, col = "red")
    })
  )
  expect_doppelganger_2(
    "plot-d-discrete-3", recordPlot({
      plot(d_dis, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(d_dis_2, xlab = "q")
    })
  )
  expect_doppelganger_2(
    "plot-d-discrete-4", recordPlot({
      plot(d_dis, xlim = c(0, 2), col = "blue")
      lines(d_dis_2, lwd = 2)
    })
  )
  expect_doppelganger_2(
    "plot-d-discrete-5", recordPlot({
      plot(d_dis, lty = 2, lwd = 2, pch = 17, cex = 2)
      lines(d_dis_2, lty = 3, lwd = 3, pch = 18, cex = 3)
    })
  )

  # These are also tests for `lines.d()`
  expect_doppelganger_2(
    "plot-d-continuous-1", recordPlot({
      plot(d_con)
      lines(d_custom)
    })
  )
  expect_doppelganger_2(
    "plot-d-continuous-2", recordPlot({
      plot(d_con, y = NA)
      lines(d_custom, y = NA)
    })
  )
  expect_doppelganger_2(
    "plot-d-continuous-3", recordPlot({
      plot(d_con, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(d_custom, lwd = 2)
    })
  )
  expect_doppelganger_2(
    "plot-d-continuous-4", recordPlot({
      plot(d_con, xlim = c(0, 2), col = "blue")
      lines(d_custom)
    })
  )
})

test_that("plot.d and lines.d use `n_extra_grid` argument", {
  expect_doppelganger_2(
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
  expect_doppelganger_2(
    "dirac-like-1", recordPlot({
      plot(form_retype(d_dis, "continuous", method = "dirac"))
    })
  )
  expect_doppelganger_2(
    "dirac-like-2", recordPlot({
      plot(form_resupport(d_con, c(-2, 1), method = "winsor"))
    })
  )
  expect_doppelganger_2(
    "dirac-like-3", recordPlot({
      plot(form_mix(list(d_dis, d_con)))
    })
  )
  expect_doppelganger_2(
    "dirac-like-4", recordPlot({
      plot(new_d(1, "continuous"))
    })
  )
})

test_that("plot.d validates input", {
  expect_error(
    plot(structure(user_d, class = c("d", "pdqr"))), "`x`.*not pdqr-function"
  )

  expect_error(plot(d_con, n_extra_grid = "a"), "`n_extra_grid`.*single number")
  expect_error(plot(d_con, n_extra_grid = 1:2), "`n_extra_grid`.*single number")
})


# plot.q ------------------------------------------------------------------
test_that("plot.q works", {
  # These are also tests for `lines.p()`
  q_dis_2 <- new_q(c(1.5, 1.75), "discrete")
  expect_doppelganger_2(
    "plot-q-discrete-1", recordPlot({
      plot(q_dis)
      lines(q_dis_2)
    })
  )
  expect_doppelganger_2(
    "plot-q-discrete-2", recordPlot({
      plot(q_dis, y = NA)
      lines(q_dis_2, col = "red")
    })
  )
  expect_doppelganger_2(
    "plot-q-discrete-3", recordPlot({
      plot(q_dis, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(q_dis_2, xlab = "q")
    })
  )
  expect_doppelganger_2(
    "plot-q-discrete-4", recordPlot({
      plot(q_dis, xlim = c(0, 2), col = "blue")
      lines(q_dis_2, lwd = 2)
    })
  )
  expect_doppelganger_2(
    "plot-q-discrete-5", recordPlot({
      plot(q_dis, lty = 2, lwd = 2, pch = 17, cex = 2)
      lines(q_dis_2, lty = 3, lwd = 3, pch = 18, cex = 3)
    })
  )

  # These are also tests for `lines.q()`
  expect_doppelganger_2(
    "plot-q-continuous-1", recordPlot({
      plot(q_con)
      lines(q_custom)
    })
  )
  expect_doppelganger_2(
    "plot-q-continuous-2", recordPlot({
      plot(q_con, y = NA)
      lines(q_custom, y = NA)
    })
  )
  expect_doppelganger_2(
    "plot-q-continuous-3", recordPlot({
      plot(q_con, xlab = "a", ylab = "b", main = "c", type = "p")
      lines(q_custom, lwd = 2)
    })
  )
  expect_doppelganger_2(
    "plot-q-continuous-4", recordPlot({
      plot(q_con, xlim = c(0, 2), col = "blue")
      lines(q_custom)
    })
  )
})

test_that("plot.q and lines.q use `n_extra_grid` argument", {
  expect_doppelganger_2(
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

  expect_error(plot(q_con, n_extra_grid = "a"), "`n_extra_grid`.*single number")
  expect_error(plot(q_con, n_extra_grid = 1:2), "`n_extra_grid`.*single number")
})


# plot.r ------------------------------------------------------------------
test_that("plot.r works", {
  expect_doppelganger_2(
    "plot-r-discrete-1", recordPlot(plot(r_dis))
  )
  expect_doppelganger_2(
    "plot-r-discrete-2", recordPlot(plot(r_dis, y = NA))
  )
  expect_doppelganger_2(
    "plot-r-discrete-3", recordPlot(
      plot(r_dis, xlab = "a", main = "c")
    )
  )
  expect_doppelganger_2(
    "plot-r-discrete-4", recordPlot(
      plot(r_dis, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )

  expect_doppelganger_2(
    "plot-r-continuous-1", recordPlot(plot(r_con))
  )
  expect_doppelganger_2(
    "plot-r-continuous-2", recordPlot(plot(r_con, y = NA))
  )
  expect_doppelganger_2(
    "plot-r-continuous-3",
    recordPlot(plot(r_con, xlab = "a", main = "c"))
  )
  expect_doppelganger_2(
    "plot-r-continuous-4",
    recordPlot(
      plot(r_con, xlim = c(0, 2), col = "blue", freq = FALSE)
    )
  )
})

test_that("plot.r uses `n_sample` argument", {
  expect_doppelganger_2(
    "plot-r-n-sample", recordPlot(plot(r_con, n_sample = 10))
  )
})

test_that("plot.r validates input", {
  expect_error(
    plot(structure(user_r, class = c("r", "pdqr"))), "`x`.*not pdqr-function"
  )
  expect_error(plot(r_con, n_sample = "a"), "`n_sample`.*single number")
  expect_error(plot(r_con, n_sample = 1:2), "`n_sample`.*single number")
})


# plot_impl_pdq -----------------------------------------------------------
# Tested in `plot()` methods


# compute_plot_grid -------------------------------------------------------
# Tested in `plot()` methods


# add_p_dis_segments ------------------------------------------------------
# Tested in `plot.p()`


# add_q_dis_segments ------------------------------------------------------
# Tested in `plot.q()`


# compute_p_dis_dots ------------------------------------------------------
# Tested in `plot.p()` and `plot.q()`


# make_plot_dots ----------------------------------------------------------
# Tested in `plot()` methods


# lines.p -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.p validates input", {
  f_no_type <- structure(function(q) {
    user_p(q)
  }, class = c("p", "pdqr"))
  environment(f_no_type) <- new.env(parent = emptyenv())
  expect_error(lines(f_no_type), "`x`.*not pdqr-function")

  expect_error(
    lines(p_con, n_extra_grid = "a"), "`n_extra_grid`.*single number"
  )
  expect_error(
    lines(p_con, n_extra_grid = 1:2), "`n_extra_grid`.*single number"
  )
})


# lines.d -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.d validates input", {
  expect_error(
    lines(structure(user_d, class = c("d", "pdqr"))), "`x`.*not pdqr-function"
  )

  expect_error(
    lines(d_con, n_extra_grid = "a"), "`n_extra_grid`.*single number"
  )
  expect_error(
    lines(d_con, n_extra_grid = 1:2), "`n_extra_grid`.*single number"
  )
})


# lines.q -----------------------------------------------------------------
# Main functionality is tested in `plot()` methods
test_that("lines.q validates input", {
  expect_error(
    lines(structure(user_q, class = c("q", "pdqr"))), "`x`.*not pdqr-function"
  )

  expect_error(
    lines(q_con, n_extra_grid = "a"), "`n_extra_grid`.*single number"
  )
  expect_error(
    lines(q_con, n_extra_grid = 1:2), "`n_extra_grid`.*single number"
  )
})

# lines_impl_pdq ----------------------------------------------------------
# Tested in `plot()` methods
