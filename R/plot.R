# Documentation -----------------------------------------------------------
#' Pdqr methods for base plotting functions
#'
#' Pdqr-functions have their own methods for [plot()] and [lines()] (except
#' r-functions, see Details).
#'
#' @param x Pdqr-function to plot.
#' @param y Argument for compatibility with `plot()` signature. Doesn't used.
#' @param n_extra_grid Number of extra grid points at which to evaluate
#'   pdqr-function (see Details). Supply `NULL` or `0` to not use extra grid.
#' @param ... Other arguments for `plot()` or [hist()][graphics::hist()] (in
#'   case of plotting r-function).
#' @param n_sample Size of a sample to be generated for plotting histogram in
#'   case of an r-function.
#'
#' @details Main idea of plotting pdqr-functions is to use plotting mechanisms
#' for appropriate numerical data.
#'
#' Plotting of [type][meta_type()] **discrete** functions:
#' - P-functions are plotted as step-line with jumps at points of "x" column of
#' ["x_tbl" metadata][meta_x_tbl()].
#' - D-functions are plotted with vertical lines at points of "x" column of
#' "x_tbl" with height equal to values from "prob" column.
#' - Q-functions are plotted as step-line with jumps at points of "cumprob"
#' column of "x_tbl".
#' - R-functions are plotted by generating sample of size `n_sample` and calling
#' [hist()][graphics::hist()] function.
#'
#' Plotting of type **continuous** functions:
#' - P-functions are plotted in piecewise-linear fashion at their values on
#' compound grid: sorted union of "x" column from "x_tbl" metadata and sequence
#' of length `n_extra_grid` consisting from equidistant points between edges of
#' support. Here extra grid is needed to show curvature of lines between "x"
#' points from "x_tbl" (see Examples).
#' - D-functions are plotted in the same way as p-functions.
#' - Q-functions are plotted similarly as p- and d-functions but grid consists
#' from union of "cumprob" column of "x_tbl" metadata and equidistant grid of
#' length `n_extra_grid` from 0 to 1.
#' - R-functions are plotted the same way as type "discrete" ones: as histogram
#' of generated sample of size `n_sample`.
#'
#' @return Output of [invisible()][base::invisible()] without arguments, i.e.
#'   `NULL` without printing.
#'
#' @family pdqr methods for generic functions
#'
#' @examples
#' d_norm_1 <- as_d(dnorm)
#' d_norm_2 <- as_d(dnorm, mean = 1)
#'
#' plot(d_norm_1)
#' lines(d_norm_2, col = "red")
#'
#' # Usage of `n_extra_grid` is important in case of "continuous" p- and
#' # q-functions
#' simple_p <- new_p(data.frame(x = c(0, 1), y = c(0, 1)), "continuous")
#' plot(simple_p, main = "Case study of n_extra_grid argument")
#' lines(simple_p, n_extra_grid = 0, col = "red")
#'
#' # R-functions are plotted with histogram
#' plot(as_r(d_norm_1))
#' @name methods-plot
NULL


# plot() ------------------------------------------------------------------
#' @rdname methods-plot
#' @export
plot.p <- function(x, y = NULL, n_extra_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)
  assert_type(
    n_extra_grid, is_single_number, type_name = "single number",
    allow_null = TRUE
  )

  dots <- make_plot_dots(
    ...,
    type = "l", main = paste0("Cumulative distribution function ", x_name),
    xlab = "x", ylab = "Cumulative probability"
  )

  if (meta_type(x) == "discrete") {
    # Create canvas
    no_plot_dots <- c_dedupl(
      list(x = meta_support(x), y = c(0, 1), type = "n"),
      dots
    )

    do.call(graphics::plot, no_plot_dots)

    # Add segments
    add_p_dis_segments(x, list(...))
  } else {
    plot_impl_pdq(x, compute_plot_grid(x, n_extra_grid), dots)
  }
}

#' @rdname methods-plot
#' @export
plot.d <- function(x, y = NULL, n_extra_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)
  assert_type(
    n_extra_grid, is_single_number, type_name = "single number",
    allow_null = TRUE
  )

  if (meta_type(x) == "discrete") {
    x_tbl <- meta_x_tbl(x)

    dots <- make_plot_dots(
      ...,
      type = "h", main = paste0("Probability mass function ", x_name),
      xlab = "x", ylab = "Probability", ylim = c(0, max(x_tbl[["prob"]]))
    )
    plot_args <- c_dedupl(list(x = x_tbl[["x"]], y = x_tbl[["prob"]]), dots)

    do.call(graphics::plot, plot_args)
  } else {
    dots <- make_plot_dots(
      ...,
      type = "l", main = paste0("Density function ", x_name),
      xlab = "x", ylab = "Density",
      # This is needed for a nice plotting in presence of dirac-like entries in
      # "x_tbl". Note, that this will compute `ylim` based on "x_tbl" in `x` and
      # not based on actually plotted points, so the output `ylim` can be an
      # overestimation (which is kind of a good indication of too small value
      # of `n_extra_grid`).
      ylim = compute_d_con_ylim(x)
    )

    plot_impl_pdq(x, compute_plot_grid(x, n_extra_grid), dots)
  }
}

#' @rdname methods-plot
#' @export
plot.q <- function(x, y = NULL, n_extra_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)
  assert_type(
    n_extra_grid, is_single_number, type_name = "single number",
    allow_null = TRUE
  )

  dots <- make_plot_dots(
    ...,
    type = "l", main = paste0("Quantile function ", x_name),
    xlab = "Cumulative probability", ylab = "x"
  )

  if (meta_type(x) == "discrete") {
    # Create canvas
    no_plot_dots <- c_dedupl(
      list(x = c(0, 1), y = meta_support(x), type = "n"),
      dots
    )

    do.call(graphics::plot, no_plot_dots)

    # Add segments
    add_q_dis_segments(x, list(...))
  } else {
    plot_impl_pdq(x, compute_plot_grid(x, n_extra_grid), dots)
  }
}

#' @rdname methods-plot
#' @export
plot.r <- function(x, y = NULL, n_sample = 1000, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)
  assert_type(n_sample, is_single_number, type_name = "single number")

  smpl <- x(n_sample)
  hist_args <- make_plot_dots(
    x = smpl,
    ...,
    main = paste0("Output of random generation function ", x_name),
    xlab = "x"
  )

  do.call(graphics::hist, hist_args)

  invisible()
}

plot_impl_pdq <- function(f, grid, dots) {
  plot_args <- c_dedupl(list(x = grid, y = f(grid)), dots)

  do.call(graphics::plot, plot_args)
}

compute_plot_grid <- function(f, n_extra_grid) {
  f_class <- meta_class(f)
  f_x_tbl <- meta_x_tbl(f)
  f_support <- meta_support(f)

  grid <- switch(
    f_class, p = f_x_tbl[["x"]], d = f_x_tbl[["x"]], q = f_x_tbl[["cumprob"]]
  )

  if (is.null(n_extra_grid)) {
    extra_grid <- numeric()
  } else {
    extra_grid <- switch(
      f_class,
      p = seq_between(f_support, length.out = n_extra_grid),
      d = seq_between(f_support, length.out = n_extra_grid),
      q = seq_between(c(0, 1), length.out = n_extra_grid)
    )
  }

  sort(unique(c(grid, extra_grid)))
}

add_p_dis_segments <- function(x, dots) {
  plot_dots <- compute_p_dis_dots(x, dots)

  do.call(graphics::segments, plot_dots[["seg_ver"]])
  do.call(graphics::segments, plot_dots[["seg_hor"]])
  do.call(graphics::points, plot_dots[["points"]])
}

add_q_dis_segments <- function(x, dots) {
  plot_p_dots <- compute_p_dis_dots(x, dots)

  # Output should be inverse of plot for "p"
  plot_dots <- list(
    seg_ver = plot_p_dots[["seg_hor"]],
    seg_hor = plot_p_dots[["seg_ver"]],
    points = plot_p_dots[["points"]]
  )
  plot_dots[["seg_ver"]] <- swap(plot_dots[["seg_ver"]], "x0", "y0")
  plot_dots[["seg_ver"]] <- swap(plot_dots[["seg_ver"]], "x1", "y1")
  plot_dots[["seg_ver"]][["lty"]] <- plot_p_dots[["seg_ver"]][["lty"]]

  plot_dots[["seg_hor"]] <- swap(plot_dots[["seg_hor"]], "x0", "y0")
  plot_dots[["seg_hor"]] <- swap(plot_dots[["seg_hor"]], "x1", "y1")
  plot_dots[["seg_hor"]][["lty"]] <- plot_p_dots[["seg_hor"]][["lty"]]

  plot_dots[["points"]] <- swap(plot_dots[["points"]], "x", "y")

  do.call(graphics::segments, plot_dots[["seg_ver"]])
  do.call(graphics::segments, plot_dots[["seg_hor"]])
  do.call(graphics::points, plot_dots[["points"]])
}

compute_p_dis_dots <- function(x, dots) {
  x_tbl <- meta_x_tbl(x)
  x <- x_tbl[["x"]]
  cumprob <- x_tbl[["cumprob"]]
  n <- nrow(x_tbl)

  # Remove supplied `type` argument as it will not work and throw warnings
  dots[["type"]] <- NULL

  vertical_segments_dots <- c_dedupl(
    dots,
    list(x0 = x, y0 = c(0, cumprob[-n]), x1 = x, y1 = cumprob, lty = 2)
  )

  horizontal_segments_dots <- c_dedupl(
    dots,
    list(x0 = x[-n], y0 = cumprob[-n], x1 = x[-1], y1 = cumprob[-n])
  )

  points_dots <- c_dedupl(dots, list(x = x, y = cumprob, pch = 16))

  list(
    seg_ver = vertical_segments_dots,
    seg_hor = horizontal_segments_dots,
    points = points_dots
  )
}

make_plot_dots <- function(...) {
  c_dedupl(list(...))
}


# lines() -----------------------------------------------------------------
#' @rdname methods-plot
#' @export
lines.p <- function(x, n_extra_grid = 1001, ...) {
  assert_pdqr_fun(x)
  assert_type(
    n_extra_grid, is_single_number, type_name = "single number",
    allow_null = TRUE
  )

  if (meta_type(x) == "discrete") {
    add_p_dis_segments(x, list(...))
  } else {
    lines_impl_pdq(x, compute_plot_grid(x, n_extra_grid), list(...))
  }
}

#' @rdname methods-plot
#' @export
lines.d <- function(x, n_extra_grid = 1001, ...) {
  assert_pdqr_fun(x)
  assert_type(
    n_extra_grid, is_single_number, type_name = "single number",
    allow_null = TRUE
  )

  if (meta_type(x) == "discrete") {
    x_tbl <- meta_x_tbl(x)
    lines_args <- c_dedupl(
      list(x = x_tbl[["x"]], y = x_tbl[["prob"]], type = "h"), list(...)
    )

    do.call(graphics::lines, lines_args)
  } else {
    lines_impl_pdq(x, compute_plot_grid(x, n_extra_grid), list(...))
  }
}

#' @rdname methods-plot
#' @export
lines.q <- function(x, n_extra_grid = 1001, ...) {
  assert_pdqr_fun(x)
  assert_type(
    n_extra_grid, is_single_number, type_name = "single number",
    allow_null = TRUE
  )

  if (meta_type(x) == "discrete") {
    add_q_dis_segments(x, list(...))
  } else {
    lines_impl_pdq(x, compute_plot_grid(x, n_extra_grid), list(...))
  }
}

lines_impl_pdq <- function(f, grid, dots) {
  lines_args <- c_dedupl(list(x = grid, y = f(grid)), dots)

  do.call(graphics::lines, lines_args)
}
