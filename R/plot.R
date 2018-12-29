# plot() ------------------------------------------------------------------
plot.p <- function(x, y = NULL, n_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)

  dots <- make_plot_dots(
    ...,
    type = "l", main = paste0("Cumulative distribution function ", x_name),
    xlab = "x", ylab = "Cumulative probability"
  )

  if (meta(x, "type") == "raw") {
    # Create canvas
    no_plot_dots <- dedupl_list(c(
      list(x = meta(x, "support"), y = c(0, 1), type = "n"),
      dots
    ))

    do.call(graphics::plot, no_plot_dots)

    # Add segments
    add_p_raw_segments(x, list(...))
  } else {
    # Stretch support to guarantee 0 and 1 on edges
    plot_impl_pdq(x, stretch_range(meta(x, "support")), n_grid, dots)
  }
}

plot.d <- function(x, y = NULL, n_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)

  if (meta(x, "type") == "raw") {
    x_tbl <- meta(x, "x_tbl")

    dots <- make_plot_dots(
      ...,
      type = "h", main = paste0("Density function ", x_name),
      xlab = "x", ylab = "Probability", ylim = c(0, max(x_tbl[["prob"]]))
    )
    plot_args <- dedupl_list(
      c(list(x = x_tbl[["x"]], y = x_tbl[["prob"]]), dots)
    )

    do.call(graphics::plot, plot_args)
  } else {
    dots <- make_plot_dots(
      ...,
      type = "l", main = paste0("Density function ", x_name),
      xlab = "x", ylab = "Density"
    )

    plot_impl_pdq(x, meta(x, "support"), n_grid, dots)
  }
}

plot.q <- function(x, y = NULL, n_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)

  dots <- make_plot_dots(
    ...,
    type = "l", main = paste0("Quantile function ", x_name),
    xlab = "Cumulative probability", ylab = "x"
  )

  plot_impl_pdq(x, c(0, 1), n_grid, dots)
}

plot.r <- function(x, y = NULL, n_sample = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)

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

plot_impl_pdq <- function(f, grid_range, n_grid, dots) {
  grid <- seq(grid_range[1], grid_range[2], length.out = n_grid)
  plot_args <- dedupl_list(c(list(x = grid, y = f(grid)), dots))

  do.call(graphics::plot, plot_args)
}

add_p_raw_segments <- function(x, dots) {
  x_tbl <- meta(x, "x_tbl")
  x <- x_tbl[["x"]]
  cumprob <- x_tbl[["cumprob"]]
  n <- nrow(x_tbl)

  # Remove supplied `type` argument as it will not work and throw warnings
  dots[["type"]] <- NULL

  vertical_segments_dots <- dedupl_list(c(
    dots,
    list(x0 = x, y0 = c(0, cumprob[-n]), x1 = x, y1 = cumprob, lty = 2)
  ))
  do.call(graphics::segments, vertical_segments_dots)

  horizontal_segments_dots <- dedupl_list(c(
    dots,
    list(x0 = x[-n], y0 = cumprob[-n], x1 = x[-1], y1 = cumprob[-n])
  ))
  do.call(graphics::segments, horizontal_segments_dots)

  # Add points
  points_dots <- dedupl_list(c(dots, list(x = x, y = cumprob, pch = 16)))
  do.call(graphics::points, points_dots)
}

make_plot_dots <- function(...) {
  dedupl_list(list(...))
}


# lines() -----------------------------------------------------------------
lines.p <- function(x, n_grid = 1001, ...) {
  assert_pdqr_fun(x)

  if (meta(x, "type") == "raw") {
    add_p_raw_segments(x, list(...))
  } else {
    # Stretch support to guarantee 0 and 1 on edges
    lines_impl_pdq(x, stretch_range(meta(x, "support")), n_grid, list(...))
  }
}

lines.d <- function(x, n_grid = 1001, ...) {
  assert_pdqr_fun(x)

  if (meta(x, "type") == "raw") {
    x_tbl <- meta(x, "x_tbl")
    lines_args <- dedupl_list(
      c(list(x = x_tbl[["x"]], y = x_tbl[["prob"]], type = "h"), list(...))
    )

    do.call(graphics::lines, lines_args)
  } else {
    lines_impl_pdq(x, meta(x, "support"), n_grid, list(...))
  }
}

lines.q <- function(x, n_grid = 1001, ...) {
  assert_pdqr_fun(x)

  lines_impl_pdq(x, c(0, 1), n_grid, list(...))
}

lines_impl_pdq <- function(f, grid_range, n_grid, dots) {
  grid <- seq(grid_range[1], grid_range[2], length.out = n_grid)
  lines_args <- dedupl_list(c(list(x = grid, y = f(grid)), dots))

  do.call(graphics::lines, lines_args)
}
