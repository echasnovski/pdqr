# plot() ------------------------------------------------------------------
plot.p <- function(x, y = NULL, n_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)

  dots <- make_plot_dots(
    ...,
    type = "l", main = paste0("Cumulative distribution function ", x_name),
    xlab = "x", ylab = "Cumulative probability"
  )

  if (meta_type(x) == "fin") {
    # Create canvas
    no_plot_dots <- dedupl_list(c(
      list(x = meta_support(x), y = c(0, 1), type = "n"),
      dots
    ))

    do.call(graphics::plot, no_plot_dots)

    # Add segments
    add_p_fin_segments(x, list(...))
  } else {
    # Stretch support to guarantee 0 and 1 on edges
    plot_impl_pdq(x, stretch_range(meta_support(x)), n_grid, dots)
  }
}

plot.d <- function(x, y = NULL, n_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  assert_pdqr_fun(x)

  if (meta_type(x) == "fin") {
    x_tbl <- meta_x_tbl(x)

    dots <- make_plot_dots(
      ...,
      type = "h", main = paste0("Probability mass function ", x_name),
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
      xlab = "x", ylab = "Density",
      # This is needed for a nice plotting in presence of dirac-like entries in
      # "x_tbl". Note, that this will compute `ylim` based on "x_tbl" in `x` and
      # not based on actually plotted points, so the output `ylim` can be an
      # overestimation (which is kind of a good indication of too small value
      # of `n_grid`).
      ylim = compute_d_infin_ylim(x)
    )

    plot_impl_pdq(x, meta_support(x), n_grid, dots)
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

  if (meta_type(x) == "fin") {
    # Create canvas
    no_plot_dots <- dedupl_list(c(
      list(x = c(0, 1), y = meta_support(x), type = "n"),
      dots
    ))

    do.call(graphics::plot, no_plot_dots)

    # Add segments
    add_q_fin_segments(x, list(...))
  } else {
    plot_impl_pdq(x, c(0, 1), n_grid, dots)
  }
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

compute_d_infin_ylim <- function(f) {
  # This is an euristic for a nice plotting in presence of dirac-like entries in
  # "x_tbl"
  f_x_tbl <- meta_x_tbl(f)
  x <- f_x_tbl[["x"]]
  y <- f_x_tbl[["y"]]

  x_neigh_dist <- neigh_dist(x, neigh_type = "max")

  # Here `2e-8` is used instead of default `1e-8` to account for possible
  # numerical representation inaccuracies.
  y_non_dirac <- y[x_neigh_dist >= 2e-8]
  if (max(y_non_dirac) == 0) {
    # The case when all entries seems to be dirac-like. So all points should be
    # used in computing `ylim`.
    range(y)
  } else {
    # The case when only some entries are dirac-like and the respective "y"s
    # should be removed.
    range(y_non_dirac)
  }
}

add_p_fin_segments <- function(x, dots) {
  plot_dots <- compute_p_fin_dots(x, dots)

  do.call(graphics::segments, plot_dots[["seg_ver"]])
  do.call(graphics::segments, plot_dots[["seg_hor"]])
  do.call(graphics::points, plot_dots[["points"]])
}

add_q_fin_segments <- function(x, dots) {
  plot_p_dots <- compute_p_fin_dots(x, dots)

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

compute_p_fin_dots <- function(x, dots) {
  x_tbl <- meta_x_tbl(x)
  x <- x_tbl[["x"]]
  cumprob <- x_tbl[["cumprob"]]
  n <- nrow(x_tbl)

  # Remove supplied `type` argument as it will not work and throw warnings
  dots[["type"]] <- NULL

  vertical_segments_dots <- dedupl_list(c(
    dots,
    list(x0 = x, y0 = c(0, cumprob[-n]), x1 = x, y1 = cumprob, lty = 2)
  ))

  horizontal_segments_dots <- dedupl_list(c(
    dots,
    list(x0 = x[-n], y0 = cumprob[-n], x1 = x[-1], y1 = cumprob[-n])
  ))

  points_dots <- dedupl_list(c(dots, list(x = x, y = cumprob, pch = 16)))

  list(
    seg_ver = vertical_segments_dots,
    seg_hor = horizontal_segments_dots,
    points = points_dots
  )
}

make_plot_dots <- function(...) {
  dedupl_list(list(...))
}


# lines() -----------------------------------------------------------------
lines.p <- function(x, n_grid = 1001, ...) {
  assert_pdqr_fun(x)

  if (meta_type(x) == "fin") {
    add_p_fin_segments(x, list(...))
  } else {
    # Stretch support to guarantee 0 and 1 on edges
    lines_impl_pdq(x, stretch_range(meta_support(x)), n_grid, list(...))
  }
}

lines.d <- function(x, n_grid = 1001, ...) {
  assert_pdqr_fun(x)

  if (meta_type(x) == "fin") {
    x_tbl <- meta_x_tbl(x)
    lines_args <- dedupl_list(
      c(list(x = x_tbl[["x"]], y = x_tbl[["prob"]], type = "h"), list(...))
    )

    do.call(graphics::lines, lines_args)
  } else {
    lines_impl_pdq(x, meta_support(x), n_grid, list(...))
  }
}

lines.q <- function(x, n_grid = 1001, ...) {
  assert_pdqr_fun(x)

  if (meta_type(x) == "fin") {
    add_q_fin_segments(x, list(...))
  } else {
    lines_impl_pdq(x, c(0, 1), n_grid, list(...))
  }
}

lines_impl_pdq <- function(f, grid_range, n_grid, dots) {
  grid <- seq(grid_range[1], grid_range[2], length.out = n_grid)
  lines_args <- dedupl_list(c(list(x = grid, y = f(grid)), dots))

  do.call(graphics::lines, lines_args)
}
