# plot() ------------------------------------------------------------------
plot.p_fun <- function(x, y = NULL, n_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  dots <- make_plot_dots(
    ...,
    type = "l", main = paste0("Cumulative distribution function ", x_name),
    xlab = "x", ylab = "Cumulative probability"
  )

  # Stretch support to guarantee 0 and 1 on edges
  plot_impl_pdq(x, stretch_range(meta(x, "support")), n_grid, dots)
}

plot.d_fun <- function(x, y = NULL, n_grid = 1001, ...) {
  x_name <- deparse(substitute(x))

  warn_plotting_raw_d_fun(x)

  dots <- make_plot_dots(
    ...,
    type = "l", main = paste0("Density function ", x_name),
    xlab = "x", ylab = "Density"
  )

  plot_impl_pdq(x, meta(x, "support"), n_grid, dots)
}

plot.q_fun <- function(x, y = NULL, n_grid = 1001, ...) {
  x_name <- deparse(substitute(x))
  dots <- make_plot_dots(
    ...,
    type = "l", main = paste0("Quantile function ", x_name),
    xlab = "Cumulative probability", ylab = "x"
  )

  plot_impl_pdq(x, c(0, 1), n_grid, dots)
}

plot.r_fun <- function(x, y = NULL, n_sample = 1001, ...) {
  x_name <- deparse(substitute(x))

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

make_plot_dots <- function(...) {
  dedupl_list(list(...))
}

warn_plotting_raw_d_fun <- function(f) {
  if (inherits(f, "d_fun") && (meta(f, "type") == "raw")) {
    warning_collapse(
      'Plotting density function with `type` = "raw" can miss actual points.'
    )
  }

  f
}


# lines() -----------------------------------------------------------------
lines.p_fun <- function(x, n_grid = 1001, ...) {
  # Stretch support to guarantee 0 and 1 on edges
  lines_impl_pdq(x, stretch_range(meta(x, "support")), n_grid, list(...))
}

lines.d_fun <- function(x, n_grid = 1001, ...) {
  warn_plotting_raw_d_fun(x)

  lines_impl_pdq(x, meta(x, "support"), n_grid, list(...))
}

lines.q_fun <- function(x, n_grid = 1001, ...) {
  lines_impl_pdq(x, c(0, 1), n_grid, list(...))
}

lines_impl_pdq <- function(f, grid_range, n_grid, dots) {
  grid <- seq(grid_range[1], grid_range[2], length.out = n_grid)
  lines_args <- dedupl_list(c(list(x = grid, y = f(grid)), dots))

  do.call(graphics::lines, lines_args)
}
