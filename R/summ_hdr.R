compute_density_height_points <- function(f, height) {
  x_tbl <- meta_x_tbl(f)
  x <- x_tbl[["x"]]
  y <- x_tbl[["y"]]

  if ((height < min(y)) || (height > max(y))) {
    return(numeric(0))
  }

  n <- length(y)
  x_l <- x[-n]
  x_r <- x[-1]
  y_l <- y[-n]
  y_r <- y[-1]

  is_high <- ((y_l <= height) & (height <= y_r)) |
    ((y_l >= height) & (height >= y_r))

  # Compute `x` in which density crosses `height`. If some interval's `y_r` and
  # `y_l` are the same, it means that whole interval is a plateau at height
  # `height`. In that case `lambda` will be `NaN` (as output of `0/0`) and this
  # interval will not contribute to output height points. It is done to return
  # only edges of plateaus, which will be returns from "not plateau" intervals
  # and/or from manual inspection of `f`'s support edges.
  lambda <- abs(height - y_l[is_high]) / abs(y_r[is_high] - y_l[is_high])
  res <- (1 - lambda) * x_l[is_high] + lambda * x_r[is_high]
  res <- res[!is.na(res)]

  # Manually check `f`'s support edges
  if (y[1] == height) {
    res <- c(x[1], res)
  }
  if (y[n] == height) {
    res <- c(res, x[n])
  }

  # `unique()` is needed because otherwise if some `x` from `x_tbl` is a desired
  # height point, it will be returned twice.
  unique(res)
}
