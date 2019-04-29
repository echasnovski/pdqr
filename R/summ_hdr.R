compute_hdr_intervals <- function(f, height) {
  f_supp <- meta_support(f)
  # `x_height` has edges of intervals **with positive width** inside which
  # `as_d(f)` is either `>= height` or `<= height`. Here `unique()` removes
  # possible zero width intervals.
  x_height <- unique(
    c(f_supp[1], compute_density_height_points(f, height), f_supp[2])
  )
  n <- length(x_height)

  # Determining which intervals are `>= height`
  intervals_center <- 0.5*(x_height[-1] + x_height[-n])
  interval_is_high <- f(intervals_center) >= height
  high_inds <- which(interval_is_high)
  x_l <- x_height[high_inds]
  x_r <- x_height[high_inds + 1]

  # Collapsing consecutive interval edges
  is_consec <- x_r[-length(x_r)] == x_l[-1]
  x_l[c(FALSE, is_consec)] <- NA
  x_r[c(is_consec, FALSE)] <- NA

  data.frame(left = x_l[!is.na(x_l)], right = x_r[!is.na(x_r)])
}

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
