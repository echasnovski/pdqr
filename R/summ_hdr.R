#' Summarize distribution with Highest Density Region
#'
#' `summ_hdr()` computes a Highest Density Region (HDR) of some pdqr-function
#' for a supplied `level`: a union of (closed) intervals total probability of
#' which is not less than `level` and probability/density at any point inside it
#' is bigger than some threshold (which should be maximum one with a property
#' of HDR having total probability not less than `level`). This also represents
#' a set of intervals with the lowest total width among all sets with total
#' probability not less than a `level`.
#'
#' @inheritParams summ_mean
#' @param level A desired lower bound for a total probability of an output set
#'   of intervals.
#'
#' @details General algorithm of `summ_hdr()` consists from two steps:
#' 1. **Find "target height"**. That is a value of probability/density which
#' divides all [support][meta_support()] into two sets: the one with
#' probability/density not less than target height (it is a desired HDR) and the
#' other - with strictly less. The first set should also have total probability
#' not less than `level`.
#' 1. **Form a HDR as a set of closed intervals**.
#'
#' If `f` has "discrete" type, target height is computed by looking at "x"
#' values of ["x_tbl" metadata][meta_x_tbl()] in order of decreasing probability
#' until their total probability is not less than `level`. After that, all "x"
#' values with probability not less than height are considered to form a HDR.
#' Output is formed as a set of **closed** intervals (i.e. both edges included)
#' inside of which lie all HDR "x" elements and others - don't.
#'
#' If `f` has "continuous" type, target height is estimated as `1-level`
#' quantile of `Y = d_f(X)` distribution, where `d_f` is d-function
#' corresponding to `f` ([`as_d(f)`][as_d()] in other words) and `X` is a random
#' variable represented by `f`. Essentially, `Y` has a distribution of `f`'s
#' density values and its `1-level` quantile is a target height. After that, HDR
#' is formed as a set of intervals **with positive width** (if `level` is more
#' than 0, see Notes) inside which density is not less than target height.
#'
#' **Notes**:
#' - If `level` is 0, output has one interval of zero width at point of [global
#' mode][summ_mode()].
#' - If `level` is 1, output has one interval equal to support.
#' - Computation of target height in case of "continuous" type is approximate
#' which in some extreme cases (for example, like [winsorized][form_tails()]
#' distributions) can lead to HDR having total probability very approximate to
#' and even slightly lower than `level`.
#' - If d-function has "plateaus" (consecutive values with equal
#' probability/density) at computed target height, total probability of HDR can
#' be considerably bigger than `level` (see examples). However, this aligns with
#' HDR definition, as density values should be **not less** than target height
#' and total probability should be **not less** than `level`.
#'
#' @return A data frame with one row representing one closed interval of HDR and
#' the following columns:
#' - **left** `<dbl>` : Left end of intervals.
#' - **right** `<dbl>` : Right end of intervals.
#'
#' @seealso [`region_*()`][region] family of functions for working with output
#'   HDR.
#'
#' [summ_interval()] for computing of single interval summary of distribution.
#'
#' @family summary functions
#'
#' @examples
#' # "discrete" functions
#' d_dis <- new_d(data.frame(x = 1:4, prob = c(0.4, 0.2, 0.3, 0.1)), "discrete")
#' summ_hdr(d_dis, 0.3)
#' summ_hdr(d_dis, 0.5)
#' summ_hdr(d_dis, 0.9)
#' ## Zero width interval at global mode
#' summ_hdr(d_dis, 0)
#'
#' # "continuous" functions
#' d_norm <- as_d(dnorm)
#' summ_hdr(d_norm, 0.95)
#' ## Zero width interval at global mode
#' summ_hdr(d_norm, 0)
#'
#' # Works well with mixture distributions
#' d_mix <- form_mix(list(as_d(dnorm), as_d(dnorm, mean = 5)))
#' summ_hdr(d_mix, 0.95)
#'
#' # Plateaus
#' d_unif <- as_d(dunif)
#' ## Returns all support because of density "plateau"
#' summ_hdr(d_unif, 0.1)
#'
#' # Draw HDR
#' plot(d_mix)
#' region_draw(summ_hdr(d_mix, 0.95))
#' @export
summ_hdr <- function(f, level = 0.95) {
  assert_pdqr_fun(f)
  assert_type(
    level, is_single_number,
    type_name = "single number between 0 and 1",
    min_val = 0, max_val = 1
  )

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  # Early returns for special `level`s
  if (level == 0) {
    f_mode <- summ_mode(f, method = "global")

    return(data.frame(left = f_mode, right = f_mode))
  }
  if (level == 1) {
    f_supp <- meta_support(f)

    return(data.frame(left = f_supp[1], right = f_supp[2]))
  }

  switch(
    meta_type(f),
    discrete = hdr_dis(f, level),
    continuous = hdr_con(f, level)
  )
}

hdr_dis <- function(f, level) {
  x_tbl <- meta_x_tbl(f)
  x <- x_tbl[["x"]]
  prob <- x_tbl[["prob"]]
  prob_order <- order(prob, decreasing = TRUE)

  # Compute index of ordered decreacingly `x` at which total probability exceeds
  # `level`.
  height_ind <- which(cumsum(prob[prob_order]) >= level)[1]
  prob_is_high <- prob >= prob[prob_order][height_ind]
  high_inds <- which(prob_is_high)
  x_high <- x[prob_is_high]

  # Squash `high_inds` into intervals of consecutive indicies
  ind_is_jump <- (high_inds[-1] - high_inds[-length(high_inds)]) != 1
  is_interval_left <- c(TRUE, ind_is_jump)
  is_interval_right <- c(ind_is_jump, TRUE)

  region_new(left = x_high[is_interval_left], right = x_high[is_interval_right])
}

hdr_con <- function(f, level) {
  # "Cut down" `y` values inside very narrow intervals. Otherwise they might
  # greatly skew computation of target height. This happen in winsorized
  # pdqr-functions.
  cut_dd <- cut_down_density(f, level)

  # Compute height which gives desired HDR
  target_height <- compute_target_height(cut_dd[["x_tbl"]], cut_dd[["level"]])

  # Computation of intervals should be made with original `f`, and not with its
  # "cut down" version
  compute_hdr_intervals(f, target_height)
}

cut_down_density <- function(f, level) {
  # This computes `y` range excluding those values inside "very narrow"
  # intervals, which are (very probably) dirac-like values.
  y_lim <- compute_d_con_ylim(f)
  x_tbl <- meta_x_tbl(f)
  y <- x_tbl[["y"]]
  y_cut_down <- pmin(y, y_lim[2])

  # Proceed based upon how much of probability might be cut out when `y` values
  # are "cut down" to range computed earlier.
  exclude_prob <- 1 - trapez_integral(x_tbl[["x"]], y_cut_down)
  if (exclude_prob > level) {
    # If possible cut is bigger than `level`, act only on "very narrow"
    # intervals, because they contain the needed HDR
    x_tbl[["y"]][y <= y_lim[2]] <- 0
  } else {
    # If possible cut is smaller than `level`, act on the rest of intervals
    # while adjusting target `level`
    x_tbl[["y"]] <- y_cut_down
    level <- (level - exclude_prob) / (1 - exclude_prob)
  }

  list(x_tbl = x_tbl[, c("x", "y")], level = level)
}

compute_target_height <- function(x_tbl, level) {
  # Target height for HDR with level `level` is computed as `1-level` quantile
  # of random variable `Y = d_f(X)` (`d_f` - probability density of input random
  # variable, `X` - input random variable represented by pdqr-function `f`).
  # `Y`s CDF is approximated by computing density values `y` at certain grid and
  # assuming that density of `Y` at point `y` is proportional to `y` (because it
  # is a value of `X`'s density, i.e. transformation function). Grid is created
  # to be a union of all `x` values from `f`'s "x_tbl" and other `x`s
  # corresponding to non-zero `f`'s density. Zero probability intervals are not
  # interesting in case of transformation of random variable (also using them
  # introduces errors if they obtain much of the support).
  # Another way of approximate this quantile is to use simulation, but it is
  # a random approach which is not desirable:
  #   smpl <- as_r(f)(n_sample)
  #   quantile(as_d(f)(smpl), probs = 1 - level)

  d_f <- enfun_x_tbl(x_tbl)

  # Here `unique()` is used to not "over-sample" `y` values
  x_vec <- unique(c(x_tbl[["x"]], grid_on_pos_prob(x_tbl, n_grid = 10001)))
  y_vec <- sort(d_f(x_vec))
  quan_ind <- findInterval(
    1 - level, cumsum(y_vec) / sum(y_vec),
    # `all.inside = TRUE` is needed to ensure that too small values of `1-level`
    # won't cause `quan_ind` equal to 0
    all.inside = TRUE
  )

  y_vec[quan_ind]
}

grid_on_pos_prob <- function(x_tbl, n_grid) {
  n <- nrow(x_tbl)
  x <- x_tbl[["x"]]
  x_l <- x[-n]
  x_r <- x[-1]
  y <- x_tbl[["y"]]

  pos_prob <- !((y[-n] == 0) & (y[-1] == 0))
  x_l <- x_l[pos_prob]
  x_r <- x_r[pos_prob]

  # Treat all intervals with positive probability as one consecutive
  # ("squashed") interval and produce sequence with desired number of elements
  width_vec <- cumsum(x_r - x_l)
  res_width <- seq(0, width_vec[length(width_vec)], length.out = n_grid)
  inter_ind <- findInterval(res_width, width_vec, rightmost.closed = TRUE) + 1

  x_r[inter_ind] - (width_vec[inter_ind] - res_width)
}

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
  intervals_center <- 0.5 * (x_height[-1] + x_height[-n])
  interval_is_high <- as_d(f)(intervals_center) >= height
  high_inds <- which(interval_is_high)
  x_l <- x_height[high_inds]
  x_r <- x_height[high_inds + 1]

  # Collapsing consecutive interval edges
  is_consec <- x_r[-length(x_r)] == x_l[-1]
  x_l[c(FALSE, is_consec)] <- NA
  x_r[c(is_consec, FALSE)] <- NA

  region_new(left = x_l[!is.na(x_l)], right = x_r[!is.na(x_r)])
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
