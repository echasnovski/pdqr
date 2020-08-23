#' Work with regions
#'
#' These functions provide ways of working with a **region**: a data frame with
#' numeric "left" and "right" columns, each row of which represents a unique
#' finite interval (open, either type of half-open, or closed). Values of "left"
#' and "right" columns should create an "ordered" set of intervals:
#' `left[1] <= right[1] <= left[2] <= right[2] <= ...` (intervals with zero
#' width are accepted). Originally, `region_*()` functions were designed to work
#' with output of [summ_hdr()] and [summ_interval()], but can be used for any
#' data frame which satisfies the definition of a region.
#'
#' @param region A data frame representing region.
#' @param x Numeric vector to be tested for being inside region.
#' @param left_closed A single logical value representing whether to treat left
#'   ends of intervals as their parts.
#' @param right_closed A single logical value representing whether to treat
#'   right ends of intervals as their parts.
#' @param f A pdqr-function.
#' @param region2 A data frame representing region.
#' @param method Method for computing distance between regions in
#'   `region_distance()`. Should be one of "Jaccard" or methods of
#'   [summ_distance()].
#' @param col Single color of rectangles to be used. Should be appropriate for
#'   `col` argument of [col2rgb()][grDevices::col2rgb()].
#' @param alpha Single number representing factor modifying the opacity alpha;
#'   typically in \[0; 1\].
#'
#' @details `region_is_in()` tests each value of `x` for being inside interval.
#' In other words, if there is a row for which element of `x` is between "left"
#' and "right" value (respecting `left_closed` and `right_closed` options),
#' output for that element will be `TRUE`. **Note** that for zero-width
#' intervals one of `left_closed` or `right_closed` being `TRUE` is enough to
#' accept that point as "in region".
#'
#' `region_prob()` computes total probability of region according to
#' pdqr-function `f`. If `f` has "discrete" [type][meta_type()], output is
#' computed as sum of probabilities for all "x" values from ["x_tbl"
#' metadata][meta_x_tbl()] which lie inside a region (respecting `left_closed`
#' and `right_closed` options while using `region_is_in()`). If `f` has
#' "continuous" type, output is computed as integral of density over a region
#' (`*_closed` options having any effect).
#'
#' `region_height()` computes "height" of a region (with respect to `f`):
#' minimum value of corresponding to `f` d-function can return based on relevant
#' points inside a region. If `f` has "discrete" type, those relevant points are
#' computed as "x" values from "x_tbl" metadata which lie inside a region (if
#' there are no such points, output is 0). If `f` has "continuous" type, the
#' whole intervals are used as relevant points. The notion of "height" comes
#' from [summ_hdr()] function: if `region` is `summ_hdr(f, level)` for some
#' `level`, then `region_height(region, f)` is what is called in `summ_hdr()`'s
#' docs as "target height" of HDR. That is, a maximum value of d-function for
#' which a set consisting from points at which d-function has values not less
#' than target height and total probability of the set being not less than
#' `level`.
#'
#' `region_width()` computes total width of a region, i.e. sum of differences
#' between "right" and "left" columns.
#'
#' `region_distance()` computes distance between a pair of regions. As in
#' [summ_distance()], it is a single non-negative number representing how much
#' two regions differ from one another (bigger values indicate bigger
#' difference). Argument `method` represents method of computing distance.
#' Method "Jaccard" computes Jaccard distance: one minus ratio of intersection
#' width and union width. Other methods come from `summ_distance()` and
#' represent distance between regions as probability distributions:
#' - If total width of region is zero (i.e. it consists only from points),
#' distribution is a uniform discrete one based on points from region.
#' - If total width is positive, then distribution is a uniform continuous one
#' based on intervals with positive width.
#'
#' `region_draw()` draws (on current plot) intervals stored in `region` as
#' colored rectangles vertically starting from zero and ending in the top of the
#' plot (technically, at "y" value of `2e8`).
#'
#' @return `region_is_in()` returns a logical vector (with length equal to
#' length of `x`) representing whether certain element of `x` is inside a
#' region.
#'
#' `region_prob()` returns a single number between 0 and 1 representing total
#' probability of region.
#'
#' `region_height()` returns a single number representing a height of a region
#' with respect to `f`, i.e. minimum value that corresponding d-function can
#' return based on relevant points inside a region.
#'
#' `region_width()` returns a single number representing total width of a
#' region.
#'
#' `region_draw()` draws colored rectangles filling `region` intervals.
#'
#' @seealso [summ_hdr()] for computing of Highest Density Region.
#'
#' [summ_interval()] for computing of single interval summary of distribution.
#'
#' @examples
#' # Type "discrete"
#' d_binom <- as_d(dbinom, size = 10, prob = 0.7)
#' hdr_dis <- summ_hdr(d_binom, level = 0.6)
#' region_is_in(hdr_dis, 0:10)
#' ## This should be not less than 0.6
#' region_prob(hdr_dis, d_binom)
#' region_height(hdr_dis, d_binom)
#' region_width(hdr_dis)
#'
#' # Type "continuous"
#' d_norm <- as_d(dnorm)
#' hdr_con <- summ_hdr(d_norm, level = 0.95)
#' region_is_in(hdr_con, c(-Inf, -2, 0, 2, Inf))
#' ## This should be approximately equal to 0.95
#' region_prob(hdr_con, d_norm)
#' ## This should be equal to `d_norm(hdr_con[["left"]][1])`
#' region_height(hdr_con, d_norm)
#' region_width(hdr_con)
#'
#' # Usage of `*_closed` options
#' region <- data.frame(left = 1, right = 3)
#' ## Closed intervals
#' region_is_in(region, 1:3)
#' ## Open from left, closed from right
#' region_is_in(region, 1:3, left_closed = FALSE)
#' ## Closed from left, open from right
#' region_is_in(region, 1:3, right_closed = FALSE)
#' ## Open intervals
#' region_is_in(region, 1:3, left_closed = FALSE, right_closed = FALSE)
#'
#' # Handling of intervals with zero width
#' region <- data.frame(left = 1, right = 1)
#' ## If at least one of `*_closed` options is `TRUE`, 1 will be considered as
#' ## "in a region"
#' region_is_in(region, 1)
#' region_is_in(region, 1, left_closed = FALSE)
#' region_is_in(region, 1, right_closed = FALSE)
#' ## Only this will return `FALSE`
#' region_is_in(region, 1, left_closed = FALSE, right_closed = FALSE)
#'
#' # Distance between regions
#' region1 <- data.frame(left = c(0, 2), right = c(1, 2))
#' region2 <- data.frame(left = 0.5, right = 1.5)
#' region_distance(region1, region2, method = "Jaccard")
#' region_distance(region1, region2, method = "KS")
#'
#' # Drawing
#' d_mix <- form_mix(list(as_d(dnorm), as_d(dnorm, mean = 5)))
#' plot(d_mix)
#' region_draw(summ_hdr(d_mix, 0.95))
#' @name region
NULL

#' @rdname region
#' @export
region_is_in <- function(region, x, left_closed = TRUE, right_closed = TRUE) {
  assert_region(region)
  assert_missing(x, "numeric vector")
  assert_type(x, is.numeric)
  assert_type(left_closed, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(right_closed, is_truefalse, "`TRUE` or `FALSE`")

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  # Using `findInterval()` is safe because `region` should represent ordered
  # distinct intervals
  left_ind <- findInterval(x, region[["left"]], left.open = !left_closed)
  right_ind <- findInterval(x, region[["right"]], left.open = right_closed)

  # Inside intervals left index should be bigger than right by 1 because `x`
  # element should be more than left and less than right.
  is_inside <- left_ind == (right_ind + 1)

  # There are corner cases when consecutive intervals have common edge and `x`
  # has element equal to that edge. For example, for region [1; 2], [2; 3] and
  # `x` being 2 `left_ind` is 2 and `right_ind` is 0.
  is_in_left <- left_closed & (x %in% region[["left"]])
  is_in_right <- right_closed & (x %in% region[["right"]])

  is_inside | is_in_left | is_in_right
}

#' @rdname region
#' @export
region_prob <- function(region, f, left_closed = TRUE, right_closed = TRUE) {
  assert_region(region)
  assert_pdqr_fun(f)
  assert_type(left_closed, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(right_closed, is_truefalse, "`TRUE` or `FALSE`")

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  if (meta_type(f) == "discrete") {
    x_tbl <- meta_x_tbl(f)

    x_is_in_region <- region_is_in(
      region = region, x = x_tbl[["x"]],
      left_closed = left_closed, right_closed = right_closed
    )

    sum(x_tbl[["prob"]][x_is_in_region])
  } else {
    p_f <- as_p(f)

    # Formally, this returns probability of (left, right], which is equal to
    # probability of all other three edge configurations
    sum(p_f(region[["right"]]) - p_f(region[["left"]]))
  }
}

#' @rdname region
#' @export
region_height <- function(region, f, left_closed = TRUE, right_closed = TRUE) {
  assert_region(region)
  assert_pdqr_fun(f)
  assert_type(left_closed, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(right_closed, is_truefalse, "`TRUE` or `FALSE`")

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  x <- meta_x_tbl(f)[["x"]]
  x_is_in_region <- region_is_in(
    region, x, left_closed = left_closed, right_closed = right_closed
  )

  # `x_probe` contains all points of interest on which minimum
  # probability/density inside region might occure
  if (meta_type(f) == "discrete") {
    x_probe <- x[x_is_in_region]
  } else {
    x_probe <- c(region[["left"]], region[["right"]], x[x_is_in_region])
  }

  if (length(x_probe) == 0) {
    return(0)
  } else {
    d_vec <- as_d(f)(x_probe)

    min(d_vec)
  }
}

#' @rdname region
#' @export
region_width <- function(region) {
  assert_region(region)

  sum(region[["right"]] - region[["left"]])
}

#' @rdname region
#' @export
region_distance <- function(region, region2, method = "Jaccard") {
  assert_region(region)
  assert_region(region2)
  assert_method(method, c("Jaccard", methods_distance))

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  switch(
    method,
    Jaccard = region_distance_jaccard(region, region2),
    # Method "avgdist" is implemented separately for accuracy and performance
    avgdist = region_distance_avgdist(region, region2),
    # Method "entropy" is implemented separately to throw informative error
    entropy = {
      region_pdqr <- region_as_pdqr(region)
      region2_pdqr <- region_as_pdqr(region2)
      if (meta_type(region_pdqr) != meta_type(region2_pdqr)) {
        stop_collapse(
          'For method "entropy", regions should represent distribution of the ',
          "same type (either both have zero width or both have positive width)."
        )
      }

      summ_distance(region_pdqr, region2_pdqr, method)
    },
    summ_distance(region_as_pdqr(region), region_as_pdqr(region2), method)
  )
}

region_distance_jaccard <- function(region, region2) {
  # Compute grid of all possible breaks from either `region` or `region2`
  breaks <- sort(unique(c(unlist(region), unlist(region2))))
  names(breaks) <- NULL
  l <- breaks[-length(breaks)]
  r <- breaks[-1]
  mids <- (l + r) / 2
  lens <- r - l

  # Determine if intervals of grid are in input regions
  interval_is_in_1 <- region_is_in(region, mids)
  interval_is_in_2 <- region_is_in(region2, mids)

  # Determine if intervals of grid are in intersection and union of regions
  interval_is_in_intercept <- interval_is_in_1 & interval_is_in_2
  interval_is_in_union     <- interval_is_in_1 | interval_is_in_2

  # Compute result based on fact that length of intersection/union is equal to
  # sum of lengths of grid intervals that are their part
  1 - sum(lens[interval_is_in_intercept]) / sum(lens[interval_is_in_union])
}

region_distance_avgdist <- function(region, region2) {
  left <- region[["left"]]
  right <- region[["right"]]
  left2 <- region2[["left"]]
  right2 <- region2[["right"]]

  # For every interval in `region` compute its distance to `region2`
  interval_region2 <- vapply(seq_len(nrow(region)), function(i) {
    # For every interval in `region2` compute its distance to current interval
    # in `region`
    interval_interval2 <- vapply(seq_len(nrow(region2)), function(j) {
      mean_unif_dist(left[i], right[i], left2[j], right2[j])
    }, numeric(1))

    # Distance from interval to `region2` is a weighted sum of
    # "interval-interval" distances. This aligns with treating regions as
    # uniform distributions on them. **Note** that it means zero length
    # intervals are not used if there is at least one interval with positive
    # length.
    sum(interval_interval2 * to_weights(right2 - left2))
  }, numeric(1))

  # "region-region" distance is a weighted sum of "interval-region" distances
  sum(interval_region2 * to_weights(right - left))
}

#' @rdname region
#' @export
region_draw <- function(region, col = "blue", alpha = 0.2) {
  assert_region(region)
  assert_type(col, is_single_color, "single color")
  assert_type(alpha, is_single_number, "single number")

  # This function doesn't use more convinient `rect()` because there were
  # problems with what seemed like overflow: when setting too big `ytop`
  # rectangles were drawn downward and not upward.
  n <- nrow(region)
  x <- numeric(5 * n)
  y <- numeric(5 * n)
  rect_id_start <- 5 * seq_len(n) - 4

  x[rect_id_start] <- region[["left"]]
  x[rect_id_start + 1] <- region[["left"]]
  x[rect_id_start + 2] <- region[["right"]]
  x[rect_id_start + 3] <- region[["right"]]
  x[rect_id_start + 4] <- NA

  y[rect_id_start] <- 0
  y[rect_id_start + 1] <- 2e8
  y[rect_id_start + 2] <- 2e8
  y[rect_id_start + 3] <- 0
  y[rect_id_start + 4] <- NA

  graphics::polygon(
    x, y, border = NA, col = grDevices::adjustcolor(col, alpha.f = alpha)
  )
}


# Helpers -----------------------------------------------------------------
region_new <- function(left, right) {
  output_region <- data.frame(left = left, right = right)
  assert_region(output_region)

  output_region
}

assert_region <- function(df) {
  if (dont_assert()) {
    return(TRUE)
  }

  df_name <- enbacktick(deparse(substitute(df)))
  start_msg <- paste0(df_name, " is not a region. ")

  if (missing(df)) {
    error_missing(df_name, "region data frame")
  }

  if (!is.data.frame(df)) {
    stop_collapse(start_msg, "It should be a data frame.")
  }
  if (!(
    ("left" %in% names(df)) && is.numeric(df[["left"]]) &&
      all(is.finite(df[["left"]]))
  )) {
    stop_collapse(
      start_msg, 'It should have numeric column "left" with finite values.'
    )
  }
  if (!(
    ("right" %in% names(df)) && is.numeric(df[["right"]]) &&
      all(is.finite(df[["right"]]))
  )) {
    stop_collapse(
      start_msg, 'It should have numeric column "right" with finite values.'
    )
  }
  if (!all(df[["right"]] >= df[["left"]])) {
    stop_collapse(
      start_msg, 'All elements of column "right" should be not less than ',
      'corresponding elements from column "left".'
    )
  }
  if (!is_region_ordered(df)) {
    stop_collapse(
      start_msg, 'Columns "left" and "right" should represent ordered set of ',
      "distinct intervals: left[1] <= right[1] <= left[2] <= rihgt[2] <= ..., ",
      "and there should not be duplicated intervals."
    )
  }

  TRUE
}

is_region_ordered <- function(df) {
  # Values in "left" (`l`) and "right" (`r`) column should be ordered
  # (weakly) increasingly as: l[1] <= r[1] <= l[2] <= r[2] <= ... <= r[nrow(df)]
  comb <- alternate(df[["left"]], df[["right"]])

  # Order condition
  all(order(comb) == seq_len(2 * nrow(df))) &&
    # Uniqueness condition
    !any(duplicated(df[, c("left", "right")]))
}

# Region is transformed into a random variable based on the following algorithm:
# - If all intervals have "zero" lengths (based on 1e-10 tolerance), output is a
#   uniform discrete discrete distribution over their values (left edges).
# - If there is at least one interval with positive length, output is a uniform
#   continuous distribution over the region. This means that intervals with zero
#   length don't affect output, because their probability with respect to output
#   random variable is zero.
region_as_pdqr <- function(region) {
  interval_is_point <- is_zero(region[["right"]] - region[["left"]])

  if (all(interval_is_point)) {
    res <- new_d(region[["left"]], "discrete")
  } else {
    region_con <- region[!interval_is_point, , drop = FALSE]

    interval_width <- region_con[["right"]] - region_con[["left"]]
    mix_weights <- to_weights(interval_width)

    f_list <- lapply(seq_len(nrow(region_con)), function(i) {
      new_d(
        x = data.frame(
          x = c(region_con[["left"]][i], region_con[["right"]][i]),
          y = c(1, 1)
        ),
        type = "continuous"
      )
    })

    res <- form_mix(f_list, weights = mix_weights)
  }

  res
}
