#' Transform tails of distribution
#'
#' Modify tail(s) of distribution defined by certain cutoff level using method
#' of choice. This function is useful for doing robust analysis in presence of
#' possible outliers.
#'
#' @param f A pdqr-function.
#' @param level Cutoff level. For direction "both" should be between 0 and 0.5;
#'   for "left" and "right" - between 0 and 1.
#' @param method Modification method. One of "trim" or "winsor".
#' @param direction Information about which tail(s) to modify. One of "both",
#'   "left", "right".
#'
#' @details Edges for left and right tails are computed as `level` and `1 -
#' level` quantiles respectively. The left tail is interval to the left of
#' left edge, and right tail - to the right of right edge.
#'
#' Method "trim" removes tail(s) while normalizing "center part". Method
#' "winsor" "squashes" tails inside center of distribution in dirac-like
#' fashion, i.e. probability of tail(s) is moved inside and becomes concentrated
#' in `1e-8` neighborhood of nearest edge.
#'
#' Direction "both" affect both tails. Directions "left" and "right" affect only
#' left and right tail respectively.
#'
#' @return A pdqr-function with transformed tail(s).
#'
#' @seealso [form_resupport()] for changing [support][meta_support()] to some
#' known interval.
#'
#' [summ_center()] and [summ_spread()] for computing summaries of distributions.
#'
#' @family form functions
#'
#' @examples
#' # Type "discrete"
#' my_dis <- new_d(data.frame(x = 1:4, prob = (1:4)/10), type = "discrete")
#' meta_x_tbl(form_tails(my_dis, level = 0.1))
#' meta_x_tbl(
#'   form_tails(my_dis, level = 0.35, method = "winsor", direction = "left")
#' )
#'
#' # Type "continuous"
#' d_norm <- as_d(dnorm)
#' plot(d_norm)
#' lines(form_tails(d_norm, level = 0.1), col = "blue")
#' lines(
#'   form_tails(d_norm, level = 0.1, method = "winsor", direction = "right"),
#'   col = "green"
#' )
#'
#' # Use `form_resupport()` and `as_q()` to remove different levels from both
#' # directions. Here 0.1 level tail from left is removed, and 0.05 level from
#' # right
#' new_supp <- as_q(d_norm)(c(0.1, 1-0.05))
#' form_resupport(d_norm, support = new_supp)
#'
#' # Examples of robust mean
#' set.seed(101)
#' x <- rcauchy(1000)
#' d_x <- new_d(x, "continuous")
#' summ_mean(d_x)
#'   # Trimmed mean
#' summ_mean(form_tails(d_x, level = 0.1, method = "trim"))
#'   # Winsorized mean
#' summ_mean(form_tails(d_x, level = 0.1, method = "winsor"))
#'
#' @export
form_tails <- function(f, level, method = "trim", direction = "both") {
  assert_form_tails_args(f, level, method, direction)

  switch(
    method,
    trim = tails_trim(f, level, direction),
    winsor = tails_winsor(f, level, direction)
  )
}

tails_trim <- function(f, level, direction = "both") {
  is_all_trimmed <- ((direction %in% c("left", "right")) && (level == 1)) ||
    ((direction == "both") && (level == 0.5))
  if (is_all_trimmed) {
    return(trim_all(f, direction))
  }

  switch(
    meta_type(f),
    discrete = tails_trim_dis(f, level, direction),
    continuous = tails_trim_con(f, level, direction)
  )
}

tails_winsor <- function(f, level, direction = "both") {
  new_supp <- compute_support_after_remove(f, level, direction)

  form_resupport(f, new_supp, method = "winsor")
}

tails_trim_dis <- function(f, level, direction) {
  f_x_tbl <- meta_x_tbl(f)

  new_supp <- compute_support_after_remove(f, level, direction)

  if (direction %in% c("left", "both")) {
    x_is_to_remove <- f_x_tbl[["x"]] < new_supp[1]
    tot_p_to_remove <- sum(f_x_tbl[["prob"]][x_is_to_remove])
    f_x_tbl <- f_x_tbl[!x_is_to_remove, ]

    # Delete "underremoved" probability from left "x" value. If all probability
    # is removed, the whole row is removed.
    f_x_tbl <- decrease_row_prob(
      f_x_tbl, row = 1, by_prob = level - tot_p_to_remove
    )
  }

  if (direction %in% c("right", "both")) {
    x_is_to_remove <- f_x_tbl[["x"]] > new_supp[2]
    tot_p_to_remove <- sum(f_x_tbl[["prob"]][x_is_to_remove])
    f_x_tbl <- f_x_tbl[!x_is_to_remove, ]

    # Delete "underremoved" probability from right "x" value. If all probability
    # is removed, the whole row is removed.
    f_x_tbl <- decrease_row_prob(
      f_x_tbl, row = nrow(f_x_tbl), by_prob = level - tot_p_to_remove
    )
  }

  new_pdqr_by_ref(f)(f_x_tbl, "discrete")
}

tails_trim_con <- function(f, level, direction) {
  new_supp <- compute_support_after_remove(f, level, direction)

  form_resupport(f, new_supp, method = "trim")
}

assert_form_tails_args <- function(f, level, method, direction) {
  assert_pdqr_fun(f)
  assert_missing(level, "tail level to modify")
  assert_type(level, is_single_number, "single number")
  if (level < 0) {
    stop_collapse("`level` should not be negative.")
  }

  assert_type(method, is_string)
  assert_in_set(method, c("trim", "winsor"))

  assert_type(direction, is_string)
  assert_in_set(direction, c("left", "right", "both"))

  if (direction == "both") {
    if (level > 0.5) {
      stop_collapse(
        '`level` should not be greater than 0.5 in case `direction` is "both".'
      )
    }
  } else {
    if (level > 1) {
      stop_collapse(
        '`level` should not be greater than 1 in case `direction` is one of ',
        '"left" or "right".'
      )
    }
  }

  TRUE
}

trim_all <- function(f, direction) {
  f_x_tbl <- meta_x_tbl(f)

  res_x <- switch(
    direction,
    left = max(f_x_tbl[["x"]]),
    right = min(f_x_tbl[["x"]]),
    both = as_q.pdqr(f)(0.5)
  )

  new_pdqr_by_ref(f)(res_x, meta_type(f))
}

compute_support_after_remove <- function(f, level, direction) {
  supp <- meta_support(f)
  q_f <- as_q.pdqr(f)

  if (direction %in% c("left", "both")) {
    left_val <- q_f(level)
  } else {
    left_val <- supp[1]
  }
  if (direction %in% c("right", "both")) {
    right_val <- q_f(1 - level)
  } else {
    right_val <- supp[2]
  }

  c(left_val, right_val)
}

decrease_row_prob <- function(x_tbl, row, by_prob) {
  res_row_prob <- x_tbl[["prob"]][row] - by_prob
  if (res_row_prob == 0) {
    x_tbl <- x_tbl[-row, ]
  } else {
    x_tbl[["prob"]][row] <- res_row_prob
  }

  x_tbl
}
