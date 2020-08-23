#' Summarize list of pdqr-functions with order
#'
#' Functions for ordering the set of pdqr-functions supplied in a list. This
#' might be useful for doing comparative statistical inference for several
#' groups of data.
#'
#' @param f_list List of pdqr-functions.
#' @param method Method to be used for ordering. Should be one of "compare",
#'   "mean", "median", "mode", "midrange".
#' @param decreasing If `TRUE` ordering is done decreasingly.
#'
#' @details Ties for all methods are handled so as to preserve the original
#' order.
#'
#' Method "compare" is using the following ordering relation: pdqr-function `f`
#' is greater than `g` if and only if `P(f >= g) > 0.5`, or in code
#' `summ_prob_true(f >= g) > 0.5` (see [pdqr methods for "Ops" group generic
#' family][methods-group-generic] for more details on comparing pdqr-functions).
#' This method orders input based on this relation and [order()][base::order()]
#' function. **Notes**:
#' - This relation doesn't define strictly ordering because it is not
#' transitive: there can be pdqr-functions `f`, `g`, and `h`, for which `f` is
#' greater than `g`, `g` is greater than `h`, and `h` is greater than `f` (but
#' should be otherwise). If not addressed, this might result into dependence of
#' output on order of the input. It is solved by first preordering `f_list`
#' based on method "mean" and then calling `order()`.
#' - Because comparing two pdqr-functions can be time consuming, this method
#' becomes rather slow as number of `f_list` elements grows.
#'
#' Methods "mean", "median", "mode", and "midrange" are based on
#' [summ_center()]: ordering of `f_list` is defined as ordering of corresponding
#' measures of distribution's center.
#'
#' @return `summ_order()` works essentially like [order()][base::order()]. It
#' returns an integer vector representing a permutation which rearranges
#' `f_list` in desired order.
#'
#' `summ_sort()` returns a sorted (in desired order) variant of `f_list`.
#'
#' `summ_rank()` returns a numeric vector representing ranks of `f_list`
#' elements: 1 for the "smallest", `length(f_list)` for the "biggest".
#'
#' @family summary functions
#'
#' @examples
#' d_fun <- as_d(dunif)
#' f_list <- list(a = d_fun, b = d_fun + 1, c = d_fun - 1)
#' summ_order(f_list)
#' summ_sort(f_list)
#' summ_rank(f_list)
#'
#' # All methods might give different results on some elaborated pdqr-functions
#' # Methods "compare" and "mean" are not equivalent
#' non_mean_list <- list(
#'   new_d(data.frame(x = c(0.56, 0.815), y = c(1, 1)), "continuous"),
#'   new_d(data.frame(x = 0:1, y = c(0, 1)), "continuous")
#' )
#' summ_order(non_mean_list, method = "compare")
#' summ_order(non_mean_list, method = "mean")
#'
#' # Methods powered by `summ_center()` are not equivalent
#' m <- c(0, 0.2, 0.1)
#' s <- c(1.1, 1.2, 1.3)
#' dlnorm_list <- lapply(seq_along(m), function(i) {
#'   as_d(dlnorm, meanlog = m[i], sdlog = s[i])
#' })
#' summ_order(dlnorm_list, method = "mean")
#' summ_order(dlnorm_list, method = "median")
#' summ_order(dlnorm_list, method = "mode")
#'
#' # Method "compare" handles inherited non-transitivity. Here third element is
#' # "greater" than second (`P(f >= g) > 0.5`), second - than first, and first
#' # is "greater" than third.
#' non_trans_list <- list(
#'   new_d(data.frame(x = c(0.39, 0.44, 0.46), y = c(17, 14, 0)), "continuous"),
#'   new_d(data.frame(x = c(0.05, 0.3, 0.70), y = c(4, 0, 4)), "continuous"),
#'   new_d(data.frame(x = c(0.03, 0.40, 0.80), y = c(1, 1, 1)), "continuous")
#' )
#' summ_sort(non_trans_list)
#' ## Output doesn't depend on initial order
#' summ_sort(non_trans_list[c(2, 3, 1)])
#' @name summ_order
NULL

#' @export
#' @rdname summ_order
summ_order <- function(f_list, method = "compare", decreasing = FALSE) {
  assert_f_list(f_list, allow_numbers = FALSE)
  assert_method(method, methods_order)
  assert_type(decreasing, is_truefalse, "`TRUE` or `FALSE`")

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  switch(
    method,
    compare = order_compare(f_list, decreasing = decreasing),
    mean = ,
    median = ,
    mode = ,
    midrange = order_center(f_list, method, decreasing = decreasing)
  )
}

methods_order <- c("compare", methods_center)

#' @export
#' @rdname summ_order
summ_sort <- function(f_list, method = "compare", decreasing = FALSE) {
  f_list[summ_order(f_list, method = method, decreasing = decreasing)]
}

#' @export
#' @rdname summ_order
summ_rank <- function(f_list, method = "compare") {
  # Here the fact that `order(order(x))` and `rank(x)` return equal results is
  # used (holds for "simple" cases without ties)
  res <- order(summ_order(f_list, method = method))
  names(res) <- names(f_list)

  res
}

order_compare <- function(f_list, decreasing) {
  # Preorder by mean value is done to increase performance, because in this case
  # less calls to `>` and `==` will be made. This also "fixes" the
  # non-transitivity issue of `>.pdqr_list` by removing dependence of output
  # based on the order of the input.
  # Example of the effect of non-transitivity:
  # ```
  # # Here P(d1 <= d2) >= 0.5, P(d2 <= d3) >= 0.5, but P(d1 <= d3) < 0.5
  # d1 <- new_d(
  #   data.frame(x = c(0.39, 0.44, 0.46), y = c(17, 14, 0)), "continuous"
  # )
  # d2 <- new_d(
  #   data.frame(x = c(0.05, 0.3, 0.70), y = c(4, 0, 4)), "continuous"
  # )
  # d3 <- new_d(
  #   data.frame(x = c(0.03, 0.40, 0.80), y = c(1, 1, 1)), "continuous"
  # )
  # f_list <- list(d1, d2, d3)
  # class(f_list) <- "pdqr_list"
  # # Returns c(1, 2, 3)
  # order(f_list)
  # # And this returns c(2, 1, 3), which is not aligned with previous output
  # # (should be c(3, 2, 1))
  # order(f_list[3:1])
  # ```
  mean_vec <- vapply(f_list, summ_mean, numeric(1))
  mean_order <- order(mean_vec)

  f_list <- f_list[mean_order]
  class(f_list) <- "pdqr_list"

  # Here `order(f_list)` works because of `>`, `==`, and `[` methods for
  # "pdqr_list" class
  mean_order[order(f_list, decreasing = decreasing)]
}

order_center <- function(f_list, method, decreasing = decreasing) {
  center_vec <- vapply(f_list, summ_center, numeric(1), method = method)

  order(center_vec, decreasing = decreasing)
}

`[.pdqr_list` <- function(x, i) {
  res <- unclass(x)[i]
  class(res) <- "pdqr_list"

  res
}

`>.pdqr_list` <- function(e1, e2) {
  # NOTE that here `prob_geq()` is used and not `prob_greater()`. Strict
  # inequality is achieved with `> 0.5` instead of `>= 0.5`. Also NOTE that this
  # definition can result into `e1 > e2` and `e2 > e1` being both `TRUE` (in
  # case of two "discrete" functions) or both `FALSE` (in case of equivalent but
  # not equal "continuous" functions; this is kind of reasonable).
  prob_geq(e1[[1]], e2[[1]]) > 0.5
}

`==.pdqr_list` <- function(e1, e2) {
  # This is done for performance reasons. No unwanted effects are found yet.
  FALSE
}
