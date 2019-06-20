summ_order <- function(f_list, method = "compare", decreasing = FALSE) {
  assert_f_list(f_list, allow_numbers = FALSE)
  assert_type(method, is_string)
  assert_in_set(method, c("compare", "mean", "median", "mode"))
  assert_type(decreasing, is_truefalse, "`TRUE` or `FALSE`")

  switch(
    method,
    compare = order_compare(f_list, decreasing = decreasing),
    mean = ,
    median = ,
    mode = order_center(f_list, method, decreasing = decreasing)
  )
}

summ_sort <- function(f_list, method = "compare", decreasing = FALSE) {
  f_list[summ_order(f_list, method = method, decreasing = decreasing)]
}

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
  #```
  # # Here P(d1 <= d2) >= 0.5, P(d2 <= d3) >= 0.5, but P(d1 <= d3) < 0.5
  # d1 <- new_d(data.frame(x = c(0.39, 0.44, 0.46), y = c(17, 14, 0)), "infin")
  # d2 <- new_d(data.frame(x = c(0.05, 0.3, 0.70), y = c(4, 0, 4)), "infin")
  # d3 <- new_d(data.frame(x = c(0.03, 0.40, 0.80), y = c(1, 1, 1)), "infin")
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
  # inequality is achieved with `> 0.5` instead of `>= 0.5`.
  # Also NOTE that this definition can result into `e1 > e2` and `e2 > e1`
  # being both `TRUE` (in case of two "fin" functions) or both `FALSE` (in case
  # of equivalent but not equal "infin" functions; this is kind of reasonable).
  prob_geq(e1[[1]], e2[[1]]) > 0.5
}

`==.pdqr_list` <- function(e1, e2) {
  # This is done for performance reasons. No unwanted effects are found yet.
  FALSE
}
