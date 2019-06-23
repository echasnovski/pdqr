#' Summarize distributions with ROC curve
#'
#' These functions help you perform a ROC ("Receiver Operating Characteristic")
#' analysis for one-dimensional linear classifier: values not more than some
#' threshold are classified as "negative", and more than threshold -
#' as "positive". Here input pair of pdqr-functions represent "true"
#' distributions of values with "negative" (`f`) and "positive" (`g`) labels.
#'
#' @inheritParams summ_separation
#' @param n_grid Number of points of ROC curve to be computed.
#' @param method Method of computing ROC AUC. Should be one of "expected",
#'   "pessimistic", "optimistic" (see Details).
#' @param roc A data frame representing ROC curve. Typically an output of
#'   `summ_roc()`.
#' @param ... Other arguments to be passed to `plot()` or `lines()`.
#' @param add_bisector If `TRUE` (default), `roc_plot()` adds bisector line as
#'   reference for "random guess" classifier.
#'
#' @details ROC curve describes how well classifier performs under different
#' thresholds. For all possible thresholds two classification metrics are
#' computed which later form x and y coordinates of a curve:
#' - **False positive rate (FPR)**: proportion of "negative" distribution which
#' was (incorrectly) classified as "positive". This is the same as one minus
#' "specificity" (proportion of "negative" values classified as "negative").
#' - **True positive rate (TPR)**: proportion of "positive" distribution which
#' was (correctly) classified as "positive". This is also called "sensitivity".
#'
#' `summ_roc()` creates a uniform grid of decreasing `n_grid` values (so that
#' output points of ROC curve are ordered from left to right) covering range of
#' all meaningful thresholds. This range is computed as slightly extended range
#' of `f` and `g` supports (extension is needed to achieve extreme values of
#' "fpr" in presence of "discrete" type). Then FPR and TPR are computed for
#' every threshold.
#'
#' `summ_rocauc()` computes a common general (without any particular threshold
#' in mind) diagnostic value of classifier, **area under ROC curve** ("ROC AUC"
#' or "AUROC"). Numerically it is equal to a probability of random variable with
#' distribution *`g` being strictly greater than `f`* plus *possible correction
#' for functions being equal*, with multiple ways to account for it. Method
#' "pessimistic" doesn't add correction, "expected" adds half of probability of
#' `f` and `g` being equal (which is default), "optimistic" adds full
#' probability. **Note** that this means that correction might be done only if
#' both input pdqr-functions have "discrete" type. See [pdqr methods for "Ops"
#' group generic family][methods-group-generic] for more details on comparing
#' functions.
#'
#' `roc_plot()` and `roc_lines()` perform plotting (with
#' [plot()][graphics::plot()]) and adding (with [lines()][graphics::lines()])
#' ROC curves respectively.
#'
#' @return `summ_roc()` returns a data frame with `n_grid` rows and columns
#' "threshold" (grid of classification thresholds, ordered decreasingly), "fpr",
#' and "tpr" (corresponding false and true positive rates, ordered
#' non-decreasingly by "fpr").
#'
#' `summ_rocauc()` returns single number representing area under the ROC curve.
#'
#' `roc_plot()` and `roc_lines()` create plotting side effects.
#'
#' @seealso [summ_separation()] for computing optimal separation threshold.
#'
#' @family summary functions
#'
#' @examples
#' d_norm_1 <- as_d(dnorm)
#' d_norm_2 <- as_d(dnorm, mean = 1)
#' roc <- summ_roc(d_norm_1, d_norm_2)
#' head(roc)
#'
#' # `summ_rocauc()` is equivalent to probability of `g > f`
#' summ_rocauc(d_norm_1, d_norm_2)
#' summ_prob_true(d_norm_2 > d_norm_1)
#'
#' # Plotting
#' roc_plot(roc)
#' roc_lines(summ_roc(d_norm_2, d_norm_1), col = "blue")
#'
#' # For "discrete" functions `summ_rocauc()` can produce different outputs
#' d_dis_1 <- new_d(1:2, "discrete")
#' d_dis_2 <- new_d(2:3, "discrete")
#' summ_rocauc(d_dis_1, d_dis_2)
#' summ_rocauc(d_dis_1, d_dis_2, method = "pessimistic")
#' summ_rocauc(d_dis_1, d_dis_2, method = "optimistic")
#'   # These methods correspond to different ways of plotting ROC curves
#' roc <- summ_roc(d_dis_1, d_dis_2)
#'   # Default line plot for "expected" method
#' roc_plot(roc, main = "Different type of plotting ROC curve")
#'   # Method "pessimistic"
#' roc_lines(roc, type = "s", col = "blue")
#'   # Method "optimistic"
#' roc_lines(roc, type = "S", col = "green")
#'
#' @name summ_roc
NULL

#' @rdname summ_roc
#' @export
summ_roc <- function(f, g, n_grid = 1001) {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 1",
    min_val = 1
  )

  # This is needed to achieve [0; 1] range of both `fpr` and `tpr` in case of
  # "discrete" type input
  t_range <- stretch_range(union_support(f, g))
  # Inversing range so that `t_grid` will be decreasing sequence. This is needed
  # to order points of ROC curve from left to right.
  t_grid <- seq_between(t_range[2:1], length.out = n_grid)

  fpr <- 1 - as_p(f)(t_grid)
  tpr <- 1 - as_p(g)(t_grid)

  data.frame(threshold = t_grid, fpr = fpr, tpr = tpr)
}

#' @rdname summ_roc
#' @export
summ_rocauc <- function(f, g, method = "expected") {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(method, is_string)
  assert_in_set(method, c("expected", "pessimistic", "optimistic"))

  method_coef <- switch(method, expected = 0.5, pessimistic = 0, optimistic = 1)

  prob_greater(g, f) + method_coef * prob_equal(g, f)
}

#' @rdname summ_roc
#' @export
roc_plot <- function(roc, ..., add_bisector = TRUE) {
  assert_roc(roc)
  assert_type(add_bisector, is_truefalse, type_name = "`TRUE` or `FALSE`")

  roc_name <- deparse(substitute(roc))

  # Ensure ordering of points from left to right with respect to "fpr" using
  # inverse relationship between "threshold" and "fpr". This is needed to avoid
  # possible "zig-zags" in output line.
  if (is.unsorted(-roc[["threshold"]])) {
    roc <- roc[order(roc[["threshold"]], decreasing = TRUE), ]
  }

  plot_args <- c_dedupl(
    list(x = roc[["fpr"]], y = roc[["tpr"]]),
    ...,
    list(
      type = "l",
      xlim = c(0, 1), ylim = c(0, 1),
      xlab = "FPR or (1 - specificity)", ylab = "TPR or sensitivity",
      main = paste0("ROC curve for ", roc_name)
    )
  )
  do.call(graphics::plot, plot_args)

  if (add_bisector) {
    graphics::lines(c(0, 1), c(0, 1), lty = "dotted")
  }

  invisible()
}

#' @rdname summ_roc
#' @export
roc_lines <- function(roc, ...) {
  assert_roc(roc)

  # Ensure ordering of points from left to right with respect to "fpr" using
  # inverse relationship between "threshold" and "fpr". This is needed to avoid
  # possible "zig-zags" in output line.
  if (is.unsorted(-roc[["threshold"]])) {
    roc <- roc[order(roc[["threshold"]], decreasing = TRUE), ]
  }

  lines_args <- c_dedupl(
    list(x = roc[["fpr"]], y = roc[["tpr"]]),
    ...,
    list(type = "l")
  )

  do.call(graphics::lines, lines_args)
}

is_roc <- function(roc) {
  is.data.frame(roc) &&
    all(c("threshold", "fpr", "tpr") %in% names(roc)) &&
    is.numeric(roc[["fpr"]]) && is.numeric(roc[["tpr"]]) &&
    is.numeric(roc[["threshold"]])
}

assert_roc <- function(roc) {
  if (missing(roc)) {
    error_missing("`roc`", "data frame for ROC curve")
  }
  if (!is_roc(roc)) {
    stop_collapse("`roc` should represent ROC curve. See `summ_roc()`.")
  }

  TRUE
}
