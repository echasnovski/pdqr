summ_roc <- function(f, g, n_grid = 1001) {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 1",
    min_val = 1
  )

  # This is needed to achieve [0; 1] range of both `fpr` and `tpr` in case of
  # "fin" type input
  t_range <- stretch_range(union_support(f, g))
  t_grid <- seq_between(t_range, length.out = n_grid)

  fpr <- 1 - as_p(f)(t_grid)
  tpr <- 1 - as_p(g)(t_grid)

  data.frame(threshold = t_grid, fpr = fpr, tpr = tpr)
}

summ_rocauc <- function(f, g) {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)

  prob_greater(g, f)
}

roc_plot <- function(roc, ..., add_bisector = TRUE) {
  assert_roc(roc)
  assert_type(add_bisector, is_truefalse, type_name = "`TRUE` or `FALSE`")

  roc_name <- deparse(substitute(roc))

  # Order points from left to right with respect to "fpr" using inverse
  # relationship between "threshold" and "fpr". This is needed for correct
  # "horizontal-vertical" ordering of curve "steps".
  roc <- roc[order(roc[["threshold"]], decreasing = TRUE), ]

  plot_args <- c_dedupl(
    list(x = roc[["fpr"]], y = roc[["tpr"]]),
    ...,
    list(
      type = "s",
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

roc_lines <- function(roc, ...) {
  assert_roc(roc)

  # Order points from left to right with respect to "fpr" using inverse
  # relationship between "threshold" and "fpr". This is needed for correct
  # "horizontal-vertical" ordering of curve "steps".
  roc <- roc[order(roc[["threshold"]], decreasing = TRUE), ]

  lines_args <- c_dedupl(
    list(x = roc[["fpr"]], y = roc[["tpr"]]),
    ...,
    list(type = "s")
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
  if (!is_roc(roc)) {
    stop_collapse("`roc` should represent ROC curve. See `summ_roc()`.")
  }

  TRUE
}
