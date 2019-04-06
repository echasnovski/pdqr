summ_quantile <- function(f, probs) {
  assert_pdqr_fun(f)
  assert_type(probs, is.numeric)
  if (any((probs < 0) | (probs > 1))) {
    stop_collapse("`probs` should have values inside [0; 1].")
  }

  as_q(f)(probs)
}

summ_prob_true <- function(f) {
  assert_pdqr_fun(f)
  if (!is_boolean_pdqr_fun(f)) {
    warning_collapse(
      '`f` is not a "boolean" pdqr-function (type "fin" with `x` in "x_tbl" ',
      'identical to c(0, 1)). Proceed with caution.'
    )
  }

  x_tbl <- meta_x_tbl(f)

  x_tbl[["prob"]][x_tbl[["x"]] == 1]
}

summ_prob_false <- function(f) {
  1 - summ_prob_true(f)
}
