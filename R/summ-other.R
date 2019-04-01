summ_quantile <- function(f, probs) {
  assert_pdqr_fun(f)
  assert_type(probs, is.numeric)
  if (any((probs < 0) | (probs > 1))) {
    stop_collapse("`probs` should have values inside [0; 1].")
  }

  as_q(f)(probs)
}
