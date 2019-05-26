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
