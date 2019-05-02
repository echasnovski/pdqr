summ_interval <- function(f, level = 0.95, method = "minwidth",
                          n_grid = 10001) {
  assert_pdqr_fun(f)
  assert_type(
    level, is_single_number,
    type_name = "single number between 0 and 1",
    min_val = 0, max_val = 1
  )
  assert_type(method, is_string)
  assert_in_set(method, c("minwidth", "percentile", "sigma"))
  assert_type(
    n_grid, is_single_number,
    type_name = "single number more than 1",
    min_val = 1
  )

  edge_probs <- 0.5 * c(1-level, 1+level)
  res <- switch(
    method,
    minwidth = interval_minwidth(f, level, n_grid),
    percentile = as_q(f)(edge_probs),
    sigma = summ_mean(f) + stats::qnorm(edge_probs) * summ_sd(f)
  )
  f_supp <- meta_support(f)

  region_new(left = max(f_supp[1], res[1]), right = min(f_supp[2], res[2]))
}

interval_minwidth <- function(f, level = 0.95, n_grid = 10001) {
  if (level == 0) {
    mode <- summ_mode(f, method = "global")

    return(c(mode, mode))
  }

  n_seq <- seq_len(n_grid)

  left_prob_seq <- seq(0, 1-level, length.out = n_grid)
  prob_seq <- c(left_prob_seq, left_prob_seq + level)
  quants <- as_q(f)(prob_seq)

  width_vec <- quants[n_seq + n_grid] - quants[n_seq]
  # Rounding is needed to overcome numerical storage precision issues
  minwidth_ind <- which.min(round(width_vec, digits = 10))

  quants[minwidth_ind + c(0, n_grid)]
}
