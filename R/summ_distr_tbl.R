# To use `summ_distr_tbl` given a sample, call `new_*()` with `type = "fin"`.
summ_distr_tbl <- function(f, n_discrete = 10001, ...) {
  if (!is_pdqr_fun(f)) {
    stop_collapse('`f` should be "pdqr" function.')
  }

  # Discretize "pdqr" function
  p_f <- as_p(f, ...)

  p_summ_distr_tbl(p_f, n_discrete = n_discrete)
}

p_summ_distr_tbl <- function(p_f, n_discrete = n_discrete) {
  support <- meta(p_f, "support")

  x_discrete <- seq(from = support[1], to = support[2], length.out = n_discrete)
  p_vec <- p_f(c(support[1] - 10^(-6), x_discrete))
  prob <- diff(p_vec)
  prob <- prob / sum(prob)

  prob_isnt_zero <- !is_near(prob, 0)
  new_prob <- prob[prob_isnt_zero]
  new_prob <- new_prob / sum(new_prob)

  data.frame(x = x_discrete[prob_isnt_zero], prob = new_prob)
}
