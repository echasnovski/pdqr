# To use `distr_tbl` given a sample, call `*_fun()` with `attach_x = TRUE`.
distr_tbl <- function(f, n_discrete = 10001, ...) {
  if (!(has_meta_x(f) || is_pdqr_fun(f, check_x = FALSE))) {
    stop_collapse('`f` should have "x" metadata or be "pdqr" function.')
  }

  if (has_meta_x(f)) {
    vec_distr_tbl(meta(f, "x"))
  } else {
    # Discretize "pdqr" function
    p_f <- as_p(f, ...)

    p_fun_distr_tbl(p_f, n_discrete = n_discrete)
  }
}

vec_distr_tbl <- function(x, vals = sort(unique(x))) {
  x <- x[!is.na(x)]

  x_val_id <- match(x, vals)
  prob <- tabulate(x_val_id) / length(x)

  data.frame(x = vals, prob = prob)
}

p_fun_distr_tbl <- function(p_f, n_discrete = n_discrete) {
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
