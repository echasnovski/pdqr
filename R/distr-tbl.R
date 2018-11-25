distr_tbl <- function(obj, n_discrete = 10001, ...) {
  if (!(is.numeric(obj) || has_meta(obj, "x") || is_pdqr_fun(obj))) {
    stop_collapse(
      '`obj` should have "x" metadata or be either numeric or "pdqr" function.'
    )
  }

  if (is.numeric(obj)) {
    return(vec_distr_tbl(obj))
  }

  if (has_meta(obj, "x")) {
    x_meta <- meta(obj, "x")
    if (!is.numeric(x_meta)) {
      stop_collapse(
        '"x" metadata should be ', "'numeric', not '", get_type(x_meta), "'."
      )
    }
    return(vec_distr_tbl(x_meta))
  }

  # Discretize "pdqr" function
  p_obj <- as_p(obj, ...)

  p_fun_distr_tbl(p_obj, n_discrete = n_discrete)
}

vec_distr_tbl <- function(x, vals = sort(unique(x))) {
  x_val_id <- match(x, vals)
  prob <- tabulate(x_val_id) / length(x)

  data.frame(x = vals, prob = prob)
}

p_fun_distr_tbl <- function(p_f, n_discrete = n_discrete) {
  support <- meta(p_f, "support")
  if (!is_support(support)) {
    stop_collapse('`obj` should have proper "support" metadata.')
  }

  x_discrete <- seq(from = support[1], to = support[2], length.out = n_discrete)
  p_vec <- p_f(c(support[1] - 10^(-6), x_discrete))
  prob <- diff(p_vec)
  prob <- prob / sum(prob)

  prob_isnt_zero <- !is_near(prob, 0)

  data.frame(x = x_discrete[prob_isnt_zero], prob = prob[prob_isnt_zero])
}
