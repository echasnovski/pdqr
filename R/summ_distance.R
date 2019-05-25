# summ_distance() ---------------------------------------------------------
summ_distance <- function(f, g, method = "KS") {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_type(method, is_string)
  assert_in_set(method, c("KS"))

  switch(
    method,
    KS = distance_ks(f, g)
  )
}


# Method "KS" -------------------------------------------------------------
distance_ks <- function(f, g) {
  p_f <- as_p(f)
  p_g <- as_p(g)

  f_type <- meta_type(f)
  g_type <- meta_type(g)

  if (f_type == "fin") {
    if (g_type == "fin") {
      distance_ks_two_fin(p_f, p_g)
    } else {
      distance_ks_mixed(p_fin = p_f, p_infin = p_g)
    }
  } else {
    if (g_type == "fin") {
      distance_ks_mixed(p_fin = p_g, p_infin = p_f)
    } else {
      distance_ks_two_infin(p_f, p_g)
    }
  }
}

distance_ks_two_fin <- function(p_f, p_g) {
  ks_sep <- separation_ks_two_fin(p_f, p_g)

  abs(p_f(ks_sep) - p_g(ks_sep))
}

distance_ks_mixed <- function(p_fin, p_infin) {
  # Not using `separation_ks_mixed()` because of possible "limit" nature of K-S
  # distance which is a "supremum" and not "maximum". Its output might be
  # misleading because supremum distance might be achieved as left limit at the
  # point. See also commentary in `separation_ks_mixed()`.
  x_test <- meta_x_tbl(p_fin)[["x"]]

  p_infin_cumprob <- p_infin(x_test)

  p_fin_cumprob <- meta_x_tbl(p_fin)[["cumprob"]]
  p_fin_left_cumprob <- c(0, p_fin_cumprob[-length(p_fin_cumprob)])

  max(
    abs(p_infin_cumprob - p_fin_cumprob),
    abs(p_infin_cumprob - p_fin_left_cumprob)
  )
}

distance_ks_two_infin <- function(p_f, p_g) {
  ks_sep <- separation_ks_two_infin(p_f, p_g)

  abs(p_f(ks_sep) - p_g(ks_sep))
}
