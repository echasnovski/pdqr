summ_mean <- function(f) {
  assert_pdqr_fun(f)

  # Not using `raw_moment(f, 1)` for speed reasons
  x_tbl <- meta_x_tbl(f)

  switch(
    meta_type(f),
    fin = dotprod(x_tbl[["x"]], x_tbl[["prob"]]),
    infin = summ_mean_infin(x_tbl)
  )
}

summ_mean_infin <- function(x_tbl) {
  n <- nrow(x_tbl)
  x_lag <- x_tbl[["x"]][-n]
  x_lead <- x_tbl[["x"]][-1]
  y_lag <- x_tbl[["y"]][-n]
  y_lead <- x_tbl[["y"]][-1]
  y_sum <- y_lag + y_lead
  x_mass <- (x_lag * (y_lag + y_sum) + x_lead * (y_lead + y_sum))/(3 * y_sum)

  x_mass_is_good <- is.finite(x_mass)

  prob <- diff(x_tbl[["cumprob"]])

  dotprod(x_mass[x_mass_is_good], prob[x_mass_is_good])
}
