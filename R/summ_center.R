summ_center <- function(f, method = "mean") {
  # `f` is validated inside `summ_*()` calls
  assert_type(method, is_string)
  assert_in_set(method, c("mean", "median", "mode"))

  switch(
    method,
    mean = summ_mean(f),
    median = summ_median(f),
    mode = summ_mode(f, method = "global")
  )
}

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

summ_median <- function(f) {
  assert_pdqr_fun(f)

  as_q(f)(0.5)
}

summ_mode <- function(f, method = "global") {
  assert_pdqr_fun(f)
  assert_type(method, is_string)
  assert_in_set(method, c("global", "local"))

  f_x_tbl <- meta_x_tbl(f)
  x <- f_x_tbl[["x"]]
  col_name <- switch(meta_type(f), fin = "prob", infin = "y")
  col <- f_x_tbl[[col_name]]

  if (method == "global") {
    # Returns the first (smallest) value if there are more than 1
    x[which.max(col)]
  } else {
    col_left <- col[-length(col)]
    col_right <- col[-1]
    col_geq_right <- c(col_left >= col_right, TRUE)
    col_geq_left <- c(TRUE, col_right >= col_left)

    x[col_geq_right & col_geq_left]
  }
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
