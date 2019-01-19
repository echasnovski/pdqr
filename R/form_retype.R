form_retype <- function(f, type) {
  assert_pdqr_fun(f)
  assert_distr_type(type)

  switch(
    type,
    fin = retype_fin(f),
    infin = retype_infin(f)
  )
}

retype_fin <- function(f) {
  if (meta_type(f) == "fin") {
    return(f)
  }

  x_tbl <- meta_x_tbl(f)
  n <- nrow(x_tbl)

  x_lag <- x_tbl[["x"]][-n]
  x_lead <- x_tbl[["x"]][-1]
  y_lag <- x_tbl[["y"]][-n]
  y_lead <- x_tbl[["y"]][-1]
  y_sum <- y_lag + y_lead

  # Output `x` values are computed as intervals' centres of mass
  x_mass <- (x_lag * (y_lag + y_sum) + x_lead * (y_lead + y_sum)) / (3 * y_sum)
    # If interval has zero probability then its centre is set to the middle
  x_mass_bad <- !is.finite(x_mass)
  x_mass[x_mass_bad] <- (x_lag[x_mass_bad] + x_lead[x_mass_bad]) / 2

  # Output probabilities are computed as probabilites (mass) of intervals
  prob <- diff(x_tbl[["cumprob"]])

  # Creating pdqr-function
  pdqr_fun <- impute_pdqr_fun(pdqr_class = NULL, ref = f)

  pdqr_fun(data.frame(x = x_mass, prob = prob), "fin")
}

retype_infin <- function(f) {
  if (meta_type(f) == "infin") {
    return(f)
  }

  # Note that `f` has already passed `assert_pdqr_fun()` which means that "x"
  # column in "x_tbl" metadata is sorted and has no duplicate values
  x_tbl <- meta_x_tbl(f)

  n <- nrow(x_tbl)
  if (n < 4) {
    stop_collapse(
      'For conversion to "infin" type `form_retype()` needs at least 4 unique ',
      '`x` values.'
    )
  }

  x <- x_tbl[["x"]]
  prob <- x_tbl[["prob"]]

  # Values of `x` grid (except first and last elements) of "infin" output is
  # approximated as convex combination of nearest "centres of mass":
  # `x_mid = (1-alpha)*x_mass_left + alpha*x_mass_right`. Here `alpha` is
  # approximated based on two separate assumptions:
  # *Assumption 1*: if locally `y_left` (`y` value in "true" `x` to the left of
  # `x_mid`) = `y_mid` = `y_right` (by analogy with `y_left`), then
  # `alpha = prob_left / (prob_left + prob_right)`.
  # *Assumption 2*: if locally "true" values of `x` are equidistant then `alpha`
  # lie inside [1/3; 2/3] interval.
  # Final approximation is formed by combining these conclusions
  prob_sum <- prob[1:(n-1)] + prob[2:n]
  alpha <- pmin(pmax(prob[1:(n-1)] / prob_sum, 1/3), 2/3)
  alpha[!is.finite(alpha)] <- 0.5

  x_grid <- numeric(n + 1)
  x_grid[2:n] <- (1 - alpha) * x[1:(n-1)] + alpha * x[2:n]
  # First and last `x` are approximated so that first and last `x` triplets are
  # equidistant
  x_grid[1] <- x_grid[2] - (x_grid[3] - x_grid[2])
  x_grid[n+1] <- x_grid[n] + (x_grid[n] - x_grid[n-1])

  # Output `y` grid is approximated in 'pdqr' fashion
  p_grid <- c(0, cumsum(prob))
  y <- y_from_p_grid(x_grid, p_grid)

  # Creating pdqr-function
  pdqr_fun <- impute_pdqr_fun(pdqr_class = NULL, ref = f)

  pdqr_fun(data.frame(x = x_grid, y = y), "infin")
}
