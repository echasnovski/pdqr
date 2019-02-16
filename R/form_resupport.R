# form_resupport ----------------------------------------------------------
form_resupport <- function(f, support, method = "trim") {
  assert_pdqr_fun(f)
  assert_support(support, allow_na = TRUE)
  assert_type(method, is_string)
  assert_in_set(method, c("trim", "linear", "reflect", "winsor"))

  if (all(is.na(support))) {
    return(f)
  }

  supp <- coalesce_pair(support, meta_support(f))
  if (supp[1] > supp[2]) {
    stop_collapse(
      "After imputing `NA`s `support` equals (",
      supp[1], ", ", supp[2], ") which is not proper."
    )
  }

  switch(
    method,
    trim = resupport_trim(f, supp),
    linear = resupport_linear(f, supp),
    reflect = resupport_reflect(f, supp),
    winsor = resupport_winsor(f, supp)
  )
}


# "trim" ------------------------------------------------------------------
resupport_trim <- function(f, support) {
  switch(
    meta_type(f),
    fin = resupport_trim_fin(f, support),
    infin = resupport_trim_infin(f, support)
  )
}

resupport_trim_fin <- function(f, support) {
  res_x_tbl <- filter_x_tbl(meta_x_tbl(f), support)
  res_x_tbl <- res_x_tbl[, c("x", "prob")]

  if (sum(res_x_tbl[["prob"]]) <= 0) {
    stop_resupport_zero_tot_prob()
  }

  new_pdqr_by_ref(f)(res_x_tbl, "fin")
}

resupport_trim_infin <- function(f, support) {
  d_f <- as_d(f)
  edge_y <- d_f(support)

  # Add new rows to "x_tbl" metadata of `f` to capture "trimming" behavior.
  # However, if edge of `support` is outside of `f`'s support, then it is not
  # added. This is needed to not change shape of distribution.
  x_tbl_plus <- union_inside_x_tbl(
    x_tbl_orig = meta_x_tbl(f),
    x_tbl_new = data.frame(x = support, y = edge_y)
  )
  res_x_tbl <- filter_x_tbl(x_tbl_plus, support)

  if (trapez_integral(res_x_tbl[["x"]], res_x_tbl[["y"]]) <= 0) {
    stop_resupport_zero_tot_prob()
  }

  new_pdqr_by_ref(f)(res_x_tbl, "infin")
}


# "linear" ----------------------------------------------------------------
resupport_linear <- function(f, support) {
  if (support[1] == support[2]) {
    return(point_dirac(support[1], meta_type(f), get_pdqr_class(f)))
  }

  f_supp <- meta_support(f)

  if (f_supp[1] == f_supp[2]) {
    stop_collapse(
      "Can't resupport from single point support to interval one."
    )
  }

  res_x_tbl <- meta_x_tbl(f)
  res_x_tbl[["x"]] <- extrap_lin(
    x_1 = f_supp[1], x_2 = f_supp[2],
    y_1 = support[1], y_2 = support[2],
    x_target = res_x_tbl[["x"]]
  )

  new_pdqr_by_ref(f)(res_x_tbl, meta_type(f))
}


# "reflect" ---------------------------------------------------------------
resupport_reflect <- function(f, support) {
  f_supp <- meta_support(f)
  f_x_tbl <- meta_x_tbl(f)

  # Sum up densities for possible reflections
  x_tbl_list <- list(f_x_tbl)

  if (support[1] > f_supp[1]) {
    x_tbl_list <- c(x_tbl_list, list(reflect_x_tbl(f_x_tbl, support[1])))
  }
  if (support[2] < f_supp[2]) {
    x_tbl_list <- c(x_tbl_list, list(reflect_x_tbl(f_x_tbl, support[2])))
  }

  x_tbl <- stack_x_tbl(x_tbl_list)
  res <- new_pdqr_by_ref(f)(x_tbl, meta_type(f))

  # Trim total sum to supplied support
  form_resupport(res, support, "trim")
}


# "winsor" ----------------------------------------------------------------
resupport_winsor <- function(f, support) {
  if (support[1] == support[2]) {
    return(point_dirac(support[1], meta_type(f), get_pdqr_class(f)))
  }

  switch(
    meta_type(f),
    fin = resupport_winsor_fin(f, support),
    infin = resupport_winsor_infin(f, support)
  )
}

resupport_winsor_fin <- function(f, support) {
  f_x_tbl <- meta_x_tbl(f)
  x <- f_x_tbl[["x"]]
  x[x <= support[1]] <- support[1]
  x[x >= support[2]] <- support[2]
  f_x_tbl[["x"]] <- x

  new_pdqr_by_ref(f)(f_x_tbl, "fin")
}

resupport_winsor_infin <- function(f, support, h = 1e-8) {
  p_f <- as_p.pdqr(f)
  f_x_tbl <- meta_x_tbl(f)
  f_supp <- meta_support(f)

  # Early return extreme cases
  if (support[1] >= f_supp[2]) {
    return(
      return(point_dirac(support[1], meta_type(f), get_pdqr_class(f), h = h))
    )
  }
  if (support[2] <= f_supp[1]) {
    return(point_dirac(support[2], meta_type(f), get_pdqr_class(f), h = h))
  }

  x_tbl <- f_x_tbl

  # Winsor left
  if (support[1] > f_supp[1]) {
    x_tbl <- add_x_tbl_knots(x_tbl, support[1] + c(0, h))
    x_tbl <- filter_x_tbl(x_tbl, c(support[1], f_supp[2]))
    tail_prob <- p_f(support[1])
    x_tbl <- increase_tail_weight(x_tbl, tail_prob, "left")
  }

  # Winsor right
  if (support[2] < f_supp[2]) {
    x_tbl <- add_x_tbl_knots(x_tbl, support[2] - c(h, 0))
    x_tbl <- filter_x_tbl(x_tbl, c(f_supp[1], support[2]))
    tail_prob <- 1 - p_f(support[2])
    x_tbl <- increase_tail_weight(x_tbl, tail_prob, "right")
  }

  new_pdqr_by_ref(f)(x_tbl, "infin")
}

increase_tail_weight <- function(x_tbl, by_prob, edge) {
  n <- nrow(x_tbl)
  x <- x_tbl[["x"]]
  y <- x_tbl[["y"]]

  if (edge == "left") {
    present_prob <- (y[1] + y[2]) * (x[2] - x[1]) / 2
    to_prob <- present_prob + by_prob
    y[1] <- 2 * to_prob / (x[2] - x[1]) - y[2]
  } else if (edge == "right") {
    present_prob <- (y[n-1] + y[n]) * (x[n] - x[n-1]) / 2
    to_prob <- present_prob + by_prob
    y[n] <- 2 * to_prob / (x[n] - x[n-1]) - y[n-1]
  }

  data.frame(x = x, y = y)
}


# Other -------------------------------------------------------------------
stop_resupport_zero_tot_prob <- function() {
  stop_collapse(
    'Output of `form_resupport()` will not have positive total probability.'
  )
}
