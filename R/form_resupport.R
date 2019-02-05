form_resupport <- function(f, support = NULL, method = "trim") {
  support <- format_support(support)

  assert_pdqr_fun(f)
  assert_support(support, allow_na = TRUE)
  assert_type(method, is_string)
  if (!(method %in% c("trim", "linear", "reflect", "winsor"))) {
    stop_collapse(
      '`method` should be one of "trim", "linear", "reflect", or "winsor".'
    )
  }

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
    reflect = resupport_reflect(f, supp)
    # winsor = resupport_winsor(f, supp)
  )
}

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

resupport_linear <- function(f, support) {
  if (support[1] == support[2]) {
    new_pdqr_by_ref(f)(data.frame(x = support[1], prob = 1), "fin")
  } else {
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
}

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

# resupport_winsor <- function(f, support) {
#
# }

stop_resupport_zero_tot_prob <- function() {
  stop_collapse(
    'Output of `form_resupport()` will not have positive total probability.'
  )
}
