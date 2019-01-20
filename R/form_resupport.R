form_resupport <- function(f, support = NULL, method = "trim") {
  support <- format_support(support)

  assert_pdqr_fun(f)
  assert_support(support, allow_na = TRUE)
  assert_type(method, is_string)
  if (!(method %in% c("trim", "move", "reflect", "winsor"))) {
    stop_collapse(
      '`method` should be one of "trim", "move", "reflect", or "winsor".'
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
    trim = resupport_trim(f, supp)
    # move = resupport_move(f, supp),
    # reflect = resupport_reflect(f, supp),
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

# resupport_move <- function(f, support) {
#
# }

# resupport_reflect <- function(f, support) {
#
# }

# resupport_winsor <- function(f, support) {
#
# }

filter_x_tbl <- function(x_tbl, support) {
  is_x_in_support <- (x_tbl[["x"]] >= support[1]) & (x_tbl[["x"]] <= support[2])

  x_tbl[is_x_in_support, ]
}

union_inside_x_tbl <- function(x_tbl_orig, x_tbl_new) {
  # Remove rows from `x_tbl_new` which are outside from `x_tbl_orig`'s support
  orig_supp <- range(x_tbl_orig[["x"]])
  x_tbl_new <- filter_x_tbl(x_tbl_new, orig_supp)

  second_col <- if ("prob" %in% names(x_tbl_orig)) {"prob"} else {"y"}

  res <- data.frame(x = c(x_tbl_orig[["x"]], x_tbl_new[["x"]]))
  res[[second_col]] <- c(x_tbl_orig[[second_col]], x_tbl_new[[second_col]])

  # Remove rows with duplicate "x" (leaving in output rows from `x_tbl_orig`)
  res <- res[!duplicated(res[["x"]]), ]

  res <- res[order(res[["x"]]), ]
  row.names(res) <- NULL

  res
}

stop_resupport_zero_tot_prob <- function() {
  stop_collapse(
    'Output of `form_resupport()` will not have positive total probability.'
  )
}
