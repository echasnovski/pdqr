# form_mix ----------------------------------------------------------------
form_mix <- function(f_list, weights = NULL) {
  assert_type(f_list, is.list)
  assert_f_list(f_list, allow_numbers = FALSE)

  assert_type(weights, is.numeric, allow_null = TRUE)
  weights <- impute_weights(weights, length(f_list))

  f_list_meta <- compute_f_list_meta(f_list)
  res_type <- f_list_meta[["type"]]

  sec_col <- if (res_type == "fin") {"prob"} else {"y"}

  x_tbl_list <- lapply(seq_along(f_list), function(i) {
    f_typed <- form_retype(f_list[[i]], res_type, method = "dirac")

    x_tbl <- meta_x_tbl(f_typed)
    # Do weighting of distributions
    x_tbl[[sec_col]] <- x_tbl[[sec_col]] * weights[i]

    x_tbl
  })

  x_tbl <- stack_x_tbl(x_tbl_list)

  new_pdqr_by_class(f_list_meta[["class"]])(x_tbl, res_type)
}

impute_weights <- function(weights, n) {
  if (is.null(weights)) {
    weights <- rep(1, n) / n
  } else {
    weights <- recycle_vec(weights, n)
  }

  if (any(weights < 0)) {
    stop_collapse("`weights` should not have negative elements")
  }
  if (sum(weights) <= 0) {
    stop_collapse("`weights` should have positive sum.")
  }

  weights / sum(weights)
}
