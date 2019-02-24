form_tails <- function(f, level, method = "trim", direction = "both") {
  assert_form_tails_args(f, level, method, direction)

  switch(
    method,
    trim = tails_trim(f, level, direction),
    winsor = tails_winsor(f, level, direction)
  )
}

tails_trim <- function(f, level, direction = "both") {
  is_all_trimmed <- ((direction %in% c("left", "right")) && (level == 1)) ||
    ((direction == "both") && (level == 0.5))
  if (is_all_trimmed) {
    return(trim_all(f, direction))
  }

  switch(
    meta_type(f),
    fin = tails_trim_fin(f, level, direction),
    infin = tails_trim_infin(f, level, direction)
  )
}

tails_winsor <- function(f, level, direction = "both") {
  new_supp <- compute_support_after_remove(f, level, direction)

  form_resupport(f, new_supp, method = "winsor")
}

tails_trim_fin <- function(f, level, direction) {
  f_x_tbl <- meta_x_tbl(f)

  new_supp <- compute_support_after_remove(f, level, direction)

  if (direction %in% c("left", "both")) {
    x_is_to_remove <- f_x_tbl[["x"]] < new_supp[1]
    tot_p_to_remove <- sum(f_x_tbl[["prob"]][x_is_to_remove])
    f_x_tbl <- f_x_tbl[!x_is_to_remove, ]

    # Delete "underremoved" probability from left "x" value. If all probability
    # is removed, the whole row is removed.
    f_x_tbl <- decrease_row_prob(
      f_x_tbl, row = 1, by_prob = level - tot_p_to_remove
    )
  }

  if (direction %in% c("right", "both")) {
    x_is_to_remove <- f_x_tbl[["x"]] > new_supp[2]
    tot_p_to_remove <- sum(f_x_tbl[["prob"]][x_is_to_remove])
    f_x_tbl <- f_x_tbl[!x_is_to_remove, ]

    # Delete "underremoved" probability from right "x" value. If all probability
    # is removed, the whole row is removed.
    f_x_tbl <- decrease_row_prob(
      f_x_tbl, row = nrow(f_x_tbl), by_prob = level - tot_p_to_remove
    )
  }

  new_pdqr_by_ref(f)(f_x_tbl, "fin")
}

tails_trim_infin <- function(f, level, direction) {
  new_supp <- compute_support_after_remove(f, level, direction)

  form_resupport(f, new_supp, method = "trim")
}

assert_form_tails_args <- function(f, level, method, direction) {
  assert_pdqr_fun(f)
  assert_type(level, is_single_number, "single number")
  if (level < 0) {
    stop_collapse("`level` should not be negative.")
  }

  assert_type(method, is_string)
  assert_in_set(method, c("trim", "winsor"))

  assert_type(direction, is_string)
  assert_in_set(direction, c("left", "right", "both"))

  if (direction == "both") {
    if (level > 0.5) {
      stop_collapse(
        '`level` should not be greater than 0.5 in case `direction` is "both".'
      )
    }
  } else {
    if (level > 1) {
      stop_collapse(
        '`level` should not be greater than 1 in case `direction` is one of ',
        '"left" or "right".'
      )
    }
  }

  TRUE
}

trim_all <- function(f, direction) {
  f_x_tbl <- meta_x_tbl(f)

  res_x <- switch(
    direction,
    left = max(f_x_tbl[["x"]]),
    right = min(f_x_tbl[["x"]]),
    both = as_q.pdqr(f)(0.5)
  )

  new_pdqr_by_ref(f)(res_x, meta_type(f))
}

compute_support_after_remove <- function(f, level, direction) {
  supp <- meta_support(f)
  q_f <- as_q.pdqr(f)

  if (direction %in% c("left", "both")) {
    left_val <- q_f(level)
  } else {
    left_val <- supp[1]
  }
  if (direction %in% c("right", "both")) {
    right_val <- q_f(1 - level)
  } else {
    right_val <- supp[2]
  }

  c(left_val, right_val)
}

decrease_row_prob <- function(x_tbl, row, by_prob) {
  res_row_prob <- x_tbl[["prob"]][row] - by_prob
  if (res_row_prob == 0) {
    x_tbl <- x_tbl[-row, ]
  } else {
    x_tbl[["prob"]][row] <- res_row_prob
  }

  x_tbl
}