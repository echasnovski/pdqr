# Compute "x_tbl" ---------------------------------------------------------
compute_x_tbl <- function(x, type, ...) {
  switch(
    type,
    discrete = compute_x_tbl_dis(x),
    continuous = compute_x_tbl_con(x, ...)
  )
}

compute_x_tbl_dis <- function(x) {
  x <- x[!is.na(x)]
  x <- round(x, digits = 10)
  vals <- sort(unique(x))

  x_val_id <- match(x, vals)
  prob <- tabulate(x_val_id) / length(x)
  cumprob <- cumsum(prob)

  data.frame(x = vals, prob = prob, cumprob = cumprob)
}

compute_x_tbl_con <- function(x, ...) {
  if (length(x) == 1) {
    dirac_x_tbl(x)
  } else {
    res <- density_piecelin(x, ...)
    res[["cumprob"]] <- trapez_part_integral(res[["x"]], res[["y"]])

    res
  }
}

dirac_x_tbl <- function(at_x, h = 1e-8) {
  x <- at_x + h*c(-1, 0, 1)
  y <- c(0, 1, 0)/h
  # Normalization is needed to ensure that total integral is 1. If omit this,
  # then, for example, `new_d(1e8, "continuous")` will have total integral of
  # around `1.49`.
  y <- y / trapez_integral(x, y)

  data.frame(x = x, y = y, cumprob = c(0, 0.5, 1))
}


# Impute "x_tbl" ----------------------------------------------------------
impute_x_tbl <- function(x, type, ...) {
  if (is.numeric(x)) {
    x <- filter_numbers(x)
    if (length(x) == 0) {
      stop_collapse("`x` shouldn't be empty.")
    }

    compute_x_tbl(x, type, ...)
  } else if (is.data.frame(x)) {
    assert_x_tbl(x, type)

    impute_x_tbl_impl(x, type)
  } else {
    stop_collapse("`x` should be numeric vector or data frame.")
  }
}

impute_x_tbl_impl <- function(x_tbl, type) {
  # `if` is used for effecient memory management because calling `[` creates
  # copy of an object
  if (is.unsorted(x_tbl[["x"]])) {
    x_tbl <- x_tbl[order(x_tbl[["x"]]), ]
  }

  if (type == "discrete") {
    impute_x_tbl_impl_dis(x_tbl)
  } else if (type == "continuous") {
    impute_x_tbl_impl_con(x_tbl)
  } else {
    stop("Wrong `type`.")
  }
}

impute_x_tbl_impl_dis <- function(x_tbl) {
  # Rounding is needed due to some usage of `form_retype()`. This also aligns
  # with how d-functions of type "discrete" work (see `new_d_dis()` or
  # `new_p_dis()`).
  x <- round(x_tbl[["x"]], digits = 10)

  if (anyDuplicated(x) != 0) {
    # `x_tbl[["x"]]` is already sorted, so `vals` is automatically sorted too,
    # i.e. no need for `sort()`.
    vals <- unique(x)
    x_val_id <- match(x, vals)

    # `as.vector()` is used to remove extra attributes
    prob <- as.vector(tapply(x_tbl[["prob"]], x_val_id, sum))
    prob <- prob / sum(prob)

    data.frame(x = vals, prob = prob, cumprob = cumsum(prob))
  } else {
    res <- data.frame(
      x = x,
      prob = impute_prob(x_tbl[["prob"]])
    )
    res[["cumprob"]] <- impute_vec(
      vec = x_tbl[["cumprob"]], new_vec = cumsum(res[["prob"]])
    )

    res
  }
}

impute_x_tbl_impl_con <- function(x_tbl) {
  res <- data.frame(
    x = x_tbl[["x"]],
    y = impute_y(x_tbl[["y"]], x_tbl[["x"]])
  )
  res[["cumprob"]] <- impute_vec(
    vec = x_tbl[["cumprob"]],
    new_vec = trapez_part_integral(res[["x"]], res[["y"]])
  )

  res
}

# Extra property checks are needed to avoid creating unnecessary copies
impute_prob <- function(prob) {
  tot_prob <- sum(prob)

  if (is_near(tot_prob, 1)) {
    prob
  } else {
    prob / tot_prob
  }
}

impute_y <- function(y, x) {
  tot_prob <- trapez_integral(x, y)

  if (is_near(tot_prob, 1)) {y} else {y / tot_prob}
}

impute_vec <- function(vec, new_vec) {
  if (is.null(vec) || !all(is_near(vec, new_vec))) {
    new_vec
  } else {
    vec
  }
}


# Get "x_tbl" info --------------------------------------------------------
# Most of the functions here assume "x_tbl" to be a "proper" one (`is_x_tbl()`
# with appropriate `type` returns `TRUE`)
get_x_tbl_sec_col <- function(x_tbl) {
  if ("prob" %in% names(x_tbl)) {
    "prob"
  } else {
    "y"
  }
}

get_type_from_x_tbl <- function(x_tbl) {
  if (get_x_tbl_sec_col(x_tbl) == "prob") {
    "discrete"
  } else {
    "continuous"
  }
}


# Modify "x_tbl" ----------------------------------------------------------
# Most of the functions here assume "x_tbl" to be a "proper" one (`is_x_tbl()`
# returns `TRUE`)
filter_x_tbl <- function(x_tbl, support) {
  is_x_in_support <- (x_tbl[["x"]] >= support[1]) & (x_tbl[["x"]] <= support[2])

  x_tbl[is_x_in_support, ]
}

union_inside_x_tbl <- function(x_tbl_orig, x_tbl_new) {
  # Remove rows from `x_tbl_new` which are outside from `x_tbl_orig`'s support
  orig_supp <- range(x_tbl_orig[["x"]])
  x_tbl_new <- filter_x_tbl(x_tbl_new, orig_supp)

  second_col <- get_x_tbl_sec_col(x_tbl_orig)

  res <- data.frame(x = c(x_tbl_orig[["x"]], x_tbl_new[["x"]]))
  res[[second_col]] <- c(x_tbl_orig[[second_col]], x_tbl_new[[second_col]])

  # Remove rows with duplicate "x" (leaving in output rows from `x_tbl_orig`)
  res <- res[!duplicated(res[["x"]]), ]

  res <- res[order(res[["x"]]), ]
  row.names(res) <- NULL

  res
}

reflect_x_tbl <- function(x_tbl, around) {
  res <- x_tbl[rev(seq_len(nrow(x_tbl))), ]
  res[["x"]] <- around + (around - res[["x"]])

  x_tbl_probs <- switch(
    get_type_from_x_tbl(x_tbl),
    discrete = diff(c(0, x_tbl[["cumprob"]])),
    continuous = diff(c(x_tbl[["cumprob"]], 1))
  )
  res[["cumprob"]] <- cumsum(rev(x_tbl_probs))

  row.names(res) <- NULL

  res
}

ground_x_tbl <- function(x_tbl, dir = "both", h = 1e-8) {
  if (get_type_from_x_tbl(x_tbl) == "discrete") {
    return(x_tbl)
  }

  x <- x_tbl[["x"]]
  y <- x_tbl[["y"]]
  n <- nrow(x_tbl)

  add_left <- (dir %in% c("left", "both")) && !is_zero(y[1])
  add_right <- (dir %in% c("right", "both")) && !is_zero(y[n])

  if (add_left) {
    if (add_right) {
      res <- x_tbl[c(1, 1:n, n), ]
      res[["x"]][c(1, n+2)] <- x[c(1, n)] + h*c(-1, 1)
      res[["y"]][c(1, n+2)] <- 0
    } else {
      res <- x_tbl[c(1, 1:n), ]
      res[["x"]][1] <- x[1] - h
      res[["y"]][1] <- 0
    }
  } else {
    if (add_right) {
      res <- x_tbl[c(1:n, n), ]
      res[["x"]][n+1] <- x[n] + h
      res[["y"]][n+1] <- 0
    } else {
      res <- x_tbl
    }
  }

  rownames(res) <- NULL
  res
}

add_x_tbl_knots <- function(x_tbl, at_x, only_inside = TRUE) {
  # Ensure that present knots aren't get duplicated
  at_x <- setdiff(at_x, x_tbl[["x"]])

  # Add only knots inside current range if `only_inside` is `TRUE`
  if (only_inside) {
    supp <- range(x_tbl[["x"]])
    at_x <- at_x[(at_x > supp[1]) & (at_x < supp[2])]
  }

  if (length(at_x) == 0) {
    return(x_tbl)
  }

  new_x <- c(x_tbl[["x"]], at_x)
  new_y <- c(x_tbl[["y"]], enfun_x_tbl(x_tbl)(at_x))
  x_order <- order(new_x)

  data.frame(x = new_x[x_order], y = new_y[x_order])
}


# Compute based on "x_tbl" ------------------------------------------------
# This helper is code-lighter and faster than `new_d()`
enfun_x_tbl <- function(x_tbl) {
  stats::approxfun(
    x_tbl[["x"]], x_tbl[["y"]], method = "linear", yleft = 0, yright = 0
  )
}


# Stack "x_tbl"s (sum densities/probabilities) ----------------------------
stack_x_tbl <- function(x_tbl_list) {
  # It is assumed that all "x_tbl"s have the same type
  type <- get_type_from_x_tbl(x_tbl_list[[1]])

  res <- switch(
    type,
    discrete = stack_x_tbl_dis(x_tbl_list),
    continuous = stack_x_tbl_con(x_tbl_list)
  )
  row.names(res) <- NULL

  res
}

stack_x_tbl_dis <- function(x_tbl_list) {
  x <- unlist(lapply(x_tbl_list, `[[`, i = "x"))
  prob <- unlist(lapply(x_tbl_list, `[[`, i = "prob"))

  if (anyDuplicated(x) != 0) {
    vals <- sort(unique(x))
    x_val_id <- match(x, vals)
    prob <- as.vector(tapply(prob, x_val_id, sum))
    x <- vals
  }

  data.frame(x = x, prob = prob)
}

stack_x_tbl_con <- function(x_tbl_list) {
  x_tbl_funs <- lapply(x_tbl_list, enfun_x_tbl)

  x <- unlist(lapply(x_tbl_list, function(x_tbl) {
    # Grounding is needed to ensure that `x_tbl` doesn't affect its outside
    ground_x_tbl(x_tbl)[["x"]]
  }))
  x <- sort(unique(x))

  y_at_x <- lapply(x_tbl_funs, do.call, list(x))
  y_mat <- matrix(unlist(y_at_x), nrow = length(x))
  y <- rowSums(y_mat)

  res <- data.frame(x = x, y = y)

  # Ensure that zero probability edges (possibly created during "grounding")
  # aren't newly created
  remove_extra_edges(res, x_tbl_list)
}

remove_extra_edges <- function(x_tbl, x_tbl_list) {
  n <- nrow(x_tbl)

  x_left <- x_tbl[["x"]][1]
  x_left_is_extra <- is_x_extra(x_left, x_tbl_list)

  x_right <- x_tbl[["x"]][n]
  x_right_is_extra <- is_x_extra(x_right, x_tbl_list)

  rows_to_remove <- c(1, n)[c(x_left_is_extra, x_right_is_extra)]

  if (length(rows_to_remove) > 0) {
    x_tbl[-rows_to_remove, ]
  } else {
    x_tbl
  }
}

is_x_extra <- function(x, x_tbl_list) {
  x_is_present <- vapply(x_tbl_list, function(x_tbl) {
    x %in% x_tbl[["x"]]
  }, logical(1))

  !any(x_is_present)
}
