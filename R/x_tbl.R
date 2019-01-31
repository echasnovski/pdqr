# Compute "x_tbl" ---------------------------------------------------------
compute_x_tbl <- function(x, type, ...) {
  switch(
    type,
    fin = compute_x_tbl_fin(x),
    infin = compute_x_tbl_infin(x, ...)
  )
}

compute_x_tbl_fin <- function(x, vals = sort(unique(x))) {
  x <- x[!is.na(x)]

  x_val_id <- match(x, vals)
  prob <- tabulate(x_val_id) / length(x)
  cumprob <- cumsum(prob)

  data.frame(x = vals, prob = prob, cumprob = cumprob)
}

compute_x_tbl_infin <- function(x, ...) {
  if (length(x) < 2) {
    stop_collapse(
      'There should be at least 2 values in `x` for `type` "infin".'
    )
  }

  res <- density_piecelin(x, ...)
  res[["cumprob"]] <- trapez_part_integral(res[["x"]], res[["y"]])

  res
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

  if (type == "fin") {
    impute_x_tbl_impl_fin(x_tbl)
  } else if (type == "infin") {
    impute_x_tbl_impl_infin(x_tbl)
  } else {
    stop("Wrong `type`.")
  }
}

impute_x_tbl_impl_fin <- function(x_tbl) {
  if (anyDuplicated(x_tbl[["x"]])) {
    # `x_tbl[["x"]]` is already sorted, so `vals` is automatically sorted too,
    # i.e. no need for `sort()`
    vals <- unique(x_tbl[["x"]])
    x_val_id <- match(x_tbl[["x"]], vals)

    # `as.vector()` is used to remove extra attributes
    prob <- as.vector(tapply(x_tbl[["prob"]], x_val_id, sum))
    prob <- prob / sum(prob)

    res <- data.frame(x = vals, prob = prob)
  } else {
    res <- data.frame(
      x = x_tbl[["x"]],
      prob = impute_prob(x_tbl[["prob"]])
    )
  }

  res[["cumprob"]] <- impute_vec(
    vec = x_tbl[["cumprob"]], new_vec = cumsum(res[["prob"]])
  )

  res
}

impute_x_tbl_impl_infin <- function(x_tbl) {
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
    "fin"
  } else {
    "infin"
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
    fin = diff(c(0, x_tbl[["cumprob"]])),
    infin = diff(c(x_tbl[["cumprob"]], 1))
  )
  res[["cumprob"]] <- cumsum(rev(x_tbl_probs))

  row.names(res) <- NULL

  res
}

ground_x_tbl <- function(x_tbl, dir = "both", h = 1e-8) {
  if (get_type_from_x_tbl(x_tbl) == "fin") {
    return(x_tbl)
  }

  x <- x_tbl[["x"]]
  y <- x_tbl[["y"]]
  n <- nrow(x_tbl)

  add_left <- (dir %in% c("left", "both")) && (y[1] != 0)
  add_right <- (dir %in% c("right", "both")) && (y[n] != 0)

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
