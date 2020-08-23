# 'pdqr' classes functionality --------------------------------------------
new_pdqr_by_class <- function(pdqr_class) {
  switch(
    pdqr_class,
    p = new_p, d = new_d, q = new_q, r = new_r,
    stop_collapse("Wrong 'pdqr' class.")
  )
}

new_pdqr_by_ref <- function(f) {
  new_pdqr_by_class(meta_class(f))
}

as_pdqr_by_class <- function(pdqr_class) {
  switch(
    pdqr_class,
    p = as_p, d = as_d, q = as_q, r = as_r,
    stop_collapse("Wrong 'pdqr' class.")
  )
}

as_pdqr_by_ref <- function(f) {
  as_pdqr_by_class(meta_class(f))
}


# Custom constructors -----------------------------------------------------
boolean_pdqr <- function(prob_true, pdqr_class) {
  x_tbl <- data.frame(x = c(0, 1), prob = c(1 - prob_true, prob_true))

  new_pdqr_by_class(pdqr_class)(x_tbl, "discrete")
}


# Handling list of pdqr-functions -----------------------------------------
assert_f_list <- function(f_list, allow_numbers = FALSE, f_list_name = NULL) {
  if (dont_assert()) {
    return(TRUE)
  }

  if (is.null(f_list_name)) {
    f_list_name <- enbacktick(deparse(substitute(f_list)))
  }

  if (missing(f_list)) {
    if (allow_numbers) {
      value_name <- "list of pdqr-function(s) (with possible numbers)"
    } else {
      value_name <- "list of pdqr-function(s)"
    }

    error_missing(var_name = f_list_name, value_name = value_name)
  }

  if (!is.list(f_list)) {
    stop_collapse(f_list_name, " must be 'list', not '", get_type(f_list), "'.")
  }

  if (length(f_list) == 0) {
    stop_collapse(f_list_name, " shouldn't be empty.")
  }

  elem_is_pdqr <- vapply(f_list, is_pdqr_fun, logical(1))

  if (allow_numbers) {
    elem_is_number <- vapply(f_list, is_single_number, logical(1))

    if (!all(elem_is_pdqr | elem_is_number)) {
      stop_collapse(
        f_list_name, " should contain only pdqr-functions or single numbers."
      )
    }

    # Restricting the case when all elements are numbers
    if (sum(elem_is_pdqr) == 0) {
      stop_collapse(f_list_name, " should have at least one pdqr-function.")
    }
  } else {
    if (!all(elem_is_pdqr)) {
      stop_collapse(f_list_name, " should contain only pdqr-functions.")
    }
  }

  TRUE
}

compute_f_list_meta <- function(f_list) {
  # Note that it is assumed that `f_list` contains only pdqr-functions or single
  # numbers. Currently it should be pretested with
  # `assert_f_list(f_list, allow_numbers = TRUE)`
  # Main reason behind it is a wish to avoid usage of possibly "computantionally
  # expensive" `is_pdqr_fun()`.
  is_elem_number <- vapply(f_list, is_single_number, logical(1))
  is_elem_pdqr <- !is_elem_number
  type_vec <- vapply(f_list[is_elem_pdqr], meta_type, character(1))

  # Combined type is "discrete" only if all inputs are "discrete"
  res_type <- if (all(type_vec == "discrete")) {
    "discrete"
  } else {
    "continuous"
  }

  # Combined class is the class of first pdqr-function (which should be present
  # due to call to `assert_f_list()`)
  first_pdqr <- f_list[[which(is_elem_pdqr)[1]]]
  res_class <- meta_class(first_pdqr)

  list(type = res_type, class = res_class)
}


# Create data from pdqr-pair ----------------------------------------------
intersection_x <- function(f, g) {
  f_x <- meta_x_tbl(f)[["x"]]
  g_x <- meta_x_tbl(g)[["x"]]
  f_supp <- meta_support(f)
  g_supp <- meta_support(g)

  g_x <- g_x[(g_x >= f_supp[1]) & (g_x <= f_supp[2])]
  f_x <- f_x[(f_x >= g_supp[1]) & (f_x <= g_supp[2])]

  sort(union(f_x, g_x))
}

union_x <- function(f, g) {
  f_x <- meta_x_tbl(f)[["x"]]
  g_x <- meta_x_tbl(g)[["x"]]

  sort(union(f_x, g_x))
}


intersection_support <- function(f, g) {
  f_supp <- meta_support(f)
  g_supp <- meta_support(g)

  left <- max(f_supp[1], g_supp[1])
  right <- min(f_supp[2], g_supp[2])

  if (left > right) {
    return(numeric(0))
  } else {
    c(left, right)
  }
}

union_support <- function(f, g) {
  range(meta_support(f), meta_support(g))
}
