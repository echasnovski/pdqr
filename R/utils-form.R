# 'pdqr' classes functionality --------------------------------------------
new_pdqr_by_class <- function(pdqr_class) {
  switch(
    pdqr_class,
    p = new_p, d = new_d, q = new_q, r = new_r,
    stop_collapse("Wrong 'pdqr' class.")
  )
}

new_pdqr_by_ref <- function(f) {
  new_pdqr_by_class(get_pdqr_class(f))
}

as_pdqr_by_class <- function(pdqr_class) {
  switch(
    pdqr_class,
    p = as_p, d = as_d, q = as_q, r = as_r,
    stop_collapse("Wrong 'pdqr' class.")
  )
}

as_pdqr_by_ref <- function(f) {
  as_pdqr_by_class(get_pdqr_class(f))
}

get_pdqr_class <- function(f) {
  pdqr_classes <- c("p", "d", "q", "r")
  f_type <- pdqr_classes[match(class(f), pdqr_classes)]

  f_type[!is.na(f_type)][1]
}


# Custom constructors -----------------------------------------------------
boolean_pdqr <- function(prob_true, pdqr_class) {
  x_tbl <- data.frame(x = c(0, 1), prob = c(1-prob_true, prob_true))

  new_pdqr_by_class(pdqr_class)(x_tbl, "fin")
}


# Handling list of pdqr-functions -----------------------------------------
assert_f_list <- function(f_list, allow_numbers = FALSE) {
  f_list_name <- paste0("`", deparse(substitute(f_list)), "`")

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
  is_elem_pdqr <- vapply(f_list, is_pdqr_fun, logical(1))
  type_vec <- vapply(f_list[is_elem_pdqr], meta_type, character(1))

  # Combined type is "fin" only if all inputs are "fin"
  res_type <- if (all(type_vec == "fin")) {"fin"} else {"infin"}

  # Combined class is the class of first pdqr-function (which should be present
  # due to call to `assert_f_list()`)
  first_pdqr <- f_list[[which(is_elem_pdqr)[1]]]
  res_class <- get_pdqr_class(first_pdqr)

  list(type = res_type, class = res_class)
}
