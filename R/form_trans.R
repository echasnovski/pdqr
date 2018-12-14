# Main transformation function --------------------------------------------
pdqr_transform <- function(trans, ..., .n_sample = 10000,
                           .pdqr_class = NULL,
                           .pdqr_args = list(attach_x = FALSE)) {
  assert_type(trans, is.function)
  dots <- list(...)
  assert_trans_dots(dots)
  assert_type(.n_sample, is_single_number, type = "single number")
  assert_type(.pdqr_class, is_string, allow_null = TRUE)
  assert_type(.pdqr_args, is.list)

  # Convert to random generation functions.
    # If `dots` entry isn't "pdqr" function than it is a numeric.
  r_dots <- lapply(dots, function(cur_dot) {
    if (is_pdqr_fun(cur_dot)) {
      as_r(cur_dot)
    } else {
      function(n) {rep(cur_dot, length.out = n)}
    }
  })
  # Generate samples for every such function
  smpl_list <- lapply(r_dots, do.call, args = list(.n_sample))
  # Call `trans` with all generated samples to produce sample from transformed
  # distribution
  smpl <- do.call(trans, smpl_list)

  # Produce output pdqr function
  ref_f <- find_ref_f(dots)
  pdqr_fun <- impute_pdqr_fun(.pdqr_class, ref_f)
  pdqr_call_args <- dedupl_list(c(
    list(x = smpl),
    .pdqr_args,
    list(
      type = meta(ref_f, "type"),
      attach_x = has_meta_x(ref_f),
      extra = meta(ref_f, "extra")
    )
  ))

  do.call(pdqr_fun, pdqr_call_args)
}

assert_trans_dots <- function(dots_list) {
  pdqr_check <- vapply(dots_list, is_pdqr_fun, logical(1))
  num_check <- vapply(dots_list, is_single_number, logical(1))

  if (!all(pdqr_check | num_check)) {
    stop_collapse(
      '`...` should contain only "pdqr" functions and single numbers.'
    )
  }

  if (!any(pdqr_check)) {
    stop_collapse("`...` should have at least one", '"pdqr" function.')
  }

  dots_list
}

find_ref_f <- function(obj_list) {
  is_obj_pdqr <- vapply(obj_list, is_pdqr_fun, logical(1))

  obj_list[[which(is_obj_pdqr)[1]]]
}

impute_pdqr_fun <- function(pdqr_class, ref) {
  if (is.null(pdqr_class)) {
    pdqr_class <- get_pdqr_class(ref)
  } else if (!is_pdqr_class(pdqr_class)) {
    stop_collapse(
      '`.pdqr_class` should be one of "p_fun", "d_fun", "q_fun", "r_fun".'
    )
  }

  switch(
    pdqr_class,
    p_fun = new_p, d_fun = new_d, q_fun = new_q, r_fun = new_r
  )
}

get_pdqr_class <- function(f) {
  pdqr_classes <- paste0(c("p", "d", "q", "r"), "_fun")
  f_type <- pdqr_classes[match(class(f), pdqr_classes)]

  f_type[!is.na(f_type)][1]
}


# Group generics ----------------------------------------------------------
Math.pdqr_fun <- function(x, ...) {
  n_sample <- getOption("pdqr.transform.n_sample")

  gen_fun <- function(y) {
    get(.Generic)(y, ...)
  }

  pdqr_transform(gen_fun, x, .n_sample = n_sample)
}

Ops.pdqr_fun <- function(e1, e2) {
  n_sample <- getOption("pdqr.transform.n_sample")

  gen_fun <- function(...) {
    res <- get(.Generic)(...)
    if (!is.numeric(res)) {
      warning_collapse(
        "Output of `", .Generic, "` is '", get_type(res), "'. ",
        "Converting to 'numeric'."
      )
      res <- as.numeric(res)
    }

    res
  }

  if (missing(e2)) {
    pdqr_transform(gen_fun, e1, .n_sample = n_sample)
  } else {
    pdqr_transform(gen_fun, e1, e2, .n_sample = n_sample)
  }
}

Summary.pdqr_fun <- function(..., na.rm = FALSE) {
  n_sample <- getOption("pdqr.transform.n_sample")

  if (.Generic == "range") {
    stop_collapse(
      "`range()` can't be applied to ", '"pdqr" functions as it returns two ',
      "numbers. Use `min()` and `max()`."
    )
  }

  gen_fun <- function(...) {
    g <- get(.Generic)
    f <- function(...) {g(..., na.rm = na.rm)}

    # `Map()` is needed to "vectorize" `Summary` generics
    unlist(Map(f, ...))
  }

  pdqr_transform(gen_fun, ..., .n_sample = n_sample)
}
