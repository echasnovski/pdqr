# Main transformation function --------------------------------------------
pdqr_transform <- function(trans, ..., .n_sample = 10000,
                           .pdqr_type = NULL,
                           .pdqr_args = list(attach_x = FALSE)) {
  assert_type(trans, is.function)
  dots <- list(...)
  assert_trans_dots(dots)
  assert_type(.n_sample, is_single_number, type = "single number")
  assert_type(.pdqr_type, is_string, allow_null = TRUE)
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
  ref_f <- dots[[1]]
  pdqr_fun <- impute_pdqr_fun(.pdqr_type, ref_f)
  pdqr_call_args <- dedupl_list(c(
    list(x = smpl),
    .pdqr_args,
    list(
      type = meta(ref_f, "type"),
      attach_x = has_meta(ref_f, "x"),
      extra = meta(ref_f, "extra")
    )
  ))

  do.call(pdqr_fun, pdqr_call_args)
}

assert_trans_dots <- function(dots_list) {
  lapply(dots_list, function(cur_dot) {
    if (!(is_pdqr_fun(cur_dot) || is_single_number(cur_dot))) {
      stop_collapse(
        '`...` should contain only "pdqr" functions and single numbers.'
      )
    }
  })

  dots_list
}

impute_pdqr_fun <- function(pdqr_type, ref) {
  if (is.null(pdqr_type)) {
    pdqr_type <- get_pdqr_type(ref)
  } else if (!(pdqr_type %in% c("p_fun", "d_fun", "q_fun", "r_fun" ))) {
    stop_collapse(
      '`.pdqr_type` should be one of "p_fun", "d_fun", "q_fun", "r_fun".'
    )
  }

  switch(
    pdqr_type,
    p_fun = p_fun, d_fun = d_fun, q_fun = q_fun, r_fun = r_fun
  )
}

get_pdqr_type <- function(f) {
  pdqr_types <- paste0(c("p", "d", "q", "r"), "_fun")
  f_type <- pdqr_types[match(class(f), pdqr_types)]

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
