form_trans <- function(f_list, trans, ..., method = "random", n_sample = 10000,
                       args_new = list()) {
  # Notes in docs about `method = "bruteforce"`:
  # - It is very memory consuming.
  # - It is usually useful when type of output function is "fin", i.e. when all
  #   input functions are "fin" or when output of `trans` is logical. Also
  #   probabilities shouldn't be very small because they will be directly
  #   multiplied.

  assert_type(f_list, is.list)
  assert_f_list(f_list, allow_numbers = TRUE)
  assert_type(trans, is.function)
  assert_type(method, is_string)
  assert_in_set(method, c("random", "bruteforce"))
  assert_type(n_sample, is_single_number, type_name = "single number")
  assert_type(args_new, is.list)

  switch(
    method,
    random = trans_random(
      f_list = f_list, trans = trans, ...,
      n_sample = n_sample, args_new = args_new
    ),
    bruteforce = trans_bruteforce(
      f_list = f_list, trans = trans, ...,
      args_new = args_new
    )
  )
}

form_trans_self <- function(f, trans, ..., method = "bruteforce",
                            args_new = list()) {
  form_trans(
    f_list = list(f), trans = trans, ...,
    method = method, args_new = args_new
  )
}

trans_random <- function(f_list, trans, ..., n_sample, args_new) {
  # Compute type and class of output function
  res_meta <- compute_f_list_meta(f_list)

  # Convert to random generation functions. If `f_list` element isn't
  # pdqr-function then it is a number.
  r_dots <- lapply(f_list, function(f) {
    if (is_pdqr_fun(f)) {
      as_r(f)
    } else {
      function(n) {rep(f, length.out = n)}
    }
  })

  # Generate samples for every such function
  smpl_list <- lapply(r_dots, do.call, args = list(n_sample))

  # Call `trans` with all generated samples to produce sample from transformed
  # distribution. Use `...` as additional arguments to `trans`.
  smpl <- do.call(trans, args = c(smpl_list, list(...)))
  assert_trans_output(smpl)
  res_meta <- update_f_list_meta(res_meta, smpl)

  # Produce output pdqr function
  new_pdqr <- new_pdqr_by_class(res_meta[["class"]])
  call_args <- c_dedupl(
    list(x = as.numeric(smpl), type = res_meta[["type"]]),
    args_new
  )

  do.call(new_pdqr, args = call_args)
}

trans_bruteforce <- function(f_list, trans, ..., args_new) {
  # Compute type and class of output function
  res_meta <- compute_f_list_meta(f_list)

  # Retype `f_list` to "fin" type. If element is number, create dirac-like
  # function where `prob = 1`.
  f_list <- lapply(f_list, function(f) {
    if (is_single_number(f)) {
      new_p(f, "fin")
    } else {
      form_retype(f, type = "fin")
    }
  })
  x_tbl_list <- lapply(f_list, meta_x_tbl)

  # Create grids for `x` and `prob`
  x_list <- lapply(x_tbl_list, function(x_tbl) {x_tbl[["x"]]})
  x_grid <- list_grid(x_list)

  prob_list <- lapply(x_tbl_list, function(x_tbl) {x_tbl[["prob"]]})
  prob_grid <- list_grid(prob_list)

  # Compute "x_tbl" for output "fin" function
  x_new <- do.call(trans, args = c(x_grid, list(...)))
  assert_trans_output(x_new)
  res_meta <- update_f_list_meta(res_meta, x_new)

  prob_new <- Reduce(f = `*`, x = prob_grid[-1], init = prob_grid[[1]])

  x_tbl <- data.frame(x = as.numeric(x_new), prob = prob_new)

  # Create transformed "fin" pdqr-function. Usage of `args_new` doesn't have
  # any effect for now (as input to `new_*()` is data frame), but might be
  # helpful if in future `new_*()` will have new arguments.
  new_pdqr <- new_pdqr_by_class(res_meta[["class"]])
  call_args <- c_dedupl(list(x = x_tbl, type = "fin"), args_new)
  fin_res <- do.call(new_pdqr, args = call_args)

  # Retype back to input type
  form_retype(fin_res, res_meta[["type"]])
}

list_grid <- function(vec_list) {
  res <- as.list(do.call(expand.grid, vec_list))
  names(res) <- NULL
  attributes(res) <- NULL

  res
}

assert_trans_output <- function(vec) {
  if (!(is.numeric(vec) || is.logical(vec))) {
    stop_collapse("Output of transformation should be numeric or logical.")
  }

  TRUE
}

update_f_list_meta <- function(f_list_meta, x_vec) {
  if (is.logical(x_vec)) {
    f_list_meta[["type"]] <- "fin"
  }

  f_list_meta
}
