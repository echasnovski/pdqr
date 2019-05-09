# Notes in docs about `method = "bruteforce"`:
# - It is very memory consuming.
# - It is usually useful when type of output function is "fin", i.e. when all
#   input functions are "fin" or when output of `trans` is logical. Also
#   probabilities shouldn't be very small because they will be directly
#   multiplied.

#' Transform pdqr-function
#'
#' Perform a transformation of pdqr-function(s).
#'
#' @param f_list A list consisting from pdqr-function(s) and/or number(s).
#'   Should have at least one pdqr-function (see Details).
#' @param f A pdqr-function.
#' @param trans Transformation function. Should take as many (vectorized)
#'   arguments as there are elements in `f_list`.
#' @param ... Extra arguments to `trans`.
#' @param method Transformation method. One of "random" or "bruteforce".
#' @param n_sample Number of elements to sample.
#' @param args_new List of extra arguments for [new_*()][new_d()] to control
#'   [density()].
#'
#' @return A pdqr-function for transformed random variable.
#'
#' @examples
#' # Default "random" transformation
#' d_norm <- as_d(dnorm)
#' d_norm_2 <- form_trans(list(d_norm, d_norm), trans = `+`)
#' plot(d_norm_2)
#' lines(as_d(dnorm, sd = 2), col = "red")
#'
#' # Transformation with "bruteforce" method
#' power <- function(x, n = 1) {x^n}
#' p_fin <- new_p(data.frame(x = 1:3, prob = c(0.1, 0.2, 0.7)), type = "fin")
#'
#' p_fin_sq <- form_trans_self(
#'   p_fin, trans = power, n = 2, method = "bruteforce"
#' )
#' meta_x_tbl(p_fin_sq)
#'   # Compare with "random" method
#' p_fin_sq_rand <- form_trans_self(p_fin, trans = power, n = 2)
#' meta_x_tbl(p_fin_sq_rand)
#'
#' @name form_trans
NULL

#' @rdname form_trans
#' @export
form_trans <- function(f_list, trans, ..., method = "random", n_sample = 10000,
                       args_new = list()) {
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

#' @rdname form_trans
#' @export
form_trans_self <- function(f, trans, ..., method = "random",
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

  # Return boolean pdqr-function in case output of `trans` is logical
  if (is.logical(smpl)) {
    prob_true <- mean(smpl)
    return(boolean_pdqr(prob_true, res_meta[["class"]]))
  }

  # Produce output pdqr function
  new_pdqr <- new_pdqr_by_class(res_meta[["class"]])
  call_args <- c_dedupl(list(x = smpl, type = res_meta[["type"]]), args_new)

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
  prob_new <- Reduce(f = `*`, x = prob_grid[-1], init = prob_grid[[1]])

  # Return boolean pdqr-function in case output of `trans` is logical
  if (is.logical(x_new)) {
    prob_true <- sum(prob_new[x_new])

    return(boolean_pdqr(prob_true, res_meta[["class"]]))
  }

  # Create transformed "fin" pdqr-function. Usage of `args_new` doesn't have
  # any effect for now (as input to `new_*()` is data frame), but might be
  # helpful if in future `new_*()` will have new arguments.
  x_tbl <- data.frame(x = x_new, prob = prob_new)
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
