#' Transform pdqr-function
#'
#' Perform a transformation of pdqr-function(s) (which assumed to be
#' independent).
#'
#' @param f_list A list consisting from pdqr-function(s) and/or single
#'   number(s). Should have at least one pdqr-function (see Details).
#' @param trans Transformation function. Should take as many (vectorized)
#'   arguments as there are elements in `f_list` or a single argument for
#'   `form_trans_self()`. Should return numeric or logical values.
#' @param ... Extra arguments to `trans`.
#' @param method Transformation method. One of "random" or "bruteforce".
#' @param n_sample Number of elements to sample.
#' @param args_new List of extra arguments for [new_*()][new_d()] to control
#'   [density()].
#' @param f A pdqr-function.
#'
#' @details `form_trans_self()` is a thin wrapper for `form_trans()` that
#' accepts a single pdqr-function instead of a list of them.
#'
#' [Class][meta_class()] of output is chosen as class of first pdqr-function in
#' `f_list`. [Type][meta_type()] of output is chosen to be "discrete" in case
#' all input pdqr-functions have "discrete" type, and "continuous" otherwise.
#'
#' Method "random" performs transformation using random generation of samples:
#' - **Generates a sample of size `n_sample` from every element of `f_list`**
#' (if element is single number, it is repeated `n_sample` times).
#' - **Calls `trans`** with all generated samples (in order aligned with
#' `f_list`). **Note** that output should be either numeric or logical and have
#' `n_sample` elements (one for each combination of input values in "vectorized"
#' fashion). So, for example, using `sum` directly is not possible as it returns
#' only single number.
#' - **Creates output pdqr-function**. If output is logical, probability of
#' being true is estimated as share of `TRUE` in output, and boolean
#' pdqr-function is created (type "discrete" with "x" values equal to 0 and 1,
#' and probabilities of being false and true respectively). If output is
#' numeric, one of `new_*()` (suitable for output class) is called with
#' arguments from `args_new` list.
#'
#' Method "bruteforce":
#' - **[Retypes][form_retype()] input** pdqr-function to "discrete"
#' type (using "piecelin" method).
#' - **Computes output for every combination of "x" values** (probability of
#' which will be a product of corresponding probabilities).
#' - **Creates pdqr-function of type "discrete"** with suitable `new_*()`
#' function.
#' - **Possibly retypes to "continuous" type** if output should have it (also
#' with "piecelin" method).
#'
#' **Notes** about "bruteforce" method:
#' - Its main advantage is that it is not random.
#' - It may start to be very memory consuming very quickly.
#' - It is usually useful when type of output function is "discrete". In case of
#' "continuous" type, retyping from "discrete" to "continuous" might introduce
#' big errors.
#' - Used "discrete" probabilities shouldn't be very small because they will be
#' directly multiplied, which might cause numerical accuracy issues.
#'
#' @return A pdqr-function for transformed random variable.
#'
#' @seealso [Pdqr methods for S3 group generic functions][methods-group-generic]
#'   for more accurate implementations of most commonly used functions. Some of
#'   them are direct (without randomness) and some of them use `form_trans()`
#'   with "random" method.
#'
#' [form_regrid()] to increase/decrease granularity of pdqr-functions for method
#' "bruteforce".
#'
#' @family form functions
#'
#' @examples
#' # Default "random" transformation
#' d_norm <- as_d(dnorm)
#' ## More accurate result would give use of `+` directly with: d_norm + d_norm
#' d_norm_2 <- form_trans(list(d_norm, d_norm), trans = `+`)
#' plot(d_norm_2)
#' lines(as_d(dnorm, sd = sqrt(2)), col = "red")
#'
#' ## Input list can have single numbers
#' form_trans(list(d_norm, 100), trans = `+`)
#'
#' ## Output of `trans` can be logical. Next example is random version of
#' ## `d_norm >= 0`.
#' form_trans(list(d_norm, 0), trans = `>=`)
#'
#' # Transformation with "bruteforce" method
#' power <- function(x, n = 1) {
#'   x^n
#' }
#' p_dis <- new_p(
#'   data.frame(x = 1:3, prob = c(0.1, 0.2, 0.7)),
#'   type = "discrete"
#' )
#'
#' p_dis_sq <- form_trans_self(
#'   p_dis, trans = power, n = 2, method = "bruteforce"
#' )
#' meta_x_tbl(p_dis_sq)
#' ## Compare with "random" method
#' p_dis_sq_rand <- form_trans_self(p_dis, trans = power, n = 2)
#' meta_x_tbl(p_dis_sq_rand)
#'
#' # `form_trans_self()` is a wrapper for `form_trans()`
#' form_trans_self(d_norm, trans = function(x) {
#'   2 * x
#' })
#' @name form_trans
NULL

#' @rdname form_trans
#' @export
form_trans <- function(f_list, trans, ..., method = "random", n_sample = 10000,
                       args_new = list()) {
  assert_f_list(f_list, allow_numbers = TRUE)
  assert_missing(trans, "transformation function")
  assert_type(trans, is.function)
  assert_method(method, methods_trans)
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

methods_trans <- c("random", "bruteforce")

#' @rdname form_trans
#' @export
form_trans_self <- function(f, trans, ..., method = "random",
                            args_new = list()) {
  assert_pdqr_fun(f)

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
      function(n) {
        rep(f, length.out = n)
      }
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
    prob_true <- mean(smpl, na.rm = TRUE)

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

  # Retype `f_list` to "discrete" type. If element is number, create dirac-like
  # function where `prob = 1`.
  f_list <- lapply(f_list, function(f) {
    if (is_single_number(f)) {
      new_p(f, "discrete")
    } else {
      form_retype(f, type = "discrete", method = "piecelin")
    }
  })
  x_tbl_list <- lapply(f_list, meta_x_tbl)

  # Create grids for `x` and `prob`
  x_list <- lapply(x_tbl_list, function(x_tbl) {
    x_tbl[["x"]]
  })
  x_grid <- list_grid(x_list)

  prob_list <- lapply(x_tbl_list, function(x_tbl) {
    x_tbl[["prob"]]
  })
  prob_grid <- list_grid(prob_list)

  # Compute "x_tbl" for output "discrete" function
  x_new <- do.call(trans, args = c(x_grid, list(...)))
  assert_trans_output(x_new)
  prob_new <- Reduce(f = `*`, x = prob_grid[-1], init = prob_grid[[1]])

  # Return boolean pdqr-function in case output of `trans` is logical
  if (is.logical(x_new)) {
    prob_true <- sum(prob_new[x_new], na.rm = TRUE)

    return(boolean_pdqr(prob_true, res_meta[["class"]]))
  }

  # Create transformed "discrete" pdqr-function. Usage of `args_new` doesn't
  # have any effect for now (as input to `new_*()` is data frame), but might be
  # helpful if in future `new_*()` will have new arguments.
  x_tbl <- data.frame(x = x_new, prob = prob_new)
  new_pdqr <- new_pdqr_by_class(res_meta[["class"]])
  call_args <- c_dedupl(list(x = x_tbl, type = "discrete"), args_new)
  dis_res <- do.call(new_pdqr, args = call_args)

  # Retype back to input type
  form_retype(dis_res, res_meta[["type"]], method = "piecelin")
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
