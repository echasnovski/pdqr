#' @rdname as-pdqr
#' @export
as_r <- function(f, ...) {
  UseMethod("as_r")
}

#' @rdname as-pdqr
#' @export
as_r.default <- function(f, support = NULL, ..., n_grid = 10001,
                         n_sample = 10000, args_new = list()) {
  assert_as_def_args(f, support, n_grid)
  assert_type(
    n_sample, is_single_number,
    type_name = "single number more than 1", min_val = 2
  )
  assert_type(args_new, is.list)

  # Attempt to detect "honored" distribution
  f_name <- deparse(substitute(f))
  honored_distr <- as_honored_distr(
    "r", f_name, f, support, ..., n_grid = n_grid
  )
  if (!is.null(honored_distr)) {
    return(honored_distr)
  }

  # Treate `f` as unknown r-function
  # Create sample
  smpl <- f(n_sample, ...)

  # Detect support
  support <- detect_support_r(smpl, format_support(support))

  # Create density function from sample
  new_call_args <- c_dedupl(list(x = smpl, type = "continuous"), args_new)
  d_f <- do.call(new_d, new_call_args)

  # Adjust to supplied support
  d_f <- as_d(unpdqr(d_f), support = support, n_grid = n_grid)

  # Convert to r-function
  as_r(d_f)
}

#' @rdname as-pdqr
#' @export
as_r.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  new_r(x = meta_x_tbl(f), type = meta_type(f))
}

detect_support_r <- function(smpl, supp) {
  mean_diff <- mean(diff(sort(smpl)))
  smpl_support <- range(smpl) + mean_diff * c(-1, 1)

  supp <- coalesce_pair(supp, smpl_support)

  if (!is_support(supp) || (supp[1] == supp[2])) {
    stop_collapse(
      "Detected support isn't proper. Usually this is because input random ",
      "generation function isn't proper."
    )
  }

  supp
}
