as_r <- function(f, ...) {
  if (inherits(f, "r")) {
    return(f)
  } else if (has_meta_x(f) && has_meta_type(f)) {
    return(distr_from_meta(f, new_r, ...))
  }

  UseMethod("as_r")
}

as_r.default <- function(f, type, support, adjust_max_iter = 10,
                         warn_not_adjusted = TRUE, ...) {
  assert_missing_args(
    "r", type = missing(type), support = missing(support)
  )
  assert_type(adjust_max_iter, is_single_number, "single number")
  assert_type(warn_not_adjusted, is_truefalse, "`TRUE` or `FALSE`")

  adjust_to_support_r <- adjusting_r_impl(adjust_max_iter, warn_not_adjusted)

  as_distr_impl_def("r", f, type, support, adjust_to_support_r)
}

as_r.p <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  assert_pdqr_fun(f)

  warn_conversion_from_p_raw(f, warn_precision, "random generation function")
  q_f <- as_q(f, n_grid = n_grid, warn_precision = FALSE, ...)

  as_r_impl(q_f)
}

as_r.d <- function(f, n_grid = 10001, warn_precision = TRUE, ...) {
  q_f <- as_q(f, n_grid = n_grid, warn_precision = warn_precision, ...)

  as_r_impl(q_f)
}

as_r.q <- function(f, ...) {
  assert_pdqr_fun(f)

  as_r_impl(f)
}

as_r_impl <- function(q_f) {
  res <- function(n) {
    rand_q_vec <- stats::runif(n, min = 0, max = 1)

    q_f(rand_q_vec)
  }
  res <- add_pdqr_class(res, "r")

  copy_meta(res, q_f)
}

adjusting_r_impl <- function(adjust_max_iter, warn_not_adjusted) {
  function(f, type, support) {
    res <- function(n) {
      res <- na_outside_support(f(n), support)
      adjust_iter <- 1

      while((adjust_iter <= adjust_max_iter) && anyNA(res)) {
        is_res_na <- is.na(res)
        res[is_res_na] <- f(sum(is_res_na))
        res <- na_outside_support(res, support)
        adjust_iter <- adjust_iter + 1
      }

      adjust_final(res, support, warn_not_adjusted)
    }

    copy_attrs(res, f)
  }
}

na_outside_support <- function(vec, support) {
  is_outside <- (vec < support[1]) | (vec > support[2])
  vec[is_outside] <- NA

  vec
}

adjust_final <- function(vec, support, warn_not_adjusted) {
  is_vec_na <- is.na(vec)
  if (all(is_vec_na)) {
    stop_collapse(
      'No element was generated inside supplied support. Consider creating ',
      'p-function.'
    )
  }

  if (anyNA(vec)) {
    num_na <- sum(is_vec_na)
    number_message <- num_elems_chr(num_na)

    if (warn_not_adjusted) {
      warning_collapse(
        'After random generation adjustment still ', number_message,
        ' (out of ', length(vec), ') outside of supplied support. ',
        'Replacing with bootstrap.'
      )
    }

    vec[is_vec_na] <- sample(vec[!is_vec_na], num_na, replace = TRUE)
  }

  vec
}

num_elems_chr <- function(n) {
  if (n == 1) {
    "there is 1 element"
  } else {
    paste0("there are ", n, " elements")
  }
}
