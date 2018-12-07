distr_from_meta <- function(f, new_f, ...) {
  new_f(
    x = meta(f, "x"),
    type = meta(f, "type"),
    attach_x = TRUE,
    extra = meta(f, "extra"),
    ...
  )
}

as_distr_impl_def <- function(fun_class, f, type, support, extra) {
  assert_type(f, is.function)
  assert_distr_type(type)

  res <- add_meta(remove_meta(f), type = type, support = support)
  res <- add_meta_cond(res, !is.null(extra), extra = extra)

  add_pdqr_class(res, fun_class)
}

assert_missing_args <- function(f_name, ...) {
  dots <- list(...)
  missing_args <- names(Filter(isTRUE, dots))

  if (length(missing_args) > 0) {
    stop_collapse(
      'To define "', f_name, '" supply the following arguments: ',
      paste0('`', missing_args, '`', collapse = ", "), '.'
    )
  }

  TRUE
}

as_distr_impl_r <- function(distr_fun, f, n_sample, ...) {
  assert_type(n_sample, is_single_number, type = "single number")

  distr_fun(
    x = f(n_sample),
    type = meta(f, "type"),
    # As `x` isn't attached to `f`, as it is checked in `as_*()` functions
    attach_x = FALSE,
    extra = meta(f, "extra"),
    ...
  )
}

assert_support <- function(support) {
  if (!(is.numeric(support) && (length(support) == 2))) {
    stop_collapse(
      "`support` should be 'numeric with length 2', not '",
      get_type(support), "'."
    )
  }

  if (support[1] > support[2]) {
    stop_collapse(
      "First value in `support` should be not bigger than second one."
    )
  }

  if (any(is.infinite(support))) {
    stop_collapse("`support` should have only finite elements.")
  }

  support
}

warn_conversion_from_p_raw <- function(f, warn_precision, fun_name) {
  if (isTRUE(warn_precision) && inherits(f, "p_fun") &&
      (meta(f, "type") == "raw")) {
    warning_collapse(
      'Converting from cumulative distribution function into ',
      fun_name, ' in case `type` = "raw" and no "x" in metadata is not ',
      'precise around actual raw values. Consider attaching `x` to input.'
    )
  }

  TRUE
}
