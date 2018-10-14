as_impl <- function(distr_fun, distr_class, impl_fun, f, ...) {
  if (class(f)[1] == distr_class) {
    return(f)
  }

  x_meta <- meta(f, "x")
  type_meta <- meta(f, "type")

  if (!(is.null(x_meta) || is.null(type_meta))) {
    distr_fun(
      x = x_meta,
      type = type_meta,
      attach_x = TRUE,
      extra = meta(f, "extra"),
      ...
    )
  } else {
    impl_fun(f, ...)
  }
}

as_distr_impl_def <- function(fun_class, f, type, extra, ...) {
  assert_type(f, is.function)
  assert_distr_type(type)

  res <- add_meta(f, type = type, ...)
  res <- add_meta_cond(res, !is.null(extra), extra = extra)

  structure(res, class = c(fun_class, "function"))
}

as_distr_impl_r <- function(distr_fun, f, n, ...) {
  assert_type(n, is.numeric)

  distr_fun(
    x = f(n),
    type = meta(f, "type"),
    # As `x` isn't attached to `f`, as it is checked in `as_*()` functions
    attach_x = FALSE,
    extra = meta(f, "extra"),
    ...
  )
}

assert_domain <- function(domain, domain_name) {
  if (!(is.numeric(domain) && (length(domain) == 2))) {
    stop_collapse(
      "`", domain_name, "` should be 'numeric with length 2', not '",
      get_type(domain), "'."
    )
  }

  if (domain[1] > domain[2]) {
    stop_collapse(
      "First value in `", domain_name, "` should be not bigger than second one."
    )
  }

  domain
}
