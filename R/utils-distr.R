# Common functionality for `*_fun()` --------------------------------------
distr_impl <- function(fun_class, impl_funs, x, type, attach_sample, extra,
                       ...) {
  assert_common_args(x, type, attach_sample)

  fun <- switch(
    type,
    raw = impl_funs[["raw"]](x),
    smooth = impl_funs[["smooth"]](x, ...)
  )

  res <- add_common_meta(
    fun, sample = x, type = type, attach_sample = attach_sample, extra = extra
  )

  structure(res, class = c(fun_class, "function"))
}

distr_print <- function(fun_name, x, ...) {
  meta_names <- glue::glue_collapse(names(meta(x)), sep = ", ")
  type_mod <- switch(
    meta(x, "type"),
    raw = "raw",
    smooth = "smoothed"
  )

  cat(glue_null(
    '{fun_name} based on {type_mod} input\n',
    'Meta data has following elements: {meta_names}'
  ))
  cat("\n")
  attributes(x) <- NULL

  print(x, ...)
}

assert_common_args <- function(x, type, attach_sample) {
  assert_type(x, is.numeric)

  assert_type(type, rlang::is_string)
  if (!(type %in% c("raw", "smooth"))) {
    stop_glue('`type` should be one of "raw" or "smooth", not {type}.')
  }

  assert_type(
    attach_sample, function(x) {identical(x, TRUE) || identical(x, FALSE)},
    "`TRUE` or `FALSE`"
  )

  x
}

add_common_meta <- function(obj, sample, type = "smooth", attach_sample = FALSE,
                            extra = NULL) {
  res <- add_meta_cond(obj, attach_sample, sample = sample)
  res <- add_meta_cond(res, !is.null(extra), extra = extra)
  res <- add_meta(res, type = type)

  res
}


# Construct discrete distribution table -----------------------------------
compute_distr_tbl <- function(x) {
  x_tbl <- table(round(x, digits = 8))
  x_prob <- as.numeric(x_tbl) / length(x)

  data.frame(x = as.numeric(names(x_tbl)), prob = x_prob)
}


# Extended density --------------------------------------------------------
# Ensures continuous linear interpolation by adding first and last points
# with value 0.
density_ext <- function(x, ...) {
  dens <- stats::density(x, ...)

  x_dens <- dens[["x"]]
  n <- length(x_dens)
  max_offset <- 10^(-4)
  if (n == 1) {
    offset <- rep(max_offset, 2)
  } else {
    x_width_first <- diff(utils::head(x_dens, 2))
    x_width_last <- diff(utils::tail(x_dens, 2))
    offset <- pmin(max_offset, c(x_width_first, x_width_last))
  }

  new_x <- c(x_dens[1] - offset[1], x_dens, x_dens[n] + offset[2])
  new_y <- c(0, dens[["y"]], 0)

  new_integral <- trapez_integral(new_x, new_y)

  list(x = new_x, y = new_y / new_integral)
}

trapez_integral <- function(x, y) {
  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1))) / 2
}
