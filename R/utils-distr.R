# Common functionality for `*_fun()` --------------------------------------
distr_impl <- function(fun_class, impl_funs, x, type, attach_x, extra,
                       ...) {
  assert_common_args(x, type, attach_x)

  fun <- switch(
    type,
    raw = impl_funs[["raw"]](x),
    smooth = impl_funs[["smooth"]](x, ...)
  )

  res <- add_common_meta(
    fun, x = x, type = type, attach_x = attach_x, extra = extra
  )

  structure(res, class = c(fun_class, "function"))
}

distr_print <- function(fun_name, x, ...) {
  meta_names <- paste0(names(meta(x)), collapse = ", ")
  type_mod <- switch(
    meta(x, "type"),
    raw = "raw",
    smooth = "smoothed"
  )

  cat(collapse_nullable(
    fun_name, " based on ", type_mod, " input\n",
    "Metadata has following elements: ", meta_names
  ))
  cat("\n")
  attributes(x) <- NULL

  print(x, ...)
}

assert_common_args <- function(x, type, attach_x) {
  assert_type(x, is.numeric)

  assert_distr_type(type)

  assert_type(
    attach_x, function(x) {identical(x, TRUE) || identical(x, FALSE)},
    "`TRUE` or `FALSE`"
  )

  x
}

assert_distr_type <- function(type) {
  assert_type(type, is_string)
  if (!(type %in% c("raw", "smooth"))) {
    stop_collapse(
      '`type` should be one of "raw" or "smooth", not ', type, "."
    )
  }

  type
}

add_common_meta <- function(obj, x, type = "smooth", attach_x = TRUE,
                            extra = NULL) {
  res <- add_meta_cond(obj, attach_x, x = x)
  res <- add_meta_cond(res, !is.null(extra), extra = extra)
  res <- add_meta(res, type = type)

  res
}


# Construct discrete distribution table -----------------------------------
# For export (don't forget to mention roundings)
distr_tbl <- function(x) {
  x_meta <- meta(x, "x")
  if (!is.null(x_meta)) {
    smpl <- x_meta
  } else if (is.numeric(x)) {
    smpl <- x
  } else {
    stop_collapse('Input should have metadata "x" or be numeric.')
  }

  x_tbl <- table(round(smpl, digits = 8))
  x_prob <- as.numeric(x_tbl) / length(smpl)

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

trapez_part_integral <- function(x, y) {
  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  c(0, cumsum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1))) / 2)
}
