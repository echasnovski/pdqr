new_d <- function(x, type = "infin", ...) {
  distr_impl(
    fun_class = "d",
    impl_funs = list(fin = new_d_fin, infin = new_d_infin),
    x = x, type = type, ...
  )
}

new_d_fin <- function(x_tbl) {
  type <- "fin"
  support <- range(x_tbl[["x"]])

  function(x) {
    x_ind <- match(round(x, digits = 8), x_tbl[["x"]], nomatch = NA)

    ifelse(is.na(x_ind), 0, x_tbl[["prob"]][x_ind])
  }
}

new_d_infin <- function(x_tbl) {
  type <- "infin"
  support <- range(x_tbl[["x"]])

  # Using custom function instead of `stats::approxfun()` to avoid creating
  # copies of `x_tbl[["x"]]` and `x_tbl[["y"]]`. It is slower but at acceptable
  # level.
  # For speed a better solution (which doesn't pass R CMD CHECK) would be:
  # function(v) {
  #   stats:::.approxfun(
  #     x = x_tbl[["x"]], y = x_tbl[["y"]], v = v,
  #     method = 1, yleft = 0, yright = 0, f = 0
  #   )
  # }
  function(x) {
    res <- numeric(length(x))
    x_vec <- x_tbl[["x"]]
    y_vec <- x_tbl[["y"]]

    is_inside <- (x >= support[1]) & (x <= support[2])

    # `all.inside = TRUE` is needed to account for case `x` equals `support[2]`
    x_ind <- findInterval(x[is_inside], x_vec, all.inside = TRUE)
    slopes <- (y_vec[x_ind+1] - y_vec[x_ind]) / (x_vec[x_ind+1] - x_vec[x_ind])
    res[is_inside] <- slopes * (x[is_inside] - x_vec[x_ind]) + y_vec[x_ind]

    res
  }
}

print.d <- function(x, ...) {
  x_type <- pdqr_type(x)

  if (!is.null(x_type) && (x_type == "fin")) {
    pdqr_print(x, "Probability mass")
  } else {
    pdqr_print(x, "Density")
  }
}
