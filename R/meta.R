meta <- function(x, elem = NULL) {
  if (is.null(elem)) {
    attr(x, "meta", exact = TRUE)
  } else {
    attr(x, "meta", exact = TRUE)[[elem]]
  }
}

add_meta <- function(x, ...) {
  dots <- rlang::list2(...)
  cur_meta <- meta(x)

  if (is.null(cur_meta)) {
    attr(x, "meta") <- name_sort(dots)
  } else {
    attr(x, "meta") <- name_sort(c(cur_meta, dots))
  }

  x
}

add_meta_cond <- function(x, cond, ...) {
  if (isTRUE(cond)) {
    add_meta(x, ...)
  } else {
    x
  }
}

name_sort <- function(x) {
  raw_names <- rlang::names2(x)
  raw_names[raw_names == ""] <- NA

  x[order(raw_names, na.last = TRUE)]
}
