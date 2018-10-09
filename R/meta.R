meta <- function(obj, elem = NULL) {
  if (is.null(elem)) {
    attr(obj, "meta", exact = TRUE)
  } else {
    attr(obj, "meta", exact = TRUE)[[elem]]
  }
}

add_meta <- function(obj, ...) {
  dots <- rlang::list2(...)
  cur_meta <- meta(obj)

  if (is.null(cur_meta)) {
    attr(obj, "meta") <- name_sort(dots)
  } else {
    attr(obj, "meta") <- name_sort(c(cur_meta, dots))
  }

  obj
}

add_meta_cond <- function(obj, cond, ...) {
  if (isTRUE(cond)) {
    add_meta(obj, ...)
  } else {
    obj
  }
}

name_sort <- function(obj) {
  raw_names <- rlang::names2(obj)
  raw_names[raw_names == ""] <- NA

  obj[order(raw_names, na.last = TRUE)]
}
