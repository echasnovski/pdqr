meta <- function(obj, elem = NULL) {
  if (is.null(elem)) {
    attr(obj, "meta", exact = TRUE)
  } else {
    attr(obj, "meta", exact = TRUE)[[elem]]
  }
}

add_meta <- function(obj, ...) {
  dots <- list(...)
  cur_meta <- meta(obj)

  if (is.null(cur_meta)) {
    attr(obj, "meta") <- name_sort(dots)
  } else {
    attr(obj, "meta") <- dedupl_list(name_sort(c(dots, cur_meta)))
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

remove_meta <- function(obj) {
  attr(obj, "meta") <- NULL

  obj
}

copy_meta <- function(to, from) {
  attr(to, "meta") <- attr(from, "meta")

  to
}

has_meta <- function(obj, elem = NULL) {
  !is.null(meta(obj, elem))
}

name_sort <- function(obj) {
  raw_names <- names(obj)
  if (is.null(raw_names)) {
    raw_names <- rep(NA, length(obj))
  }
  raw_names[raw_names == ""] <- NA

  obj[order(raw_names, na.last = TRUE)]
}

pdqr_type <- function(obj) {
  meta(obj, "type")
}

pdqr_support <- function(obj) {
  meta(obj, "support")
}
