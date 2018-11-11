# General -----------------------------------------------------------------
is_near <- function (x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

is_string <- function(x) {
  is.character(x) && (length(x) == 1)
}

inverse <- function(f, interval, ...) {
  function(t) {
    vapply(
      t,
      function(u) {
        stats::uniroot(function(z) {f(z) - u}, interval, ...)[["root"]]
      },
      numeric(1)
    )
  }
}

stretch_range <- function(x, ext = 10^(-6)) {
  x + ext * c(-1, 1)
}

add_class <- function(x, cl) {
  x_cl <- class(x)
  n_cl <- length(cl)

  if (x_cl[1] != cl[n_cl]) {
    class(x) <- c(cl, x_cl)
  } else {
    class(x) <- c(cl[-n_cl], x_cl)
  }

  x
}

dedupl_list <- function(l) {
  l_names <- names(l)

  if (is.null(l_names)) {
    l
  } else {
    l[!duplicated(l_names) | (l_names == "")]
  }
}


# Notifications -----------------------------------------------------------
stop_collapse <- function(...) {
  stop(collapse_nullable(...), call. = FALSE)
}

warning_collapse <- function(...) {
  warning(collapse_nullable(...), call. = FALSE)
}

message_collapse <- function(...) {
  message(collapse_nullable(...))
}

# This allows to print 'NULL' in for code which evaluates in `NULL`
collapse_nullable <- function(...) {
  dots <- lapply(list(...), capture_null)

  dot_length <- vapply(dots, length, numeric(1))
  if (any(dot_length > 1)) {
    stop("All arguments must be length 1.", call. = FALSE)
  }

  do.call(paste0, dots)
}

capture_null <- function(x) {
  if (is.null(x)) {
    "NULL"
  } else {
    x
  }
}
