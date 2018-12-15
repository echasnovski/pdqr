pdqr_print <- function(x, fun_name) {
  cat(line_title(x, fun_name))
  cat(line_support(x))

  cat(line_attached(x, "x", 'Sample ("x")', length))
  cat(appendix_num_x(x))
  cat("\n")

  invisible(x)
}

line_title <- function(x, fun_name) {
  type_print <- paste0(get_meta_type(x))

  paste0(
    bold(fun_name), " function based on ", bold(type_print), " type of input\n"
  )
}

line_support <- function(x) {
  x_support <- meta(x, "support")

  if (is.null(x_support) || !is_support(x_support)) {
    paste0("Support: ", bold("not proper"), "\n")
  } else {
    x_supp_string <- paste0(round(x_support, digits = 5), collapse = ", ")
    support_print <- bold(paste0("[", x_supp_string, "]"))

    paste0("Support: ", support_print, "\n")
  }
}

line_attached <- function(x, meta_name, print_name, length_fun) {
  meta_obj <- meta(x, meta_name)

  if (is.null(meta_obj)) {
    paste0(print_name, " ", bold("is not attached"), " to metadata")
  } else {
    paste0(
      print_name, " ", bold("is attached"), " ", elements(length_fun(meta_obj)),
      " to metadata"
    )
  }
}

appendix_num_x <- function(x) {
  if (has_meta(x, "x") && !is.numeric(meta(x, "x"))) {
    bold(" but isn't numeric")
  } else {
    ""
  }
}

# `force_color` is added for 100% coverage purpose
bold <- function(x, force_color = FALSE) {
  if (force_color || use_color()) {
    paste0("\033[1m", x, "\033[22m")
  } else {
    x
  }
}

# Reasonable effort of dependency free `crayon::has_color()` emulation
use_color <- function() {
  (sink.number() == 0) &&
    grepl(
      "^screen|^xterm|^vt100|color|ansi|cygwin|linux", Sys.getenv("TERM"),
      ignore.case = TRUE, perl = TRUE
    )
}

get_meta_type <- function(x) {
  x_type <- meta(x, "type")

  if (is.null(x_type) || !(x_type %in% c("raw", "smooth"))) {
    "unknown"
  } else {
    x_type
  }
}

elements <- function(len) {
  if (len == 1) {
    "(1 element)"
  } else {
    paste0("(", len, " elements)")
  }
}
