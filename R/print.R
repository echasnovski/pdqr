pdqr_print <- function(x, fun_name) {
  cat(line_title(x, fun_name))
  cat(line_support(x))

  invisible(x)
}

line_title <- function(x, fun_name) {
  type_print <- paste0(meta_type_print_name(x))

  paste0(
    bold(fun_name), " function with ", bold(type_print), " number of values\n"
  )
}

line_support <- function(x) {
  x_support <- meta_support(x)

  if (is.null(x_support) || !is_support(x_support)) {
    paste0("Support: ", bold("not proper"), "\n")
  } else {
    x_supp_string <- paste0(round(x_support, digits = 5), collapse = ", ")
    support_print <- bold(paste0("[", x_supp_string, "]"))

    paste0("Support: ", support_print, n_x_tbl_info(x), "\n")
  }
}

n_x_tbl_info <- function(x) {
  x_type <- meta_type(x)
  x_tbl <- meta_x_tbl(x)

  if (is.null(x_type) || !is.character(x_type) ||
      is.null(x_tbl) || !is.data.frame(x_tbl)) {
    return("")
  }

  n_x_tbl <- nrow(x_tbl)

  if (x_type == "fin") {
    paste0(" (", n_x_tbl, " ", ngettext(n_x_tbl, "element", "elements"), ")")
  } else if (x_type == "infin") {
    paste0(
      " (", n_x_tbl-1, " ", ngettext(n_x_tbl-1, "interval", "intervals"), ")"
    )
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

meta_type_print_name <- function(x) {
  x_type <- meta_type(x)

  if (is.null(x_type) || !(x_type %in% c("fin", "infin"))) {
    "unknown"
  } else {
    switch(x_type, fin = "finite", infin = "infinite")
  }
}
