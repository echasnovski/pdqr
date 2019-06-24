# Documentation -----------------------------------------------------------
#' Pdqr methods for print function
#'
#' Pdqr-functions have their own methods for [print()] which displays function's
#' [metadata][meta_all()] in readable and concise form.
#'
#' @param x Pdqr-function to print.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details Print output of pdqr-function describes the following information:
#' - Full name of function [class][meta_class()]:
#'     - P-function is "Cumulative distribution function".
#'     - D-function is "Probability mass function" for "discrete" type and
#'     "Probability density function" for "continuous".
#'     - Q-function is "Quantile function".
#'     - R-function is "Random generation function".
#' - [Type][meta_type()] of function in the form "of * type" where "\*" is
#' "discrete" or "continuous" depending on actual type.
#' - [Support][meta_support()] of function.
#' - Number of elements in distribution for "discrete" type or number of
#' intervals of piecewise-linear density for "continuous" type.
#' - If pdqr-function has "discrete" type and exactly two possible values 0 and
#' 1, it is treated as "boolean" pdqr-function and probability of 1 is shown.
#' This is done to simplify interactive work with output of comparing functions
#' like `>=`, etc. (see [description of methods for S3 group generic
#' functions][methods-group-generic]). To extract probabilities from "boolean"
#' pdqr-function, use [summ_prob_true()] and [summ_prob_false()].
#'
#' Symbol "~" in `print()` output indicates that printed value or support is an
#' approximation to a true one (for readability purpose).
#'
#' @family pdqr methods for generic functions
#'
#' @examples
#' print(new_d(1:10, "discrete"))
#'
#' r_unif <- as_r(runif, n_grid = 251)
#' print(r_unif)
#'
#' # Printing of boolean pdqr-function
#' print(r_unif >= 0.3)
#'
#' @name methods-print
NULL


# Functions ---------------------------------------------------------------
pdqr_print <- function(x, fun_name) {
  cat(line_title(x, fun_name))
  cat(line_support(x))

  invisible(x)
}

line_title <- function(x, fun_name) {
  type_print <- paste0(meta_type_print_name(x))

  paste0(
    bold(fun_name), " function of ", bold(type_print), " type\n"
  )
}

line_support <- function(x) {
  x_support <- meta_support(x)

  if (is.null(x_support) || !is_support(x_support)) {
    paste0("Support: ", bold("not proper"), "\n")
  } else {
    x_supp_round <- round(x_support, digits = 5)
    approx_sign <- get_approx_sign(x_support, x_supp_round)

    x_supp_string <- paste0(x_supp_round, collapse = ", ")
    support_print <- bold(paste0(approx_sign, "[", x_supp_string, "]"))

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

  if (x_type == "discrete") {
    # Add "probability of 1: " printing in case of a "boolean" pdqr
    if (identical(x_tbl[["x"]], c(0, 1))) {
      prob_one <- x_tbl[["prob"]][x_tbl[["x"]] == 1]
      prob_one_round <- round(prob_one, digits = 5)
      approx_sign <- get_approx_sign(prob_one, prob_one_round)
      prob_one_string <- paste0(approx_sign, prob_one_round)

      if (prob_one_round %in% c(0, 1)) {
        prob_one_string <- paste0(approx_sign, prob_one_round, ".0")
      }

      prob_string <- paste0(
        ", ", bold(paste0("probability of 1: ", prob_one_string))
      )
    } else {
      prob_string <- ""
    }
    paste0(
      " (", n_x_tbl, " ", ngettext(n_x_tbl, "element", "elements"),
      prob_string, ")"
    )
  } else if (x_type == "continuous") {
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

  if (is.null(x_type) || !(x_type %in% c("discrete", "continuous"))) {
    "unknown"
  } else {
    x_type
  }
}

get_approx_sign <- function(x, x_round) {
  x_is_rounded <- any(!is_near(x, x_round, tol = 1e-13))

  if (x_is_rounded) {
    "~"
  } else {
    ""
  }
}
