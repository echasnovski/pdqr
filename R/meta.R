#' Get metadata of pdqr-function
#'
#' Tools for getting metadata of **pdqr-function**: a function which represents
#' distribution with finite support and finite values of probability/density.
#' The key metadata which defines underline distribution is **"x_tbl"**. If two
#' pdqr-functions have the same "x_tbl" metadata, they represent the same
#' distribution and can be converted to one another with `as_*()` family of
#' functions.
#'
#' @param f A pdqr-function.
#'
#' @details Internally storage of metadata is implemented as follows:
#' - Pdqr class is a first "appropriate" ("p", "d", "q", or "r") S3 class of
#' pdqr-function. All "proper" pdqr-functions have full S3 class of the form:
#' `c(cl, "pdqr", "function")`, where `cl` is pdqr class.
#' - Pdqr type, support, and "x_tbl" are stored into function's
#' [environment][environment()].
#'
#' @section Pdqr class:
#'
#' Pdqr class is returned by `meta_class()`. This can be one of "p", "d", "q",
#' "r". Represents **how pdqr-function describes underlying distribution**:
#' - P-function (i.e. of class "p") returns value of cumulative distribution
#' function (probability of random variable being not more than certain value).
#' Internally it is implemented as direct integration of corresponding (with the
#' same "x_tbl" metadata) d-function.
#' - D-function returns value of probability mass or density function (depending
#' on pdqr type). Internally it is implemented by directly using "x_tbl"
#' metadata (see section '"x_tbl" metadata' for more details).
#' - Q-function returns value of quantile function. Internally it is implemented
#' as inverse of corresponding p-function (returns the smallest "x" value which
#' has cumulative probability not less than input).
#' - R-function generates random sample from distribution. Internally it is
#' implemented using inverse transform sampling: certain amount of points from
#' [standard uniform distribution][stats::runif()] is generated, and the output
#' is values of corresponding q-function at generated points.
#'
#' @section Pdqr type:
#'
#' Pdqr type is returned by `meta_type()`. This can be one of "fin" (short for
#' "finite") or "infin" (short for "infinite"). Represents **type of underlying
#' distribution**:
#' - Type "fin" is used for distributions with finite number of outcomes.
#' Functions with "fin" type has a fixed set of "x" values ("x" column in
#' "x_tbl" metadata) on which d-function returns possibly non-zero output
#' (values from "prob" column in "x_tbl" metadata). It is not called "discrete",
#' because discrete distributions can (in theory) have infinite number of
#' possible outcomes.
#' - Type "infin" is used for distribution with potentially infinite number of
#' outcomes (not accounting for "finite nature" of computer simulation).
#' Functions with "infin" type have piecewise-linear density which goes through
#' points defined by "x" and "y" columns in "x_tbl" metadata. It is not called
#' "continuous" (although it has continuous, on its support, density) because
#' "infin" pdqr-functions can be used to approximate mixture of discrete and
#' continuous distributions.
#'
#' @section Pdqr support:
#' Pdqr support is returned by `meta_support()`. This is a numeric vector with
#' two finite values. Represents **support of underlying distribution**: closed
#' interval, outside of which d-function is equal to zero. **Note** that inside
#' of support d-function can also be zero, which especially true for "fin"
#' functions.
#'
#' Technically, pdqr support is range of values from "x" column of "x_tbl"
#' metadata.
#'
#' @section "x_tbl" metadata:
#'
#' Metadata "x_tbl" is returned by `meta_x_tbl()`. This is a key metadata which
#' **completely defines distribution**. It is a data frame with three numeric
#' columns, content of which partially depends on pdqr type.
#'
#' Type "fin" functions have "x_tbl" with columns "x", "prob", "cumprob".
#' D-functions return a value from "prob" column for input which is very near
#' (should be equal up to ten digits, defined by [round(*, digits =
#' 10)][round()]) to corresponding value of "x" column. Rounding is done to
#' account for issues with representation of numerical values (see Note section
#' of [`==`]'s help page). For any other input, d-functions return
#' zero.
#'
#' Type "infin" functions have "x_tbl" with columns "x", "y", "cumprob".
#' D-functions return a value of piecewise-linear function passing through
#' points that have "x" and "y" coordinates. For any value outside support (i.e.
#' strictly less than minimum "x" and strictly more than maximum "x") output is
#' zero.
#'
#' Column "cumprob" always represents the probability of underlying random
#' variable being not more than corresponding value in "x" column.
#'
#' @section Change of metadata:
#'
#' All metadata of pdqr-functions are not meant to be changed directly. Also
#' change of pdqr type, support, and "x_tbl" metadata will lead to a complete
#' change of underlying distribution.
#'
#' To change **pdqr class**, for example to convert p-function to d-function,
#' use `as_*()` family of functions: [as_p()], [as_d()], [as_q()], [as_r()].
#'
#' To change **pdqr type**, use [form_retype()]. It changes underlying
#' distribution in the most suitable for user way.
#'
#' To change **pdqr support**, use [form_resupport()] or [form_tails()].
#'
#' Change of **"x_tbl" metadata** is not possible, because basically it means
#' creating completely new pdqr-function. To do that, supply data frame with
#' "x_tbl" format suitable for desired "type" to appropriate `new_*()` function:
#' [new_p()], [new_d()], [new_q()], [new_r()]. Also, there is a [form_regrid()]
#' function which will increase or decrease granularity of pdqr-function.
#'
#' @return `meta_all()` returns a list of all metadata. `meta_class()`,
#'   `meta_type()`, `meta_support`, and `meta_x_tbl()` return corresponding
#'   metadata.
#'
#' @examples
#' d_unif <- as_d(dunif)
#'
#' meta_all(d_unif)
#' meta_class(d_unif)
#' meta_type(d_unif)
#' meta_support(d_unif)
#' head(meta_x_tbl(d_unif))
#'
#' @name meta
NULL

#' @rdname meta
#' @export
meta_all <- function(f) {
  check_f_envir(f)

  env_meta_names <- c("type", "support", "x_tbl")

  res <- vector(mode = "list", length = 4)
  names(res) <- c("class", env_meta_names)

  res[["class"]] <- meta_class(f)

  # Usage of `get0()` ensures that `NULL` is returned if (for some reason) an
  # object isn't found in environement
  res[env_meta_names] <- lapply(
    env_meta_names, get0, envir = environment(f), inherits = FALSE
  )

  res
}

#' @rdname meta
#' @export
meta_class <- function(f) {
  pdqr_classes <- c("p", "d", "q", "r")
  f_class <- pdqr_classes[match(class(f), pdqr_classes)]

  f_class[!is.na(f_class)][1]
}

#' @rdname meta
#' @export
meta_type <- function(f) {
  check_f_envir(f)

  get0("type", envir = environment(f), inherits = FALSE)
}

#' @rdname meta
#' @export
meta_support <- function(f) {
  check_f_envir(f)

  get0("support", envir = environment(f), inherits = FALSE)
}

#' @rdname meta
#' @export
meta_x_tbl <- function(f) {
  check_f_envir(f)

  get0("x_tbl", envir = environment(f), inherits = FALSE)
}

has_meta <- function(f, elem) {
  !is.null(meta_all(f)[[elem]])
}

check_f_envir <- function(f) {
  f_env <- environment(f)

  if (is.null(f_env)) {
    stop_collapse("`f` should have enclosing environment.")
  }
  if (identical(f_env, globalenv())) {
    stop_collapse("`f` should not be created in Global environment.")
  }

  TRUE
}
