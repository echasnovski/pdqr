# Package description -----------------------------------------------------
#' pdqr: Work with Custom Distribution Functions
#'
#' @details Excerpt of important documentation:
#' - README and vignettes provide overview of package functionality.
#' - Documentation of [meta_*()][meta_all()] functions describes implementation
#' details of pdqr-functions.
#'     - Documentation of [print()][methods-print] and [plot()][methods-plot]
#' methods describes how you can interactively explore properties of
#' pdqr-functions.
#' - Documentation of [new_*()][new_p()] functions describes the process of
#' creating pdqr-functions.
#' - Documentation of [as_*()][as_p()] functions describes the process of
#' updating class of pdqr-functions.
#' - Documentation of `form_*()` functions describes how different
#' transformation functions work. Important pages are for [form_trans()] and
#' [Pdqr methods for S3 group generic functions][methods-group-generic].
#' - Documentation of `summ_*()` functions describes how different summary
#' functions work. A good place to start is [summ_center()].
#' - Documentation of [`region_*()`][region] functions describes functionality
#' to work with regions: data frames defining subset of one dimensional real
#' line.
#'
#' This package has the following options (should be set by
#' [options()][base::options()]):
#' - *"pdqr.approx_discrete_n_grid"*. This single number (default to 1000)
#' determines degree of granularity of how continuous pdqr-function is
#' approximated with discrete one during some complicated tasks. Approximation
#' is done by first using [form_regrid()] with `n_grid` argument equal to this
#' option and `method = "x"`, and then [form_retype()] is used with `type =
#' "discrete"` and `method = "piecelin"`. Value of this option should be big
#' enough for high accuracy and small enough for high computation
#' speed, for which value 1000 showed to be fairly appropriate.
#' - *"pdqr.assert_args"*. This boolean option (default to `TRUE`) may be used
#' to turn off sanity checks of function arguments (set it to `FALSE`), which
#' will somewhat increase general execution speed. **Use this option at your own
#' risk in case you are confident that input arguments have correct type and
#' structure**.
#' - *"pdqr.group_gen.args_new"*, *"pdqr.group_gen.n_sample"*,
#' *"pdqr.group_gen.repair_supp_method"*. They may be used to customize behavior
#' of methods for S3 group generic functions. See [their help
#' page][methods-group-generic] for more information.
"_PACKAGE"


# Methods for pretty printing ---------------------------------------------
# Methods for pretty printing of pdqr-functions as string. Usually used for
# printing them as elements of list-columns in data frame or tibble (tidyverse
# package).
# Only methods for "pdqr" class are implemented (instead of four more quick
# methods for every class separately) because of possible collision with classes
# "p", "d", "q", "r" from some other package. This implmementation has not very
# big overhead (around 4 microseconds).

# nocov start
# Function `pillar::type_sum()` is used in 'tibble' package. This method is
# registered in `.onLoad()` to avoid explicit import of 'pillar'.
type_sum.pdqr <- function(x) {
  paste0(meta_class(x), "-fun")
}

# This enables printing the `data.frame(l = I(list(new_d(1, "discrete"))))`
#' @export
toString.pdqr <- function(x, ...) {
  paste0(meta_class(x), "-function")
}
# nocov end


# Other methods -----------------------------------------------------------
# nocov start
#' @export
summary.pdqr <- function(object, ...) {
  print(object)
}
# nocov end
