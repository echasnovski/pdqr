# Methods for pretty printing ---------------------------------------------
# Methods for pretty printing of pdqr-functions as string. Usually used for
# printing them as elements of list-columns in data frame or tibble (tidyverse
# package).
# Only methods for "pdqr" class are implemented (instead of four more quick
# methods for every class separately) because of possible collision with classes
# "p", "d", "q", "r" from some other package. This implmementation has not very
# big overhead (around 4 microseconds).

#nocov start
# Function `pillar::type_sum()` is used in 'tibble' package. This method is
# registered in `.onLoad()` to avoid explicit import of 'pillar'.
type_sum.pdqr <- function(x) {
  paste0(meta_class(x), "-fun")
}

# This enables printing the `data.frame(l = I(list(new_d(1, "fin"))))`
#' @export
toString.pdqr <- function(x, ...) {
  paste0(meta_class(x), "-function")
}
#nocov end


# Other methods -----------------------------------------------------------
#nocov start
#' @export
summary.pdqr <- function(object, ...) {
  print(object)
}
#nocov end
