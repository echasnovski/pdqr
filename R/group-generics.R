Math.pdqr <- function(x, ...) {
  n_sample <- getOption("pdqr.transform.n_sample")

  gen_fun <- function(y) {
    get(.Generic)(y, ...)
  }

  form_trans(list(x), gen_fun, n_sample = n_sample)
}

Ops.pdqr <- function(e1, e2) {
  n_sample <- getOption("pdqr.transform.n_sample")

  gen_fun <- get(.Generic)

  if (missing(e2)) {
    form_trans(list(e1), gen_fun, n_sample = n_sample)
  } else {
    form_trans(list(e1, e2), gen_fun, n_sample = n_sample)
  }
}

Summary.pdqr <- function(..., na.rm = FALSE) {
  n_sample <- getOption("pdqr.transform.n_sample")

  if (.Generic == "range") {
    stop_collapse(
      "`range()` can't be applied to ", '"pdqr" functions as it returns two ',
      "numbers. Use `min()` and `max()`."
    )
  }

  gen_fun <- function(...) {
    g <- get(.Generic)
    f <- function(...) {g(..., na.rm = na.rm)}

    # `Map()` is needed to "vectorize" `Summary` generics
    unlist(Map(f, ...))
  }

  form_trans(list(...), gen_fun, n_sample = n_sample)
}
