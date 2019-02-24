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
    switch(
      .Generic,
      `+` = e1,
      `-` = reflect_pdqr_around_zero(e1),
      `!` = negate_pdqr(e1)
    )
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

reflect_pdqr_around_zero <- function(f) {
  refl_x_tbl <- reflect_x_tbl(meta_x_tbl(f), 0)

  new_pdqr_by_ref(f)(refl_x_tbl, meta_type(f))
}

negate_pdqr <- function(f) {
  # Probability of type "infin" pdqr-function being exactly 0 is equal to zero
  prob_zero <- if (meta_type(f) == "fin") {as_d(f)(0)} else {0}

  new_pdqr_by_ref(f)(
    data.frame(x = 0:1, prob = c(1-prob_zero, prob_zero)), "fin"
  )
}
