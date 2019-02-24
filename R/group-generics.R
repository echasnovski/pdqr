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
    if (is_ops_linear(.Generic, e1, e2)) {
      ops_linear(.Generic, e1, e2)
    } else if (.Generic %in% c(">=", ">", "<=", "<")) {
      ops_compare(.Generic, e1, e2)
    } else {
      form_trans(list(e1, e2), gen_fun, n_sample = n_sample)
    }
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

is_ops_linear <- function(gen, e1, e2) {
  e1_is_number <- is_single_number(e1)
  e2_is_number <- is_single_number(e2)

  ((gen %in% c("+", "-", "*")) && (e1_is_number || e2_is_number)) ||
    ((gen == "/") && (e2_is_number))
}

ops_linear <- function(gen, e1, e2) {
  # It is assumed that this function is run only if `is_ops_linear` gave `TRUE`
  e1_is_number <- is_single_number(e1)

  # `e1` and `e2` should be exactly one single number and one pdqr-function
  if (e1_is_number) {
    assert_pdqr_fun(e2)

    ops_meta <- list(e1_num = e1, e2_num = meta_support(e2), pdqr = e2)
  } else {
    assert_pdqr_fun(e1)

    ops_meta <- list(e1_num = meta_support(e1), e2_num = e2, pdqr = e1)
  }

  if ((gen == "-") && e1_is_number) {
    # This relies on custom implementations of `-` which takes one argument and
    # `+` which takes two arguments and goes into the next clause
    (-e2) + e1
  } else {
    # Output is done by transforming support linearly based on the input
    # operation `gen` and number
    res_supp <- get(gen)(ops_meta[["e1_num"]], ops_meta[["e2_num"]])

    form_resupport(ops_meta[["pdqr"]], res_supp, method = "linear")
  }
}

ops_compare <- function(gen, e1, e2) {
  if (is_single_number(e1)) {
    assert_pdqr_fun(e2)

    e1 <- new_pdqr_by_ref(e2)(e1, "fin")
  } else if (is_single_number(e2)) {
    assert_pdqr_fun(e1)

    e2 <- new_pdqr_by_ref(e1)(e2, "fin")
  } else {
    if (!is_pdqr_fun(e1)) {
      stop_collapse("`e1` should be pdqr-function or single number.")
    }
    if (!is_pdqr_fun(e2)) {
      stop_collapse("`e2` should be pdqr-function or single number.")
    }
  }

  switch(
    gen,
    `>=` = form_geq(e1, e2),
    `>`  = form_greater(e1, e2),
    `<=` = form_leq(e1, e2),
    `<`  = form_less(e1, e2)
  )
}
