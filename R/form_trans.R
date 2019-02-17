# Main transformation function --------------------------------------------
form_trans <- function(f_list, trans, ..., n_sample = 10000) {
  # The resaon `n_sample` is after the `...` is because if it was before `...`
  # it would mask the `n` argument to `density()` due to R's partial matching.
  assert_type(f_list, is.list)
  assert_f_list(f_list, allow_numbers = TRUE)
  assert_type(trans, is.function)
  assert_type(n_sample, is_single_number, type_name = "single number")

  # Convert to random generation functions. If `f_list` element isn't
  # pdqr-function then it is a number.
  r_dots <- lapply(f_list, function(f) {
    if (is_pdqr_fun(f)) {
      as_r(f)
    } else {
      function(n) {rep(f, length.out = n)}
    }
  })

  # Generate samples for every such function
  smpl_list <- lapply(r_dots, do.call, args = list(n_sample))

  # Call `trans` with all generated samples to produce sample from transformed
  # distribution
  smpl <- do.call(trans, smpl_list)

  # Produce output pdqr function
  f_list_meta <- compute_f_list_meta(f_list)
  new_pdqr <- new_pdqr_by_class(f_list_meta[["class"]])

  new_pdqr(x = smpl, type = f_list_meta[["type"]], ...)
}


# Group generics ----------------------------------------------------------
Math.pdqr <- function(x, ...) {
  n_sample <- getOption("pdqr.transform.n_sample")

  gen_fun <- function(y) {
    get(.Generic)(y, ...)
  }

  form_trans(list(x), gen_fun, n_sample = n_sample)
}

Ops.pdqr <- function(e1, e2) {
  n_sample <- getOption("pdqr.transform.n_sample")

  gen_fun <- function(...) {
    res <- get(.Generic)(...)
    if (!is.numeric(res)) {
      warning_collapse(
        "Output of `", .Generic, "` is '", get_type(res), "'. ",
        "Converting to 'numeric'."
      )
      res <- as.numeric(res)
    }

    res
  }

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
