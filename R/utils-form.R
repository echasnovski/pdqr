new_pdqr_by_ref <- function(f) {
  pdqr_class <- get_pdqr_class(f)

  switch(
    pdqr_class,
    p = new_p, d = new_d, q = new_q, r = new_r,
    stop_collapse("`f` does not have 'pdqr' class.")
  )
}

as_pdqr_by_ref <- function(f) {
  pdqr_class <- get_pdqr_class(f)

  switch(
    pdqr_class,
    p = as_p, d = as_d, q = as_q, r = as_r,
    stop_collapse("`f` does not have 'pdqr' class.")
  )
}

get_pdqr_class <- function(f) {
  pdqr_classes <- c("p", "d", "q", "r")
  f_type <- pdqr_classes[match(class(f), pdqr_classes)]

  f_type[!is.na(f_type)][1]
}
