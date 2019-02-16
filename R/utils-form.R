# 'pdqr' classes functionality --------------------------------------------
new_pdqr_by_class <- function(pdqr_class) {
  switch(
    pdqr_class,
    p = new_p, d = new_d, q = new_q, r = new_r,
    stop_collapse("Wrong 'pdqr' class.")
  )
}

new_pdqr_by_ref <- function(f) {
  new_pdqr_by_class(get_pdqr_class(f))
}

as_pdqr_by_class <- function(pdqr_class) {
  switch(
    pdqr_class,
    p = as_p, d = as_d, q = as_q, r = as_r,
    stop_collapse("Wrong 'pdqr' class.")
  )
}

as_pdqr_by_ref <- function(f) {
  as_pdqr_by_class(get_pdqr_class(f))
}

get_pdqr_class <- function(f) {
  pdqr_classes <- c("p", "d", "q", "r")
  f_type <- pdqr_classes[match(class(f), pdqr_classes)]

  f_type[!is.na(f_type)][1]
}


# Point dirac -------------------------------------------------------------
point_dirac <- function(at_x, type, pdqr_class, h = 1e-8) {
  x_tbl <- switch(
    type,
    fin = data.frame(x = at_x, prob = 1),
    infin = data.frame(x = at_x + h*c(-1, 0, 1), y = c(0, 1, 0)/h),
    stop_collapse("Incorrect `type` for `point_dirac()`.")
  )

  pdqr_f <- new_pdqr_by_class(pdqr_class)

  pdqr_f(x_tbl, type)
}
