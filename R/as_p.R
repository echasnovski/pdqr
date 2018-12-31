as_p <- function(f, ...) {
  assert_pdqr_fun(f)

  new_p(x = meta(f, "x_tbl"), type = meta(f, "type"))
}
