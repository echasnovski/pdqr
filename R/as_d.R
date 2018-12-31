as_d <- function(f, ...) {
  assert_pdqr_fun(f)

  new_d(x = meta(f, "x_tbl"), type = meta(f, "type"))
}
