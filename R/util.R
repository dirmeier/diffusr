#' @noRd
.equals.double <- function(val, cmp, delta)
{
  val  + delta <= cmp  |  val  - delta >= cmp
}
