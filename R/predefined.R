#' Create a mask for tidylog evaluation
#'
#' @param hidden A character vector of package names that should be in
#'
#' @return A masking function.
#' @export
#'
#' @examples
#' m <- tidylog_mask()
#' m( select(mtcars, mpg))
tidylog_mask <- function() {
  generate_mask("tidylog", c("dplyr", "tidyr"))
}

#' @export
purrrgress_mask <- function() {
  generate_mask("purrrgress", c("purrr"))
}
