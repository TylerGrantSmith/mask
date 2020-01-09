check_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = T) && rstudioapi::isAvailable() &&
}

tidylog_mask_selection <- function() {
  check_rstudio()

  context <- rstudioapi::getActiveDocumentContext()
  selection <- rstudioapi::primary_selection(context)
  text <- trimws(selection$text)

  if(text == "") { return() }

  tryCatch({exprs <- parse(text = text, keep.source = T)},
           error = function(e) stop(parse_error(selection$text), call. = F))

  exprs <- sapply(attr(exprs, "srcref"),
                  function(x) paste0(as.character(x), collapse = "\n"))
  # exprs <- lapply(exprs, rlang::expr_text)
  masked_exprs <- paste0("mask::tidylog_mask(", paste0(exprs), ")")
  
  if(packageVersion("rstudioapi") >= "0.10") {
    rstudioapi::sendToConsole(masked_exprs, echo = F, focus = F)
  } else {
    rstudioapi::sendToConsole(masked_exprs)
  }
}

run_mask_in_console <- function(exprs) {


}
parse_error <- function(lines, pad = 6L) {
  lines <- trimws(lines)
  padding = paste0("|", strrep(" ", pad - 1L))
  lines <- paste0("\n", padding, gsub("\n", paste0("\n", padding), lines))
  paste0("Unable to parse selection\n", lines)
}
