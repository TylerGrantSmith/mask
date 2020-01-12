check_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = T) && rstudioapi::isAvailable()
}

tidylog_mask_selection <- function(binding_env = global_env()) {
  check_rstudio()

  context <- rstudioapi::getActiveDocumentContext()
  selection <- rstudioapi::primary_selection(context)
  text <- trimws(selection$text)

  if(text == "") { return() }

  tryCatch({exprs <- parse(text = text, keep.source = T)},
           error = function(e) stop(parse_error(selection$text), call. = F))
  m <- tidylog_mask()
  for (i in 1:length(exprs)) {
    cat("\n")
    expr <- exprs[[i]]
    srcref <- attr(exprs, "srcref")[[i]]
    rstudioapi::sendToConsole("", execute = F, focus = F, echo = F)
    mask_message(srcref)
    output <- capture.output(eval(call2(m, expr, binding_env), caller_env()))
    if(length(output)) {
      cat(output, sep = "\n")
    }
  }
}

mask_message <- function(code) {
  text <- as.character(code)
  text <- unlist(strsplit(text, "\n"))
  width <- getOption("width")

  prefix <- "Masking | "
  prefix_length <- 10L

  wrap_width <- min(20L, width - prefix_length)
  code <- crayon::yellow(code)
  if(requireNamespace("crayon")) {
    prefix <- crayon::cyan(prefix)
  }

  for (i in 1:length(text)) {
    cat(prefix, text[i], "\n", sep = "", append = T)
  }
}

wrap_and_indent <- function(code, width) {
  whitespace <- "^[ \t\r\n]+"
  indent <- regmatches(code, regexpr(whitespace, code))

  if(length(indent)) {
    strsplit_size(code, width - indent)
  }

}

strsplit_size <- function(string, size){
  unlist(strsplit(string, paste0('(?<=.{',size,'})'), perl=TRUE))
}

parse_error <- function(lines, pad = 6L) {
  lines <- trimws(lines)
  padding = paste0("|", strrep(" ", pad - 1L))
  lines <- paste0("\n", padding, gsub("\n", paste0("\n", padding), lines))
  paste0("Unable to parse selection\n", lines)
}
