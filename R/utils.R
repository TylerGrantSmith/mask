check_environment <- function(envir) {
  if (is.environment(envir)) {
    return(envir)
  } else {
    return(asNamespace(envir))
  }
}

copy_bindings <- function(from, to) {
  for (nm in env_names(from)) {
    env_bind(to, !!nm := env_get(from, nm))
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

capture_utf8_output <-
  function (..., file = NULL, append = FALSE, type = c("output",
                                                     "message"), split = FALSE)
{
  args <- substitute(list(...))[-1L]
  type <- match.arg(type)
  rval <- NULL
  closeit <- TRUE
  if (is.null(file))
    file <- textConnection("rval", "w", local = TRUE, encoding = "UTF-8")
  else if (is.character(file))
    file <- file(file, if (append)
      "a"
      else "w")
  else if (inherits(file, "connection")) {
    if (!isOpen(file))
      open(file, if (append)
        "a"
        else "w")
    else closeit <- FALSE
  }
  else stop("'file' must be NULL, a character string or a connection")
  sink(file, type = type, split = split)
  on.exit({
    sink(type = type, split = split)
    if (closeit) close(file)
  })
  pf <- parent.frame()
  evalVis <- function(expr) withVisible(eval(expr, pf))
  for (i in seq_along(args)) {
    expr <- args[[i]]
    tmp <- switch(mode(expr), expression = lapply(expr,
                                                  evalVis), call = , name = list(evalVis(expr)), stop("bad argument"))
    for (item in tmp) if (item$visible)
      print(item$value)
  }
  on.exit()
  sink(type = type, split = split)
  if (closeit)
    close(file)
  if (is.null(rval))
    invisible(NULL)
  else rval
}
