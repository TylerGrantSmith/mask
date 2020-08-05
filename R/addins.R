check_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = T) && rstudioapi::isAvailable()
}

generic_mask_selection <- function(mask) {
  function(binding_env = global_env()) {
    check_rstudio()
    context <- rstudioapi::getActiveDocumentContext()
    selection <- rstudioapi::primary_selection(context)
    text <- trimws(selection$text)

    if(text == "") { return() }
    exprs <- evaluate::parse_all(text, NULL, TRUE)
    err <- attr(exprs, "PARSE_ERROR")
    if (!is.null(err)) {
      warn(paste0("Error: Unable to parse selection.\n", err$message,"\n"))
      return()
    }
    m <- mask()
    # tryCatch({exprs <- parse(text = text, keep.source = T)})
    for (i in 1:nrow(exprs)) {
      cat("\n")
      expr_text <- exprs$src[[i]]
      expr <- exprs$expr[[i]][[1]]
      expr <- call2(m, expr,
                    .binding_env = binding_env,
                    .expr_text = expr_text)
      rstudioapi::sendToConsole("", execute = F, focus = F, echo = F)
      withAutoprint(expr, evaluated = T, echo = F)
      # expr <- exprs$expr[[i]]
      # expr_src <- exprs$src[[i]]

      # withAutoprint(call2(m,
      #            expr = expr,
      #            .binding_env = binding_env,
      #            .expr_text = expr_src),
      #            evaluated = T,
      #            local = caller_env(),
      #            echo = F)
    }
  }
}

tidylog_mask_selection <- generic_mask_selection(tidylog_mask)
purrrgress_mask_selection <- generic_mask_selection(purrrgress_mask)
