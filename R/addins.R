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
    exprs <- evaluate::evaluate()

    tryCatch({exprs <- parse(text = text, keep.source = T)})
    m <- mask()
    for (i in 1:length(exprs)) {
      cat("\n")
      expr <- exprs[[i]]
      expr_src <- attr(exprs, "srcref")[[i]]
      rstudioapi::sendToConsole("", execute = F, focus = F, echo = F)
      withAutoprint(call2(m,
                 expr = expr,
                 .binding_env = binding_env,
                 .expr_text = expr_src),
                 evaluated = T,
                 local = caller_env(),
                 echo = F)
    }
  }
}

tidylog_mask_selection <- generic_mask_selection(tidylog_mask)
purrrgress_mask_selection <- generic_mask_selection(purrrgress_mask)
