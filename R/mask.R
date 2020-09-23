double_colon_intercepter <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))

  if(pkg %in% .__intercepts__) {
    get(name, rlang::caller_env())
  } else {
    getExportedValue(pkg, name)
  }
}

triple_colon_intercepter <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))

  if(pkg %in% .__intercepts__) {
    get(name, rlang::caller_env())
  } else {
    get(name, asNamespace(pkg), inherits = F)
  }
}

create_intercept_env <- function(parent, intercepts) {
  e <- rlang::new_environment(parent = parent)
  rlang::env_bind(e, `.__intercepts__` = intercepts)

  if (length(intercepts)) {
    rlang::env_bind(e, `::` = double_colon_intercepter)
    rlang::fn_env(e$`::`) <- e

    rlang::env_bind(e, `:::` = triple_colon_intercepter)
    rlang::fn_env(e$`:::`) <- e
  }

  return(e)
}

create_masked_env <- function(parent, intercepts) {
  mask_env <- rlang::new_environment(parent = parent)
  attr(mask_env, 'name') <- 'masking_env'

  intercept_env <- create_intercept_env(mask_env, intercepts)
  attr(intercept_env, 'name') <-  'intercept_env'

  enclose_env <- rlang::new_environment(parent = intercept_env)
  attr(enclose_env, 'name') <- 'masked_env'

  return(enclose_env)
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

#' Create a masking function
#'
#' @param envir An environment that will be used to mask others.
#' @param hidden A character vector of package names.
#'
#' @return A masking function.
#' @export
generate_mask <- function(envir, hidden) {
  masking_env <- check_environment(envir)
  masked_env <- create_masked_env(masking_env, hidden)

  f <- function(expr, .binding_env = rlang::caller_env(), .expr_text = deparse(substitute(expr))) {
    on.exit(copy_bindings(masked_env, .binding_env))
    mask_message(.expr_text)
    out <- eval(rlang::call2(withVisible, substitute(expr)), masked_env)

    vis <- out$vis
    val <- out$value

    if (is_function(val)) {
      environment(val) <- masked_env
    }

    if(vis) {
      val
    } else {
      invisible(val)
    }
  }

  structure(f,
            masking_env = masking_env,
            masked_env = masked_env,
            class = c('mask', class(f)))
}

#' @export
print.mask <- function(x) {
  cat(paste("Mask",
            rlang::env_name(attr(x, "masking_env")),
            rlang::env_name(attr(x, "masked_env")),
            sep = "\n"))
}

