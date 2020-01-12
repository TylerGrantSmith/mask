create_ns_intercepter <- function(exported = T) {
  if(exported) {
    intercept_ns_access <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))

      if(pkg %in% .__intercepts__) {
        get(name, caller_env())
      } else {
        getExportedValue(pkg, name)
      }
    }
  } else {
    intercept_ns_access <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))

      if(pkg %in% .__intercepts__) {
        get(name, caller_env())
      } else {
        get(name, asNamespace(pkg), inherits = F)
      }
    }
  }

  return(intercept_ns_access)
}

create_intercept_env <- function(parent, intercepts) {
  e <- new_environment(parent = parent)
  env_bind(e, `.__intercepts__` = intercepts)

  env_bind(e, `::` = create_ns_intercepter(T))
  fn_env(e$`::`) <- e

  env_bind(e, `:::` = create_ns_intercepter(F))
  fn_env(e$`:::`) <- e

  return(e)
}

create_masked_env <- function(parent, intercepts) {
  mask_env <- new_environment(parent = parent)
  attr(mask_env, 'name') <- 'mask_env'

  intercept_env <- create_intercept_env(mask_env, intercepts)
  attr(intercept_env, 'name') <-  'intercept_env'

  enclose_env <- new_environment(parent = intercept_env)
  attr(enclose_env, 'name') <- 'enclose_env'

  return(enclose_env)
}

#' Create a masking function
#'
#' @param envir An environment that will be used to mask others.
#' @param hidden A character vector of package names.
#'
#' @return A masking function.
#' @export
generate_mask <- function(envir, hidden) {
  masked_env <- create_masked_env(envir, hidden)

  function(x, binding_env = caller_env()) {
    on.exit(copy_bindings(masked_env, binding_env))
    out <- eval(call2(withVisible, substitute(x)), masked_env)
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
}

copy_bindings <- function(from, to) {
  for (nm in env_names(from)) {
    env_bind(to, !!nm := env_get(from, nm))
  }
}

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
tidylog_mask <- function(hidden = c("dplyr", "tidyr")) {
  generate_mask(getNamespace("tidylog"), hidden)
}

test <- function() {
  x <- select(mtcars, mpg, cyl) %>% dplyr::filter(mpg < 14)

  x %>% group_by(cyl) %>% summarise(mpg = mean(mpg))
}
