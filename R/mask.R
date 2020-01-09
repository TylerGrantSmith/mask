is_double_colon <- function(x, envir = caller_env()) {
  identical(as.character(x),"::")
}

is_triple_colon <- function(x) {
  identical(as.character(x), ":::")
}

intercept_ns_access <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))

  if(pkg %in% c("dplyr", "tidyr")) {
    get(name, parent.frame())
  } else {
    getExportedValue(pkg, name)
  }
}

coerce_function_name <- function(fun) {
  if(rlang::is_symbol(fun)) {
    return(paste0(as.character(fun)))
  }

  if(rlang::is_call(fun) && (is_double_colon(fun[[1]]) || is_triple_colon(fun[[1]]))) {
    return(paste0(as.character(fun[[3]])))
  }

  # If not a function name or package access, assume it is unamed
  # and generate a random name.
  return(paste0(sample(letters, 10)), collapse = "")
}


create_intercept_env <- function(envir) {
  intercept_env <- new.env(parent = envir)
  assign("::", intercept_ns_access, intercept_env)
  environment(intercept_env$`::`) <- intercept_env
  return(intercept_env)
}

create_masked_env <- function(envir) {
  mask_env      <- new.env(parent = envir)
  attr(mask_env, 'name') <-  'mask_env'
  intercept_env <- create_intercept_env(mask_env)
  attr(intercept_env, 'name') <-  'intercept_env'
  enclose_env   <- new.env(parent = intercept_env)
  attr(enclose_env, 'name') <- 'enclose_env'

  return(enclose_env)
}

make_masked_function <- function(x, envir) {
  fun <- rlang::enexpr(x)
  expr <- rlang::quo_get_expr(fun)
  fun_name <- coerce_function_name(expr)

  if(is.null(tryCatch(get(fun_name, envir), error = function(e) NULL))) {
      fun <- rlang::eval_tidy(fun)
      environment(fun) <- envir
      assign(fun_name, fun, envir)
  }

  return(get(fun_name, envir))
}

eval_masked_call <- function(expr, envir) {
  checkmate::assert_true(rlang::is_call(expr))
  # exec_env <- rlang::quo_get_env(expr)
  expr <- rlang::quo_set_env(expr, envir)
  rlang::eval_tidy(expr)
}

is_function_like <- function(x) {
  rlang::is_function(x) ||
    rlang::is_symbol(x) ||
    (rlang::is_call(x) &&
       length(x) == 3 &&
       (is_double_colon(x[[1]]) || is_triple_colon(x[[1]])))
}

eval_mask <- function(x, envir) {
  x <- rlang::enexpr(x)
  expr <- rlang::quo_get_expr(x)

  if (is_function_like(expr)) {
    return(make_masked_function(!!x, envir))
  }

  if (rlang::quo_is_call(x)) {
    return(eval_masked_call(x, envir))
  }

  stop(paste0("Unable to evaluate", as.character(expr)))
}

mask_ <- function(x, envir) {
  mask_envir <- create_masked_env(envir)
  eval_mask({{x}}, mask_envir)
}

#' Evaluate a function or expression in a masking environment
#'
#' @param x A function or an expression
#' @param envir An environment or a name of a package as a `character`, which
#'   should be injected between the execution environment and the current
#'   parent environment.
#'
#' @return In the case that `x` is a function, the result is a a copy of
#'   the function in a new enclosing environment.  Otherwise, the result of
#'   `x` executing in the new environment.
#'
#' @export
#'
#' @examples
#' mask(mtcars %>% dplyr::select(mpg, cyl), "tidylog")
mask <- function(x, envir = rlang::caller_env()) {
  if(is.character(envir)) {
    envir <- getNamespace(envir)
  }

  checkmate::assert_environment(envir)
  mask_({{x}}, envir)
}

#' Mask with tidylog
#'
#' @description Make use of tidylog without masking in the global environment.
#'
#' @param x A function or expression.
#'
#' @return If x evaluates to a function, then a new function is returned
#'   which is masked by the tidylog namespace.  Otherwise, x is evaluated
#'   in a child environment of the tidylog namespace.
#'
#' @export
tidylog_mask <- function(x) {
  mask_({{x}}, asNamespace("tidylog"))
}
