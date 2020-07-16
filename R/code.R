#' @export
ref <- function(object) {
  ref_env <- new.env(parent = parent.frame(n = 2))
  class(ref_env) <- "ref_env"
  name <- deparse(substitute(object))
  ref_env$name <- name
  ref_env
}

replace_lang <- function(expr, item, replacement) {
  if (length(expr) == 1) {
    if (expr == item) replacement else expr
  } else as.call(sapply(expr, function(elem) replace_lang(elem, item, replacement)))
}

#' @export
mod <- function(object, fun) {
  UseMethod("mod")
}

#' @export
mod.ref_env <- function(object, fun) {
  if (missing(fun)) {
    parent.env(object)[[object$name]]
  } else {
    expr <- str2lang(deparse(substitute(fun)))
    argname <- str2lang(deparse(substitute(object)))
    expr <- replace_lang(expr, argname, str2lang(object$name))
    eval(expr, parent.env(object))
  }
}

#' @export
mod.default <- function(object, fun) {
  if (missing(fun)) {
    object
  } else {
    env <- list(object)
    names(env) <- deparse(substitute(object))
    eval(fun, env)
  }
}
