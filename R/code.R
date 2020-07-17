#' @importFrom rlang env caller_env enexpr is_symbol env_has as_string abort
#' @export
ref <- function(object) {
  rfr <- env(caller_env())
  class(rfr) <- "reference"
  symbol <- enexpr(object)
  if (!is_symbol(symbol) ||
      !env_has(env = caller_env(), nms = as_string(symbol)))
    abort("Object passed to the ref has to be a name present in calling environment!")
  rfr$symbol <- symbol
  rfr$name <- as_string(symbol)
  rfr
}

#' @importFrom rlang eval_bare
#' @export
val <- function(rfr) {
  eval_bare(rfr$symbol, rfr)
}
