`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

map <- function(.x, .f, ...) {
  lapply(X = .x, FUN = .f, ...)
}

map_lgl <- function(.x, .f, ...) {
  vapply(.x, FUN = .f, FUN.VALUE = logical(1L), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, FUN = .f, FUN.VALUE = character(1L), ...)
}

modify_tree <- function(x, leaf) {
  # I use this to modify elements of pairlists, so using `is.list`
  is_node <- is.list # rlang::is_bare_list
  worker <- function(x) {
    if (is_node(x)) {
      out <- map(x, worker)
    } else {
      out <- leaf(x)
    }
    out
  }
  worker(x)
}
