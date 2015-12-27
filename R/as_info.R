#' Coerce to info object
#'
#' @export
#' @param x input
#' @template args
#' @rdname info
as_info <- function(x, url, ...) {
  UseMethod("as_info")
}

#' @export
as_info.info <- function(x, url, ...) {
  x
}

#' @export
as_info.character <- function(x, url, ...) {
  print("x")
}
