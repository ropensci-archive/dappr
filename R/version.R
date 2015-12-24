#' Fetch version information
#'
#' @export
#' @examples \dontrun{
#' dapp_version(durl())
#' dapp_version('http://oceandata.sci.gsfc.nasa.gov/opendap/')
#' }
dapp_version <- function(url, ...) {
  x <- GET(file.path(url, "version"), ...)
  stop_for_status(x)
  content(x)
}
