#' List datasets for an OPeNDAP server
#'
#' @export
#' @param ... Any number of paths to drill down into an OPeNDAP server
#' @param url A URL for an OPeNDAP server. Default: see \code{\link{durl}}
#' @param callopts Curl options
#' @examples \dontrun{
#' od_list("MERRA_MONTHLY")
#' od_list("MERRA_MONTHLY", "MSTMNXMLD.5.2.0")
#' od_list("MERRA_MONTHLY", "MATMNXLND.5.2.0")
#' od_list("MERRA_MONTHLY", "MSTMNXMLD.5.2.0", "2000")
#' od_list("MERRA_MONTHLY", "MSTMNXMLD.5.2.0", "2015")
#'
#' # NASA's ocean color data
#' url <- 'https://oceandata.sci.gsfc.nasa.gov/opendap/'
#' od_list("MODISA", url = url)
#' od_list("MODISA", "L3SMI", url = url)
#' od_list("MODISA", "L3SMI", "2015", url = url)
#' od_list("MODISA", "L3SMI", "2015", "336", url = url)
#' }
od_list <- function(..., url = durl(), callopts = list()) {
  vars <- list(...)
  if (length(vars) == 0) stop("Please pass in some values", call. = FALSE)
  catl <- "catalog.xml"
  url <- paste0(url, paste0(unlist(vars), collapse = "/"), "/", catl)
  res <- GET(url, callopts)
  stop_for_status(res)
  xx <- content(res, "text")
  xml <- xml2::read_xml(xx)
  nodes <- xml2::xml_find_all(xml, "//thredds:catalogRef", ns = xml_ns(xml))
  if (length(nodes) == 0) {
    nodes <- xml_children(xml2::xml_find_all(xml, "//thredds:dataset", ns = xml_ns(xml))[[1]])
    rbind_fill(lapply(nodes, function(z) {
      metadata <- lapply(xml_children(z), function(w) {
        c(stats::setNames(list(value = xml_text(w)), xml_name(w)), as.list(xml_attrs(w)))
      })
      data.frame(c(name = xml_attr(z, "name"), as.list(unlist(metadata))), stringsAsFactors = FALSE)
    }))
  } else {
    rbind_fill(lapply(nodes, function(z) {
      data.frame(name = xml_attr(z, "name"), href = xml_attr(z, "href"), stringsAsFactors = FALSE)
    }))
  }
}
