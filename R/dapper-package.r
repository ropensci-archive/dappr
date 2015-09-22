#' General purpose R client for ERDDAP servers
#'
#' @importFrom stats setNames
#' @importFrom methods is
#' @importFrom httr GET content stop_for_status
#' @importFrom xml2 xml_text xml_find_all xml_children xml_name
#' xml_attr xml_attrs xml_ns read_html
#' @importFrom data.table rbindlist
#' @import ncdf
#' @name dapper-package
#' @aliases dapper
#' @docType package
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @keywords package
NULL
