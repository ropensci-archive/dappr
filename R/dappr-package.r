#' General purpose R client for ERDDAP servers
#'
#' @importFrom httr GET content stop_for_status write_disk
#' @importFrom xml2 xml_text xml_find_all xml_children xml_name
#' xml_attr xml_attrs xml_ns read_html
#' @importFrom data.table rbindlist
#' @importFrom utils head read.csv read.delim
#' @import digest ncdf4
#' @name dappr-package
#' @aliases dappr
#' @docType package
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @keywords package
NULL
