#' Get dds information on an OPeNDAP dataset.
#'
#' FIXME - messy, messy parsing.
#'
#' @export
#'
#' @param id Dataset id
#' @template args
#' @return Prints a summary of the data on return, but you can index to various information.
#' @author Scott Chamberlain <myrmecocystus@@gmail.com>
#' @examples \dontrun{
#' ddx(id = "MERRA_MONTHLY/MSTMNXMLD.5.2.0/2015/MERRA300.prod.simul.tavgM_2d_mld_Nx.201507.hdf")
#'
#' # NASA's ocean color data
#' url <- 'https://oceandata.sci.gsfc.nasa.gov/opendap/'
#' ddx('MODISA/L3SMI/2015/006/A2015006.L3m_DAY_IOP_a_412_giop_4km.nc', url)
#' }
ddx <- function(id, url = durl(), ...) {
  url <- paste0(durl(), id, ".ddx")
  res <- GET(url, ...)
  stop_for_status(res)
  xx <- content(res, "text")
  xml <- xml2::read_xml(xx)
  childs <- xml2::xml_children(xml)
  out <- lapply(childs, function(z) {
    tmp <- xml_children(z)
    zname <- xml_attr(z, "name")
    atts <- list()
    dims <- list()
    for (i in seq_along(tmp)) {
      switch(xml_name(tmp[[i]]),
             Attribute = {
               if (xml_name(tmp[[i]]) == "Attribute") {
                 atts[[i]] <- lapply(xml2::as_list(xml2::xml_children(tmp[[i]])), function(b) {
                   lapply(xml_children(b), function(bb) {
                     lapply(xml_children(bb), function(cc) {
                       att <- xml_attrs(cc)
                       val <- gsub("\n\\s+", "", xml_text(cc))
                       list(c(as.list(att), list(value = val)))
                     })
                   })
                 })
               } else {
                 att <- xml_attrs(tmp[[i]])
                 val <- gsub("\n\\s+", "", xml_text(tmp[[i]]))
                 atts[[i]] <- list(c(as.list(att), list(value = val)))
               }
             },
             dimension = {
               dims[[i]] <- list(as.list(xml_attrs(tmp[[i]])))
             })
    }
    list(name = zname, attributes = unlist(atts, recursive = FALSE),
         dimensions = unlist(rc(dims), recursive = FALSE))
  })
  # dat <- lapply(out, function(z) data.frame(data.table::rbindlist(z)))
  structure(out, class = c("info", "ddx"), datasetid = id)
}

#' print.info <- function(x, ...){
#'   global <- x$alldata$NC_GLOBAL
#'   tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
#'   dims <- x$alldata[dimvars(x)]
#'   vars <- x$alldata[x$variables$variable_name]
#'   cat(sprintf("<ERDDAP info> %s", attr(x, "datasetid")), "\n")
#'   if(attr(x, "type") == "griddap") cat(" Dimensions (range): ", "\n")
#'   for(i in seq_along(dims)){
#'     if(names(dims[i]) == "time"){
#'       cat(sprintf("     time: (%s, %s)", tt[2], tt[1]), "\n")
#'     } else {
#'       cat(sprintf("     %s: (%s)", names(dims[i]), foo(dims[[i]], "actual_range")), "\n")
#'     }
#'   }
#'   cat(" Variables: ", "\n")
#'   for(i in seq_along(vars)){
#'     cat(sprintf("     %s:", names(vars[i])), "\n")
#'     ar <- foo(vars[[i]], "actual_range")
#'     if(!length(ar) == 0) cat("         Range:", foo(vars[[i]], "actual_range"), "\n")
#'     un <- foo(vars[[i]], "units")
#'     if(!length(un) == 0) cat("         Units:", foo(vars[[i]], "units"), "\n")
#'   }
#' }
#'
#' foo <- function(x, y){
#'   x[ x$attribute_name == y, "value"]
#' }
#'
#' #' @export
#' #' @rdname info
#' as.info <- function(x, url) {
#'   UseMethod("as.info")
#' }
#'
#' #' @export
#' as.info.info <- function(x, url) {
#'   x
#' }
#'
#' #' @export
#' as.info.character <- function(x, url) {
#'   info(x, url)
#' }
