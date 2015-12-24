#' Get dds information on an OPeNDAP dataset.
#'
#' @export
#'
#' @param id Dataset id
#' @param url A URL for an OPeNDAP server. Default: see \code{\link{durl}}
#' @param ... Further args passed on to \code{\link[httr]{GET}} (must be named parameters)
#' @return Prints a data.frame of metadata for a dataset
#' @examples \dontrun{
#' dds("MERRA_MONTHLY/MAIMNXINT.5.2.0/1980/MERRA100.prod.assim.instM_2d_int_Nx.198004.hdf")
#' dds("MERRA_MONTHLY/MSTMNXMLD.5.2.0/2011/MERRA300.prod.simul.tavgM_2d_mld_Nx.201105.hdf")
#'
#' # NASA's ocean color data
#' url <- 'http://oceandata.sci.gsfc.nasa.gov/opendap/'
#' dds('MODISA/L3SMI/2015/006/A2015006.L3m_DAY_CHL_chl_ocx_4km.nc', url)
#' }
dds <- function(id, url = durl(), ...) {
  url <- paste0(url, id, ".dds")
  res <- GET(url, ...)
  stop_for_status(res)
  xx <- content(res, "text")
  str <- gsub('Dataset \\{|\\}.+|\n|\\s\\s+', '', xx)
  strs <- strsplit(str, ";")[[1]]
  rbind_fill(
    lapply(strs, function(x) {
      type <- strextract(x, "[A-Za-z0-9]+")
      var <- strextract(x, "[A-Za-z0-9]+\\[.+")
      var_name <- strextract(var, "[A-Za-z0-9]+")
      atts <- lapply(strsplit(strextract(var, "\\[.+"), "\\]")[[1]], function(z) {
        out <- strtrim(strsplit(sub("\\[", "", z), "=")[[1]])
        setNames(list(out[2]), out[1])
      })
      all <- c(list(type = type, var = var_name), unlist(atts))
      all <- setNames(all, tolower(names(all)))
      toadd <- dds_vars()[!dds_vars() %in% names(all)]
      data.frame(c(all, setNames(rep("", length(toadd)), toadd)), stringsAsFactors = FALSE)
    })
  )
}

dds_vars <- function() c('type', 'var', 'time', 'ydim', 'xdim')
