#' Get files
#'
#' @export
#'
#' @param x input
#' @param ... Dimension arguments. See examples. Can be any 1 or more of the dimensions
#' for the particular dataset - and the dimensions vary by dataset. For each dimension,
#' pass in a vector of length two, with min and max value desired.
#' @param fields (character) Fields to return, in a character vector.
#' @param stride (integer) How many values to get. 1 = get every value, 2 = get
#' every other value, etc. Default: 1 (i.e., get every value)
#' @param fmt (character) One of csv or nc (for netcdf). Default: nc
#' @param url A URL for an ERDDAP server. Default: \url{http://upwell.pfeg.noaa.gov/erddap/}
#' @param store One of \code{\link{disk}} (default) or \code{\link{memory}}. You
#' can pass options to \code{\link{disk}}
#' @param read (logical) Read data into memory or not. Does not apply when \code{store}
#' parameter is set to memory (which reads data into memory). For large csv, or
#' especially netcdf files, you may want to set this to \code{FALSE}, which simply
#' returns a summary of the dataset - and you can read in data piecemeal later.
#' Default: \code{TRUE}
#' @param callopts Pass on curl options to \code{\link[httr]{GET}}
#'
#' @return Prints a summary of the data on return, but you can index to various information.
#' @author Scott Chamberlain <myrmecocystus@@gmail.com>
#' @examples \dontrun{
#' fetch(id = "MERRA_MONTHLY/MSTMNXMLD.5.2.0/2015/MERRA300.prod.simul.tavgM_2d_mld_Nx.201507.hdf")
#'
#' # NASA's ocean color data
#' url <- 'http://oceandata.sci.gsfc.nasa.gov/opendap/'
#' fetch('MODISA/L3SMI/2015/006/A2015006.L3m_DAY_IOP_a_412_giop_4km.nc', url)
#'
#' x <- "MODISA/L3SMI/2015/006/A2015006.L3m_DAY_IOP_aph_unc_443_giop_9km.nc.nc4"
#' }
fetch <- function(x, ..., fields = 'all', stride = 1, fmt = "nc",
                    url = durl(), store = disk(), read = TRUE, callopts = list()) {

  tfile <- tempfile(fileext = ".nc")
  res <- GET(paste0(url, x), write_disk(path = tfile))


  #x <- as_info(x, url)
  dimargs <- list(...)
  check_dims(dimargs, x)
  check_lat_text(dimargs)
  check_lon_text(dimargs)
  dimargs <- fix_dims(dimargs, .info = x)
  check_lon_data_range(dimargs, x)
  check_lat_data_range(dimargs, x)
  d <- attr(x, "datasetid")
  var <- field_handler(fields, x$variables$variable_name)
  dims <- dimvars(x)
  store <- toggle_store(fmt, store)
  if (all(var == "none")) {
    args <- paste0(sapply(dims, function(y) parse_args(x, y, stride, dimargs, wname = TRUE)), collapse = ",")
  } else {
    pargs <- sapply(dims, function(y) parse_args(x, y, stride, dimargs))
    args <- paste0(lapply(var, function(y) paste0(y, paste0(pargs, collapse = ""))), collapse = ",")
  }
  fmt <- match.arg(fmt, c("nc", "csv"))
  resp <- erd_up_GET(url = sprintf("%sgriddap/%s.%s", url, d, fmt), dset = d,
                     args = args, store = store, fmt = fmt, callopts)
  loc <- if (store$store == "disk") resp else "memory"
  outclasses <- switch(fmt,
                       nc = c("griddap_nc", "nc"),
                       csv = c("griddap_csv", "csv", "data.frame"))
  read <- toggle_read(read, store)
  structure(read_all(resp, fmt, read), class = outclasses, datasetid = d, path = loc)
}

toggle_read <- function(x, store) {
  if (store$store == "memory") {
    return(TRUE)
  } else {
    return(x)
  }
}

toggle_store <- function(fmt, store) {
  if (fmt == "nc") {
    if (store$store == "memory") {
      disk()
    } else {
      store
    }
  } else {
    store
  }
}

#' @export
print.griddap_csv <- function(x, ..., n = 10){
  finfo <- file_info(attr(x, "path"))
  cat(sprintf("<ERDDAP griddap> %s", attr(x, "datasetid")), sep = "\n")
  path <- attr(x, "path")
  path2 <- if (file.exists(path)) path else "<beware: file deleted>"
  cat(sprintf("   Path: [%s]", path2), sep = "\n")
  if (attr(x, "path") != "memory") {
    cat(sprintf("   Last updated: [%s]", finfo$mtime), sep = "\n")
    cat(sprintf("   File size:    [%s mb]", finfo$size), sep = "\n")
  }
  cat(sprintf("   Dimensions:   [%s X %s]\n", NROW(x), NCOL(x)), sep = "\n")
  trunc_mat(x, n = n)
}

#' @export
print.griddap_nc <- function(x, ..., n = 10){
  finfo <- file_info(attr(x, "path"))
  cat(sprintf("<ERDDAP griddap> %s", attr(x, "datasetid")), sep = "\n")
  path <- attr(x, "path")
  path2 <- if (file.exists(path)) path else "<beware: file deleted>"
  cat(sprintf("   Path: [%s]", path2), sep = "\n")
  if (attr(x, "path") != "memory") {
    cat(sprintf("   Last updated: [%s]", finfo$mtime), sep = "\n")
    cat(sprintf("   File size:    [%s mb]", finfo$size), sep = "\n")
  }
  cat(sprintf("   Dimensions (dims/vars):   [%s X %s]", x$summary$ndims, x$summary$nvars), sep = "\n")
  cat(sprintf("   Dim names: %s", paste0(names(x$summary$dim), collapse = ", ")), sep = "\n")
  cat(sprintf("   Variable names: %s", paste0(unname(sapply(x$summary$var, "[[", "longname")), collapse = ", ")), sep = "\n")
  cat(sprintf("   data.frame (rows/columns):   [%s X %s]", dim(x$data)[1], dim(x$data)[2]), sep = "\n\n")
  trunc_mat(x$data, n = n)
}

field_handler <- function(x, y){
  x <- match.arg(x, c(y, "none", "all"), TRUE)
  if (length(x) == 1 && x == "all") {
    y
  } else if (all(x %in% y) || x == "none") {
    x
  }
}

check_dims <- function(dimargs, .info) {
  if (!all(names(dimargs) %in% dimvars(.info))) {
    stop(sprintf("Some input dimensions (%s) don't match those in dataset (%s)",
                 paste0(names(dimargs), collapse = ", "),
                 paste0(dimvars(.info), collapse = ", ")), call. = FALSE)
  }
}

check_lon_text <- function(dimargs) {
  if (!is.null(dimargs$longitude)) {
    if (any(sapply(dimargs$longitude, class) == "character")) {
      txt <- dimargs$longitude[sapply(dimargs$longitude, class) == "character"]
      if (!all(grepl("last", txt))) stop("Only text values allowed are 'last' & variants on that", call. = FALSE)
    }
  }
}

check_lat_text <- function(dimargs) {
  if (!is.null(dimargs$latitude)) {
    if (any(sapply(dimargs$latitude, class) == "character")) {
      txt <- dimargs$latitude[sapply(dimargs$latitude, class) == "character"]
      if (!all(grepl("last", txt))) stop("Only text values allowed are 'last' & variants on that", call. = FALSE)
    }
  }
}

is_lon_text <- function(dimargs) {
  if (!is.null(dimargs$longitude)) {
    any(sapply(dimargs$longitude, class) == "character")
  } else {
    FALSE
  }
}

is_lat_text <- function(dimargs) {
  if (!is.null(dimargs$latitude)) {
    any(sapply(dimargs$latitude, class) == "character")
  } else {
    FALSE
  }
}

check_lon_data_range <- function(dimargs, .info) {
  if (!is.null(dimargs$longitude)) {
    val <- .info$alldata$longitude[ .info$alldata$longitude$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim(strsplit(val, ",")[[1]]))
    if (!is_lon_text(dimargs)) {
      if (max(dimargs$longitude) > max(val2) || min(dimargs$longitude) < min(val2)) {
        stop(sprintf("One or both longitude values (%s) outside data range (%s)",
                     paste0(dimargs$longitude, collapse = ", "),
                     paste0(val2, collapse = ", ")), call. = FALSE)
      }
    }
  }
}

check_lat_data_range <- function(dimargs, .info) {
  if (!is.null(dimargs$latitude)) {
    val <- .info$alldata$latitude[ .info$alldata$latitude$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim(strsplit(val, ",")[[1]]))
    if (!is_lat_text(dimargs)) {
      if (max(dimargs$latitude) > max(val2) || min(dimargs$latitude) < min(val2)) {
        stop(sprintf("One or both latitude values (%s) outside data range (%s)",
                     paste0(dimargs$latitude, collapse = ", "),
                     paste0(val2, collapse = ", ")), call. = FALSE)
      }
    }
  }
}

fix_dims <- function(dimargs, .info) {
  for (i in seq_along(dimargs)) {
    tmp <- dimargs[[i]]
    nm <- names(dimargs[i])
    tmp <- grep("last+", tmp, value = TRUE, invert = TRUE)
    if (nm == "time") {
      tmp <- as.Date(tmp)
    }
    val <- .info$alldata[[nm]][ .info$alldata[[nm]]$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim(strsplit(val, ",")[[1]]))
    if (length(tmp) != 0) {
      if (which.min(val2) != which.min(tmp)) {
        dimargs[[i]] <- rev(dimargs[[i]])
      }
    }
  }
  dimargs
}

# which_min <- function(x) {
#   if (inherits(x, "character")) {
#     grep(min(tmp), tmp)
#   } else {
#     which.min(x)
#   }
# }

parse_args <- function(.info, dim, s, dimargs, wname = FALSE){
  tmp <- if (dim %in% names(dimargs)) {
    dimargs[[dim]]
  } else {
    if (dim == "time") {
      times <- c(getvar(.info, "time_coverage_start"), getvar(.info, "time_coverage_end"))
      sprintf('[(%s):%s:(%s)]', times[1], s, times[2])
    } else {
      actrange <- foo(.info$alldata[[dim]], "actual_range")
      gsub("\\s+", "", strsplit(actrange, ",")[[1]])
    }
  }

  if (length(s) > 1) {
    if (!length(s) == length(dimvars(.info))) stop("Your stride vector must equal length of dimension variables", call. = FALSE)
    names(s) <- dimvars(.info)
    if (!wname) {
      sprintf('[(%s):%s:(%s)]', tmp[1], s[[dim]], tmp[2])
    } else {
      sprintf('%s[(%s):%s:(%s)]', dim, tmp[1], s[[dim]], tmp[2])
    }
  } else {
    if (!wname) {
      if (length(tmp) == 1) {
        tmp
      } else {
        sprintf('[(%s):%s:(%s)]', tmp[1], s, tmp[2])
      }
    } else {
      if (length(tmp) == 1) {
        tmp
      } else {
        sprintf('%s[(%s):%s:(%s)]', dim, tmp[1], s, tmp[2])
      }
    }
  }
}

getvar <- function(x, y){
  x$alldata$NC_GLOBAL[ x$alldata$NC_GLOBAL$attribute_name == y, "value"]
}

getvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% c("NC_GLOBAL", "time", x$variables$variable_name) ]
}

getallvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% "NC_GLOBAL" ]
}

dimvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% c("NC_GLOBAL", x$variables$variable_name) ]
}

erd_up_GET <- function(url, dset, args, store, fmt, ...){
  if (store$store == "disk") {
    # store on disk
    key <- gen_key(url, args, fmt)
    if ( file.exists(file.path(store$path, key)) ) {
      file.path(store$path, key)
    } else {
      dir.create(store$path, showWarnings = FALSE, recursive = TRUE)
      res <- GET(url, query = args, write_disk(file.path(store$path, key), store$overwrite), ...)
      # delete file if error, and stop message
      err_handle(res, store, key)
      # return file path
      res$request$output$path
    }
  } else {
    # read into memory, bypass disk storage
    res <- GET(url, query = args, ...)
    # if error stop message
    err_handle(res, store, key)
    # return response object
    res
  }
}

writepath <- function(path, d, fmt) file.path(path, paste0(d, ".", fmt))

gen_key <- function(url, args, fmt) {
  ky <- paste0(url, "?", args)
  paste0(digest::digest(ky), ".", fmt)
}

file_info <- function(x) {
  tmp <- file.info(x)
  row.names(tmp) <- NULL
  tmp2 <- tmp[,c('mtime', 'size')]
  tmp2$size <- round(tmp2$size/1000000L, 2)
  tmp2
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)

foo <- function(x, y){
  x[ x$attribute_name == y, "value"]
}
