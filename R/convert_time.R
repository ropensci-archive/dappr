convert_time <- function(n = NULL, isoTime = NULL,
  units = "seconds since 1970-01-01T00:00:00Z", url = "http://coastwatch.pfeg.noaa.gov",
  method = "local", ...) {

  if (!is.null(n)) stopifnot(is.numeric(n))
  if (!is.null(isoTime)) stopifnot(is.character(isoTime))
  check1notboth(n, isoTime)
  if (method == "local") {
    format(as.POSIXct(rc(list(n, isoTime))[[1]], origin = "1970-01-01T00:00:00Z", tz = "UTC"),
           format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else {
    args <- rc(list(n = n, isoTime = isoTime, units = units))
    res <- GET(paste0(pu(url), '/erddap/convert/time.txt'), query = args, ...)
    stop_for_status(res)
    content(res, "text")
  }
}

depsub <- function(x) {
  deparse(substitute(x, env = parent.env()))
  # deparse(substitute(x, env = parent.frame()))
}

check1notboth <- function(x, y) {
  if (is.null(x) && is.null(y)) {
    stop(sprintf("One of %s or %s must be non-NULL", deparse(substitute(x)), deparse(substitute(y))), call. = FALSE)
  }
  if (!is.null(x) && !is.null(y)) {
    stop(sprintf("Supply only one of %s or %s", deparse(substitute(x)), deparse(substitute(y))), call. = FALSE)
  }
}
