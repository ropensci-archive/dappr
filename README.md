dappr
=====



[![Build Status](https://travis-ci.org/ropensci/dappr.svg)](https://travis-ci.org/ropensci/dappr)

`dappr` is a general purpose R client for working with OPeNDAP servers

## Installation


```r
devtools::install_github("ropensci/dappr")
```


```r
library("dappr")
```

## Search


```r
library("dplyr")
od_list("MERRA_MONTHLY", "MSTMNXMLD.5.2.0", "2000") %>%
  select(name, dataSize, date)
```

## Information


```r
dds("MERRA_MONTHLY/MAIMNXINT.5.2.0/1980/MERRA100.prod.assim.instM_2d_int_Nx.198004.hdf")
```

## Meta

* License: MIT
* Please [report any issues or bugs](https://github.com/ropensci/pangaear/issues).
* Get citation information for `pangaear` in R doing `citation(package = 'pangaear')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![ro_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
