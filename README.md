dapper
=====

[![Build Status](https://api.travis-ci.org/ropensci/dapper)](https://travis-ci.org/ropensci/dapper)

`dapper` is a general purpose R client for working with OPeNDAP servers

## Installation


```r
devtools::install_github("dapper")
```


```r
library("dapper")
```

## Search


```r
library("dplyr")
od_search("MERRA_MONTHLY", "MSTMNXMLD.5.2.0", "2000") %>% 
  select(name, dataSize, date)
#>                                              name dataSize
#> 1  MERRA200.prod.simul.tavgM_2d_mld_Nx.200001.hdf 10269061
#> 2  MERRA200.prod.simul.tavgM_2d_mld_Nx.200002.hdf 10263231
#> 3  MERRA200.prod.simul.tavgM_2d_mld_Nx.200003.hdf 10281441
#> 4  MERRA200.prod.simul.tavgM_2d_mld_Nx.200004.hdf 10394672
#> 5  MERRA200.prod.simul.tavgM_2d_mld_Nx.200005.hdf 10570672
#> 6  MERRA200.prod.simul.tavgM_2d_mld_Nx.200006.hdf 10388078
#> 7  MERRA200.prod.simul.tavgM_2d_mld_Nx.200007.hdf 10231842
#> 8  MERRA200.prod.simul.tavgM_2d_mld_Nx.200008.hdf 10345456
#> 9  MERRA200.prod.simul.tavgM_2d_mld_Nx.200009.hdf 10676968
#> 10 MERRA200.prod.simul.tavgM_2d_mld_Nx.200010.hdf 10815976
#> 11 MERRA200.prod.simul.tavgM_2d_mld_Nx.200011.hdf 10659264
#> 12 MERRA200.prod.simul.tavgM_2d_mld_Nx.200012.hdf 10352159
#>                   date
#> 1  2012-03-29T06:34:27
#> 2  2012-03-29T06:34:28
#> 3  2012-03-29T06:34:28
#> 4  2012-03-29T06:34:29
#> 5  2012-03-29T06:34:30
#> 6  2012-03-29T06:34:31
#> 7  2012-03-29T06:34:31
#> 8  2012-03-29T06:34:32
#> 9  2012-03-29T06:34:33
#> 10 2012-03-29T06:34:34
#> 11 2012-03-29T06:34:35
#> 12 2012-03-29T06:34:36
```

## Information


```r
dds("MERRA_MONTHLY/MAIMNXINT.5.2.0/1980/MERRA100.prod.assim.instM_2d_int_Nx.198004.hdf")
#>       type  var time ydim xdim
#> 1  Float32  TQV    1  361  540
#> 2  Float32  TQI    1  361  540
#> 3  Float32  TQL    1  361  540
#> 4  Float32  TOX    1  361  540
#> 5  Float32 MASS    1  361  540
#> 6  Float32   KE    1  361  540
#> 7  Float32  CPT    1  361  540
#> 8  Float32  THV    1  361  540
#> 9  Float32  TQV    1  361  540
#> 10 Float32  TQI    1  361  540
#> 11 Float32  TQL    1  361  540
#> 12 Float32  TOX    1  361  540
#> 13 Float32 MASS    1  361  540
#> 14 Float32   KE    1  361  540
#> 15 Float32  CPT    1  361  540
#> 16 Float32  THV    1  361  540
#> 17 Float64 XDim            540
#> 18 Float64 YDim       361     
#> 19 Float64 TIME    1          
#> 20 Float64  EOS            540
#> 21 Float64  EOS       361     
#> 22 Float64 Time    1
```

## Meta

* License: MIT
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
