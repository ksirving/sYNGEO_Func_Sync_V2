
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sYNGEO\_Func\_Sync\_V2

# Code index

1.  01a\_trait\_ordination\_interpolated\_abundances.R
2.  01b\_single\_traits\_interpolated\_abundances.R
3.  02a\_sync\_trait\_ord\_groups\_eff\_resp\_interpolated.R
4.  02b\_sync\_single\_traits\_groups\_eff\_resp\_interpolated.R
5.  03a\_dummy\_connectivity\_variable\_interpolated.R
6.  04\_differences\_in\_synchrony.R
7.  05a\_figures\_single\_traits.R
8.  05b\_figures\_ordination.R

# 01a\_trait\_ordination\_interpolated\_abundances.R

Takes raw abundance data and traits for each species cleans and
interpolates abundances. produces ordinations for each trait group

# 01b\_single\_traits\_interpolated\_abundances.R

same as above but with single traits Produces CWMs (interpolated) for
all traits and groups

### output\_data from scripts 02a & 02b & 3a & 3b are saved into output\_data/sync under .gitignore

# 02a\_sync\_trait\_ord\_groups\_eff\_resp\_interpolated.R

calculates synchrony with ordination scores within site, between site,
jackknife (leaving one year out)

# 02b\_sync\_single\_traits\_groups\_eff\_resp\_interpolated.R

calculates synchrony for single traits within site, between site,
jackknife (leaving one year out) also includes synchrony calculation for
temperature and flow

## 03a\_dummy\_connectivity\_variable\_interpolated.R

calculates distances and connectivity variable joins to all synchrony
datasets join on water course distance for testing

## 04\_differences\_in\_synchrony.R

calculates the differences in synchrony from the calculation of sync
using all years and the jack knife leave one out (LOO) some NAs in df,
from where correaltion couldn’t be calculated in scrip 02a/b

## 05a\_figures\_single\_traits.R

create figures for single trait synchrony and env variables. for overall
synchrony and contribution of years (leave one out, LOO)

## 05b\_figures\_ordination.R

create figures for ordination synchrony and env variables. for overall
synchrony and contribution of years (leave one out, LOO) also includes
temp and flow LOO figures

# Computation:

## CWM and Variance

``` r
devtools::install_github("alaindanet/ecocom")
#> Skipping install of 'ecocom' from a github remote, the SHA1 (1eb83dab) has not changed since last install.
#>   Use `force = TRUE` to force installation
library(ecocom)

#vector of species abundance
abun <- rpois(n = 10, lambda = 3)
# vector of species trait
trait <- rnorm(n = 10, mean = 10, sd = 3)

# Compute the moments of trait distribution
calc_cw_moments(trait = trait, weight = abun)
#>      mean  variance  skewness  kurtosis 
#> 9.7561220 4.4853780 0.3346265 2.2461459

# 
calc_cw_mean(trait = trait, weight = abun)
#> [1] 9.756122
calc_cw_variance(trait = trait, weight = abun)
#> [1] 4.485378
```

## Synchrony

Source the function at the beginning of your script:

``` r
source("https://raw.githubusercontent.com/alaindanet/fishcom/master/R/synchrony.R")

sync_mat <- matrix(
  c(0, 0, 1, 1),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    paste0("t", c(1, 2)),
    c("sp1", "sp2")
  )
)

# Complete synchrony
sync_mat
#>    sp1 sp2
#> t1   0   0
#> t2   1   1
compute_synchrony(cov(sync_mat))
#> [1] 1

# Complete asynchrony
async_mat <- sync_mat
async_mat[, 1] <- rev(async_mat[, 1])
compute_synchrony(cov(async_mat))
#> [1] 0
```

# Temperature data

## Moving average over 12 months

See
[doc/b-temperature.html](https://github.com/ksirving/sYNGEO_Func_Sync_V2/blob/main/doc/b-temperature.html)

``` r
library(tidyverse)
sites_water_air_tmean_av <- read_csv(
  here::here("input_data", "Env", "sites_water_air_tmean_av.csv")
)
#> Rows: 24508 Columns: 4
#> ── Column specification ──────────────────────────────────────────────────
#> Delimiter: ","
#> chr (1): siteid
#> dbl (3): year, air, water
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```
