## Test environment
* local Windows 10, R version 4.1.0
* r-hub check_for_cran()
* r-hub check(path = ".", platform = "solaris-x86-patched")
* remote Github Actions Windows, Mac, and Ubuntu 20.04 LTS, R version 4.1.2 and R devel

## R CMF check results
There where no ERRORs, WARNINGs or NOTES on local or remote builds RMD check
checking data for non-ASCII characters (2.2s)
     Note: found 27976 marked UTF-8 strings

We did the following changes based on the package since last: 
###Note: 
We translated all spatial functions and data from sp to sf. Functions still accept sp objects for backwards compatibility.
We fixed issue #27 in function summarize

## Downstream dependencies
There are currently no downstream dependencies for this package in CRAN
