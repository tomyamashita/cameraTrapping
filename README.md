
# cameraTrapping

<!-- badges: start -->
<!-- badges: end -->

The goal of cameraTrapping is to provide general access to the functions I use for managing my camera trap data and preparing it for analyses. Many of these functions produce outputs that can be directly inputted into other functions in this package, allowing one to create a easy to use workflow.

## Installation

You can install the latest version of cameraTrapping from github:

``` r
devtools::install_github("tomyamashta/cameraTrapping")
```

This package is currently not available on CRAN and likely will not be updated on CRAN unless I find someone who actually knows how to develop packages. 

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cameraTrapping)
## basic example code

# NOTE: If the suggested workflow is not followed, these functions are likely to result in errors. I have put in checks for some functions which will produce warnings or errors if something is wrong but I cannot guarantee that these will always work. 
```

## Acknowledgements
Kevin Ryer developed early versions of the APFun_env function, from which all of this is built on. 

Aidan Branney, Chloe Bates, and Duston Duffie helped test, debug, and improve various functions in this package. 


## Changelog
0.0.0.1 - 2022-08-25
  Package created. 
  Added README, LICENSE, and NAMESPACE files
  Added package dependencies and imports
  Migrated many functions and their associated documentation from their original sources in my personal package.
  
