# Manage Camera Trap data in R

<!-- badges: start -->
<!-- badges: end -->

The goal of cameraTrapping is to provide general access to the functions I use for managing my camera trap data and preparing it for analyses. Many of these functions produce outputs that can be directly inputted into other functions in this package, allowing one to create a easy to use workflow.

## Installation

You can install the latest version of cameraTrapping from github:

``` r
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("tomyamashta/cameraTrapping")
```

This package is currently not available on CRAN and likely will not be updated on CRAN unless I find someone who actually knows how to develop packages. 

## Example

This will eventually contain an example of the standard workflow for processing camera trap images (for now, it contains nothing...):

``` r
library(cameraTrapping)
## basic example code

# NOTE: If the suggested workflow is not followed, these functions are likely to result in errors. I have put in checks for some functions which will produce warnings or errors if something is wrong but I cannot guarantee that these will always work. 
```

## Acknowledgements
Kevin Ryer developed early versions of the APFun_env function, from which all of this is built on. 

Aidan Branney, Chloe Bates, and Duston Duffie helped test, debug, and improve various functions in this package. 


## Notes
This package is a constant work in progress. I do not have any formal training in programming, developing R packages, or anything like that so all the things that I am adding are done as I learn more about building packages. I've started learning about roxygen so data formatting and installation will likely go easier as we move along.
