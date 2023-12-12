
# statconvert

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

statconvert is a helpful function to translate statistical test outputs into a copy paste-able strings. 

## Installation

You can install the development version of statconvert with:

``` r
remotes::install_github("drewoolsen/statconvert")
```

## Example

If you wanted to translate the output of a oneway ANOVA test into text you could do 
``` r
library(statconvert)
data(mtcars)
md <- aov(mpg ~ wt, data = mtcars)
statconvert(md)
```

