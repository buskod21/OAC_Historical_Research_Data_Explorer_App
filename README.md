
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataExplorer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of DataExplorer is to enable user to explore and visualize
historical data from the Borealis database. The app enables user to
visualize the metadata for the data, check the structure of raw data,
see some basic descriptive statistics and plot the data.

## Installation

You can install DataExplorer package from [GitHub](https://github.com/)
with:

``` r
# install.packages("remotes")
remotes::install_github("agrifooddatacanada/OAC_Historical_Research_Data_Explorer_App/tree/dev")

# Run the App
library(DataExplorer)
```

## Run the app on your local computer

You can also run the app directly on your local computer using:

``` r
DataExplorer::run_app()
```

## Acknowledgement

This shiny web application was developed as a part of the Reusable
research data made shiny workshop that was held at the University of
Guelph, Guelph Ontario Canada.

``` r
#library(DataExplorer)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

Please note that the DataExplorer project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
