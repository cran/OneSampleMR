# OneSampleMR
<!-- badges: start -->
[![R-CMD-check](https://github.com/remlapmot/OneSampleMR/workflows/R-CMD-check/badge.svg)](https://github.com/remlapmot/OneSampleMR/actions?workflow=R-CMD-check)
<!-- badges: end -->

Useful functions for one sample Mendelian randomization / instrumental variable 
analyses, including implementations of:

* The [Sanderson and Windmeijer (2016)](https://doi.org/10.1016/j.jeconom.2015.06.004) conditional F-statistic for multiple exposure models.
* Various one-sample instrumental variable estimators including the
  * Multiplicative structural mean model (Robins, 1989; [Hernán and Robins, 2006](https://doi.org/10.1097/01.ede.0000222409.00878.37))<!--  * Logistic structural mean model (Robins, 1989; [Vansteelandt and Goetghebeur, 2003](https://doi.org/10.1046/j.1369-7412.2003.00417.x)) -->
  * Two-stage predictor substitution estimators ([Terza, 2008](https://doi.org/10.1016/j.jhealeco.2007.09.009))
  * Two-stage residual inclusion estimators ([Terza, 2008](https://doi.org/10.1016/j.jhealeco.2007.09.009))

## Installation

Install the package within R using
``` r
# install.packages("remotes") # uncomment on first run
remotes::install_github("remlapmot/OneSampleMR")
```

To check for and install updates to the package run this command again or alternatively run
``` r
remotes::update_packages()
```
