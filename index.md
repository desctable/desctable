Desctable
================

[![Travis-CI Build
Status](https://travis-ci.org/desctable/desctable.svg?branch=master)](https://travis-ci.org/desctable/desctable)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/desctable)](https://cran.r-project.org/package=desctable)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/desctable)](https://www.r-pkg.org:443/pkg/desctable)
[![CRAN RStudio mirror total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/desctable)](https://www.r-pkg.org:443/pkg/desctable)

**Warning to existing users**  
*This version introduces a new API that should make the creation of
tables more flexible.  
The old API is still present but in a deprecated mode.  
See the roadmap below, and the website for the new usage.  
Suggestions about this change are welcome !*

------------------------------------------------------------------------

# Introduction

Desctable aims to be a simple and expressive interface to building
statistical tables in R.

# Installation

Install from CRAN with

    install.packages("desctable")

or install the development version from github with

    devtools::install_github("desctable/desctable")

# Basic usage

Load the package

``` r
library(desctable)
```

Simply apply `desc_table` on a dataframe or a grouped dataframe to get a
statistical table

``` r
iris %>%
  desc_table()
```

    ##                   Variables   N        % Min  Q1  Med     Mean  Q3 Max
    ## 1              Sepal.Length 150       NA 4.3 5.1 5.80 5.843333 6.4 7.9
    ## 2               Sepal.Width 150       NA 2.0 2.8 3.00 3.057333 3.3 4.4
    ## 3              Petal.Length 150       NA 1.0 1.6 4.35 3.758000 5.1 6.9
    ## 4               Petal.Width 150       NA 0.1 0.3 1.30 1.199333 1.8 2.5
    ## 5               **Species** 150       NA  NA  NA   NA       NA  NA  NA
    ## 6     **Species**: *setosa*  50 33.33333  NA  NA   NA       NA  NA  NA
    ## 7 **Species**: *versicolor*  50 33.33333  NA  NA   NA       NA  NA  NA
    ## 8  **Species**: *virginica*  50 33.33333  NA  NA   NA       NA  NA  NA
    ##          sd IQR
    ## 1 0.8280661 1.3
    ## 2 0.4358663 0.5
    ## 3 1.7652982 3.5
    ## 4 0.7622377 1.5
    ## 5        NA  NA
    ## 6        NA  NA
    ## 7        NA  NA
    ## 8        NA  NA

Declare the statistics you want to see, and give them the name of your
choice

``` r
iris %>%
  desc_table("N" = length,
             "%" = percent,
             mean,
             sd)
```

    ##                   Variables   N        %     mean        sd
    ## 1              Sepal.Length 150       NA 5.843333 0.8280661
    ## 2               Sepal.Width 150       NA 3.057333 0.4358663
    ## 3              Petal.Length 150       NA 3.758000 1.7652982
    ## 4               Petal.Width 150       NA 1.199333 0.7622377
    ## 5               **Species** 150       NA       NA        NA
    ## 6     **Species**: *setosa*  50 33.33333       NA        NA
    ## 7 **Species**: *versicolor*  50 33.33333       NA        NA
    ## 8  **Species**: *virginica*  50 33.33333       NA        NA

Create comparative tables, compute statistical tests and output to
`pander` for crisp markdown rendering!

``` r
mtcars %>%
  dplyr::mutate(cyl = factor(cyl),
                vs = factor(vs, labels = c("V-shaped", "straight")),
                am = factor(am, labels = c("automatic", "manual"))) %>%
  group_by(am) %>%
  desc_table(N = length,
             "%" = percent,
             "Median" = median,
             IQR) %>%
  desc_tests(vs = ~chisq.test) %>%
  desc_output("pander")
```

|              | am = manual</br> (N = 13)</br> N | %   | Median | IQR  | am = automatic</br> (N = 19)</br> N | %   | Median | IQR  | p      | test        |
|:-------------|:---------------------------------|:----|:-------|:-----|:------------------------------------|:----|:-------|:-----|:-------|:------------|
| mpg          | 13                               |     | 23     | 9.4  | 19                                  |     | 17     | 4.2  | ≤ 0.01 | wilcox.test |
| **cyl**      | 13                               |     |        |      | 19                                  |     |        |      | ≤ 0.01 | fisher.test |
|     4        | 8                                | 62  |        |      | 3                                   | 16  |        |      |        |             |
|     6        | 3                                | 23  |        |      | 4                                   | 21  |        |      |        |             |
|     8        | 2                                | 15  |        |      | 12                                  | 63  |        |      |        |             |
| disp         | 13                               |     | 120    | 81   | 19                                  |     | 276    | 164  | ≤ 0.01 | wilcox.test |
| hp           | 13                               |     | 109    | 47   | 19                                  |     | 175    | 76   | 0.046  | wilcox.test |
| drat         | 13                               |     | 4.1    | 0.37 | 19                                  |     | 3.1    | 0.63 | ≤ 0.01 | wilcox.test |
| wt           | 13                               |     | 2.3    | 0.84 | 19                                  |     | 3.5    | 0.41 | ≤ 0.01 | wilcox.test |
| qsec         | 13                               |     | 17     | 2.1  | 19                                  |     | 18     | 2    | 0.27   | wilcox.test |
| **vs**       | 13                               |     |        |      | 19                                  |     |        |      | 0.56   | chisq.test  |
|     V-shaped | 6                                | 46  |        |      | 12                                  | 63  |        |      |        |             |
|     straight | 7                                | 54  |        |      | 7                                   | 37  |        |      |        |             |
| gear         | 13                               |     | 4      | 1    | 19                                  |     | 3      | 0    | ≤ 0.01 | wilcox.test |
| carb         | 13                               |     | 2      | 3    | 19                                  |     | 3      | 2    | 0.74   | wilcox.test |

Read more in the [vignette](articles/desctable.html) !
