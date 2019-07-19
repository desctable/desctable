Desctable
================

[![Travis-CI Build
Status](https://travis-ci.org/MaximeWack/desctable.svg?branch=master)](https://travis-ci.org/MaximeWack/desctable)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/desctable)](https://cran.r-project.org/package=desctable)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/desctable)](http://www.r-pkg.org/pkg/desctable)

# Introduction

Desctable is a comprehensive descriptive and comparative tables
generator for R.

Every person doing data analysis has to create tables for descriptive
summaries of data (a.k.a. Table.1), or comparative tables.

Many packages, such as the aptly named **tableone**, address this issue.
However, they often include hard-coded behaviors, have outputs not
easily manipulable with standard R tools, or their syntax are
out-of-style (e.g. the argument order makes them difficult to use with
the pipe (`%>%`)).

Enter **desctable**, a package built with the following objectives in
mind:

  - generate descriptive and comparative statistics tables with nesting
  - keep the syntax as simple as possible
  - have good reasonable defaults
  - be entirely customizable, using standard R tools and functions
  - produce the simplest (as a data structure) output possible
  - provide helpers for different outputs
  - integrate with “modern” R usage, and the **tidyverse** set of tools
  - apply functional paradigms

# Installation

Install from CRAN with

    install.packages("desctable")

or install the development version from github with

    devtools::install_github("maximewack/desctable")

# Loading

``` r
# If you were to use DT, load it first
library(DT)

library(desctable)
library(pander) # pander can be loaded at any time
```

It is recommended to read this manual through its vignette:

``` r
vignette("desctable")
```

-----

# Descriptive tables

## Simple usage

**desctable** uses and exports the pipe (`%>%`) operator (from packages
**magrittr** and **dplyr** fame), though it is not mandatory to use it.

The single interface to the package is its eponymous `desctable`
function.

When used on a data.frame, it returns a descriptive table:

``` r
iris %>%
  desctable()
```

    ##                         N        %     Mean        sd  Med IQR
    ## 1        Sepal.Length 150       NA       NA        NA 5.80 1.3
    ## 2         Sepal.Width 150       NA 3.057333 0.4358663 3.00 0.5
    ## 3        Petal.Length 150       NA       NA        NA 4.35 3.5
    ## 4         Petal.Width 150       NA       NA        NA 1.30 1.5
    ## 5             Species 150       NA       NA        NA   NA  NA
    ## 6     Species: setosa  50 33.33333       NA        NA   NA  NA
    ## 7 Species: versicolor  50 33.33333       NA        NA   NA  NA
    ## 8  Species: virginica  50 33.33333       NA        NA   NA  NA

``` r
desctable(mtcars)
```

    ##          N      Mean        sd     Med       IQR
    ## 1   mpg 32 20.090625 6.0269481  19.200   7.37500
    ## 2   cyl 32        NA        NA   6.000   4.00000
    ## 3  disp 32        NA        NA 196.300 205.17500
    ## 4    hp 32        NA        NA 123.000  83.50000
    ## 5  drat 32  3.596563 0.5346787   3.695   0.84000
    ## 6    wt 32        NA        NA   3.325   1.02875
    ## 7  qsec 32 17.848750 1.7869432  17.710   2.00750
    ## 8    vs 32        NA        NA   0.000   1.00000
    ## 9    am 32        NA        NA   0.000   1.00000
    ## 10 gear 32        NA        NA   4.000   1.00000
    ## 11 carb 32        NA        NA   2.000   2.00000

As you can see with these two examples, `desctable` describes every
variable, with individual levels for factors. It picks statistical
functions depending on the type and distribution of the variables in the
data, and applies those statistical functions only on the relevant
variables.

## Output

The object produced by `desctable` is in fact a list of data.frames,
with a “desctable” class.  
Methods for reduction to a simple dataframe (`as.data.frame`,
automatically used for printing), conversion to markdown (`pander`), and
interactive html output with **DT** (`datatable`) are provided:

``` r
iris %>%
  desctable() %>%
  pander()
```

| Â            | N   | %  | Mean | sd   | Med | IQR |
| :----------- | :-- | :- | :--- | :--- | :-- | :-- |
| Sepal.Length | 150 |    |      |      | 5.8 | 1.3 |
| Sepal.Width  | 150 |    | 3.1  | 0.44 | 3   | 0.5 |
| Petal.Length | 150 |    |      |      | 4.3 | 3.5 |
| Petal.Width  | 150 |    |      |      | 1.3 | 1.5 |
| **Species**  | 150 |    |      |      |     |     |
| setosa       | 50  | 33 |      |      |     |     |
| versicolor   | 50  | 33 |      |      |     |     |
| virginica    | 50  | 33 |      |      |     |     |

<br> You need to load these two packages first (and prior to
**desctable** for **DT**) if you want to use them.

Calls to `pander` and `datatable` with “regular” dataframes will not be
affected by the defaults used in the package, and you can modify these
defaults for **desctable** objects.

The `datatable` wrapper function for desctable objects comes with some
default options and formatting such as freezing the row names and table
header, export buttons, and rounding of values. Both `pander` and
`datatable` wrapper take a *digits* argument to set the number of
decimals to show. (`pander` uses the *digits*, *justify* and *missing*
arguments of `pandoc.table`, whereas `datatable` calls `prettyNum` with
the `digits` parameter, and removes `NA` values. You can set `digits =
NULL` if you want the full table and format it yourself)

## Advanced usage

`desctable` chooses statistical functions for you using this algorithm:

  - always show N
  - if there are factors, show %
  - if there are normally distributed variables, show Mean and SD
  - if there are non-normally distributed variables, show Median and IQR

For each variable in the table, compute the relevant statistical
functions in that list (non-applicable functions will safely return
`NA`).

How does it work, and how can you adapt this behavior to your needs?

`desctable` takes an optional *stats* argument. This argument can either
be:

  - an automatic function to select appropriate statistical functions
  - or a named list of
      - statistical functions
      - formulas describing conditions to use a statistical function.

### Automatic function

This is the default, using the `stats_auto` function provided in the
package.

Several other “automatic statistical functions” are defined in this
package: `stats_auto`, `stats_default`, `stats_normal`,
`stats_nonnormal`.

You can also provide your own automatic function, which needs to

  - accept a dataframe as its argument (whether to use this dataframe or
    not in the function is your choice), and
  - return a named list of statistical functions to use, as defined in
    the subsequent paragraphs.

<!-- end list -->

``` r
# Strictly equivalent to iris %>% desctable() %>% pander()
iris %>%
  desctable(stats = stats_auto) %>%
  pander()
```

| Â            | N   | %  | Mean | sd   | Med | IQR |
| :----------- | :-- | :- | :--- | :--- | :-- | :-- |
| Sepal.Length | 150 |    |      |      | 5.8 | 1.3 |
| Sepal.Width  | 150 |    | 3.1  | 0.44 | 3   | 0.5 |
| Petal.Length | 150 |    |      |      | 4.3 | 3.5 |
| Petal.Width  | 150 |    |      |      | 1.3 | 1.5 |
| **Species**  | 150 |    |      |      |     |     |
| setosa       | 50  | 33 |      |      |     |     |
| versicolor   | 50  | 33 |      |      |     |     |
| virginica    | 50  | 33 |      |      |     |     |

### Statistical functions

Statistical functions can be any function defined in R that you want to
use, such as `length` or `mean`.

The only condition is that they return a single numerical value. One
exception is when they return a vector of length `1 + nlevels(x)` when
applied to factors, as is needed for the `percent` function.

As mentioned above, they need to be used inside a named list, such as

``` r
mtcars %>%
  desctable(stats = list("N" = length, "Mean" = mean, "SD" = sd)) %>%
  pander()
```

| Â    | N  | Mean | SD   |
| :--- | :- | :--- | :--- |
| mpg  | 32 | 20   | 6    |
| cyl  | 32 | 6.2  | 1.8  |
| disp | 32 | 231  | 124  |
| hp   | 32 | 147  | 69   |
| drat | 32 | 3.6  | 0.53 |
| wt   | 32 | 3.2  | 0.98 |
| qsec | 32 | 18   | 1.8  |
| vs   | 32 | 0.44 | 0.5  |
| am   | 32 | 0.41 | 0.5  |
| gear | 32 | 3.7  | 0.74 |
| carb | 32 | 2.8  | 1.6  |

<br>

The names will be used as column headers in the resulting table, and the
functions will be applied safely on the variables (errors return `NA`,
and for factors the function will be used on individual levels).

Several convenience functions are included in this package. For
statistical function we have: `percent`, which prints percentages of
levels in a factor, and `IQR` which re-implements `stats::IQR` but works
better with `NA` values.

Be aware that **all functions will be used on variables stripped of
their `NA` values\!**  
This is necessary for most statistical functions to be useful, and makes
**N** (`length`) show only the number of observations in the dataset for
each variable.

### Conditional formulas

The general form of these formulas is

``` r
predicate_function ~ stat_function_if_TRUE | stat_function_if_FALSE
```

A predicate function is any function returning either `TRUE` or `FALSE`
when applied on a vector, such as `is.factor`, `is.numeric`, and
`is.logical`.  
**desctable** provides the `is.normal` function to test for normality
(it is equivalent to `length(na.omit(x)) > 30 & shapiro.test(x)$p.value
> .1`).

The *FALSE* option can be omitted and `NA` will be produced if the
condition in the predicate is not met.

These statements can be nested using parentheses.  
For example:

`is.factor ~ percent | (is.normal ~ mean)`

will either use `percent` if the variable is a factor, or `mean` if and
only if the variable is normally distributed.

You can mix “bare” statistical functions and formulas in the list
defining the statistics you want to use in your table.

``` r
iris %>%
  desctable(stats = list("N"      = length,
                         "%/Mean" = is.factor ~ percent | (is.normal ~ mean),
                         "Median" = is.normal ~ NA | median)) %>%
  pander()
```

| Â            | N   | %/Mean | Median |
| :----------- | :-- | :----- | :----- |
| Sepal.Length | 150 |        | 5.8    |
| Sepal.Width  | 150 | 3.1    |        |
| Petal.Length | 150 |        | 4.3    |
| Petal.Width  | 150 |        | 1.3    |
| **Species**  | 150 |        |        |
| setosa       | 50  | 33     |        |
| versicolor   | 50  | 33     |        |
| virginica    | 50  | 33     |        |

<br>

For reference, here is the body of the `stats_auto` function in the
package:

    ## function (data) 
    ## {
    ##     shapiro <- data %>% Filter(f = is.numeric) %>% lapply(is.normal) %>% 
    ##         unlist
    ##     if (length(shapiro) == 0) {
    ##         normal <- F
    ##         nonnormal <- F
    ##     }
    ##     else {
    ##         normal <- any(shapiro)
    ##         nonnormal <- any(!shapiro)
    ##     }
    ##     fact <- any(data %>% lapply(is.factor) %>% unlist)
    ##     if (fact & normal & !nonnormal) 
    ##         stats_normal(data)
    ##     else if (fact & !normal & nonnormal) 
    ##         stats_nonnormal(data)
    ##     else if (fact & !normal & !nonnormal) 
    ##         list(N = length, `%` = percent)
    ##     else if (!fact & normal & nonnormal) 
    ##         list(N = length, Mean = is.normal ~ mean, sd = is.normal ~ 
    ##             sd, Med = stats::median, IQR = is.factor ~ NA | IQR)
    ##     else if (!fact & normal & !nonnormal) 
    ##         list(N = length, Mean = mean, sd = stats::sd)
    ##     else if (!fact & !normal & nonnormal) 
    ##         list(N = length, Med = stats::median, IQR = IQR)
    ##     else stats_default(data)
    ## }
    ## <bytecode: 0x0000000018ea3820>
    ## <environment: namespace:desctable>

### Labels

It is often the case that variable names are not “pretty” enough to be
used as-is in a table.  
Although you could still edit the variable labels in the table
afterwards using subsetting or string replacement functions, it is
possible to mention a **labels** argument.

The **labels** argument is a named character vector associating variable
names and labels.  
You don’t need to provide labels for all the variables, and extra labels
will be silently discarded. This allows you to define a “global” labels
vector and use it for every table even after variable selections.

``` r
mtlabels <- c(mpg  = "Miles/(US) gallon",
              cyl  = "Number of cylinders",
              disp = "Displacement (cu.in.)",
              hp   = "Gross horsepower",
              drat = "Rear axle ratio",
              wt   = "Weight (1000 lbs)",
              qsec = "¼ mile time",
              vs   = "V/S",
              am   = "Transmission",
              gear = "Number of forward gears",
              carb = "Number of carburetors")

mtcars %>%
  dplyr::mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>%
  desctable(labels = mtlabels) %>%
  pander()
```

| Â                       | N  | %  | Mean | sd   | Med | IQR  |
| :---------------------- | :- | :- | :--- | :--- | :-- | :--- |
| Miles/(US) gallon       | 32 |    | 20   | 6    | 19  | 7.4  |
| Number of cylinders     | 32 |    |      |      | 6   | 4    |
| Displacement (cu.in.)   | 32 |    |      |      | 196 | 205  |
| Gross horsepower        | 32 |    |      |      | 123 | 84   |
| Rear axle ratio         | 32 |    | 3.6  | 0.53 | 3.7 | 0.84 |
| Weight (1000 lbs)       | 32 |    |      |      | 3.3 | 1    |
| Â¼ mile time            | 32 |    | 18   | 1.8  | 18  | 2    |
| V/S                     | 32 |    |      |      | 0   | 1    |
| **Transmission**        | 32 |    |      |      |     |      |
| Automatic               | 19 | 59 |      |      |     |      |
| Manual                  | 13 | 41 |      |      |     |      |
| Number of forward gears | 32 |    |      |      | 4   | 1    |
| Number of carburetors   | 32 |    |      |      | 2   | 2    |

<br>

-----

# Comparative tables

## Simple usage

Creating a comparative table (between groups defined by a factor) using
`desctable` is as easy as creating a descriptive table.

It uses the well known `group_by` function from **dplyr**:

``` r
iris %>%
  group_by(Species) %>%
  desctable() -> iris_by_Species

iris_by_Species
```

    ##                Species: setosa (n=50) / N Species: setosa (n=50) / Mean
    ## 1 Sepal.Length                         50                         5.006
    ## 2  Sepal.Width                         50                         3.428
    ## 3 Petal.Length                         50                            NA
    ## 4  Petal.Width                         50                            NA
    ##   Species: setosa (n=50) / sd Species: setosa (n=50) / Med
    ## 1                   0.3524897                          5.0
    ## 2                   0.3790644                          3.4
    ## 3                          NA                          1.5
    ## 4                          NA                          0.2
    ##   Species: setosa (n=50) / IQR Species: versicolor (n=50) / N1
    ## 1                        0.400                              50
    ## 2                        0.475                              50
    ## 3                        0.175                              50
    ## 4                        0.100                              50
    ##   Species: versicolor (n=50) / Mean1 Species: versicolor (n=50) / sd1
    ## 1                              5.936                        0.5161711
    ## 2                              2.770                        0.3137983
    ## 3                              4.260                        0.4699110
    ## 4                                 NA                               NA
    ##   Species: versicolor (n=50) / Med1 Species: versicolor (n=50) / IQR1
    ## 1                              5.90                             0.700
    ## 2                              2.80                             0.475
    ## 3                              4.35                             0.600
    ## 4                              1.30                             0.300
    ##   Species: virginica (n=50) / N2 Species: virginica (n=50) / Mean2
    ## 1                             50                             6.588
    ## 2                             50                             2.974
    ## 3                             50                             5.552
    ## 4                             50                                NA
    ##   Species: virginica (n=50) / sd2 Species: virginica (n=50) / Med2
    ## 1                       0.6358796                             6.50
    ## 2                       0.3224966                             3.00
    ## 3                       0.5518947                             5.55
    ## 4                              NA                             2.00
    ##   Species: virginica (n=50) / IQR2    tests / p
    ## 1                            0.675 1.505059e-28
    ## 2                            0.375 4.492017e-17
    ## 3                            0.775 4.803974e-29
    ## 4                            0.500 3.261796e-29
    ##                       tests / test
    ## 1 . %>% oneway.test(var.equal = F)
    ## 2 . %>% oneway.test(var.equal = T)
    ## 3                     kruskal.test
    ## 4                     kruskal.test

The result is a table containing a descriptive subtable for each level
of the grouping factor (the statistical functions rules are applied to
each subtable independently), with the statistical tests performed, and
their p values.

When displayed as a flat dataframe, the grouping header appears in each
variable.

You can also see the grouping headers by inspecting the resulting
object, which is a deep list of dataframes, each dataframe named after
the grouping factor and its levels (with sample size for each).

``` r
str(iris_by_Species)
```

    ## List of 5
    ##  $ Variables                 :'data.frame':  4 obs. of  1 variable:
    ##   ..$ Variables: chr [1:4] "Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width"
    ##  $ Species: setosa (n=50)    :'data.frame':  4 obs. of  5 variables:
    ##   ..$ N   : int [1:4] 50 50 50 50
    ##   ..$ Mean: num [1:4] 5.01 3.43 NA NA
    ##   ..$ sd  : num [1:4] 0.352 0.379 NA NA
    ##   ..$ Med : num [1:4] 5 3.4 1.5 0.2
    ##   ..$ IQR : num [1:4] 0.4 0.475 0.175 0.1
    ##  $ Species: versicolor (n=50):'data.frame':  4 obs. of  5 variables:
    ##   ..$ N   : int [1:4] 50 50 50 50
    ##   ..$ Mean: num [1:4] 5.94 2.77 4.26 NA
    ##   ..$ sd  : num [1:4] 0.516 0.314 0.47 NA
    ##   ..$ Med : num [1:4] 5.9 2.8 4.35 1.3
    ##   ..$ IQR : num [1:4] 0.7 0.475 0.6 0.3
    ##  $ Species: virginica (n=50) :'data.frame':  4 obs. of  5 variables:
    ##   ..$ N   : int [1:4] 50 50 50 50
    ##   ..$ Mean: num [1:4] 6.59 2.97 5.55 NA
    ##   ..$ sd  : num [1:4] 0.636 0.322 0.552 NA
    ##   ..$ Med : num [1:4] 6.5 3 5.55 2
    ##   ..$ IQR : num [1:4] 0.675 0.375 0.775 0.5
    ##  $ tests                     :'data.frame':  4 obs. of  2 variables:
    ##   ..$ p   : num [1:4] 1.51e-28 4.49e-17 4.80e-29 3.26e-29
    ##   ..$ test: chr [1:4] ". %>% oneway.test(var.equal = F)" ". %>% oneway.test(var.equal = T)" "kruskal.test" "kruskal.test"
    ##  - attr(*, "class")= chr "desctable"

You can specify groups based on any variable, not only factors:

``` r
# With pander output
mtcars %>%
  group_by(cyl) %>%
  desctable() %>%
  pander()
```

| Â    | cyl: 4 (n=11)<br/>N | <br/>Med | <br/>IQR | cyl: 6 (n=7)<br/>N1 | <br/>Med1 | <br/>IQR1 | cyl: 8 (n=14)<br/>N2 | <br/>Med2 | <br/>IQR2 | tests<br/>p | <br/>test    |
| :--- | :------------------ | :------- | :------- | :------------------ | :-------- | :-------- | :------------------- | :-------- | :-------- | :---------- | :----------- |
| mpg  | 11                  | 26       | 7.6      | 7                   | 20        | 2.4       | 14                   | 15        | 1.8       | 2.6e-06     | kruskal.test |
| disp | 11                  | 108      | 42       | 7                   | 168       | 36        | 14                   | 350       | 88        | 1.6e-06     | kruskal.test |
| hp   | 11                  | 91       | 30       | 7                   | 110       | 13        | 14                   | 192       | 65        | 3.3e-06     | kruskal.test |
| drat | 11                  | 4.1      | 0.35     | 7                   | 3.9       | 0.56      | 14                   | 3.1       | 0.15      | 0.00075     | kruskal.test |
| wt   | 11                  | 2.2      | 0.74     | 7                   | 3.2       | 0.62      | 14                   | 3.8       | 0.48      | 1.1e-05     | kruskal.test |
| qsec | 11                  | 19       | 1.4      | 7                   | 18        | 2.4       | 14                   | 17        | 1.5       | 0.0062      | kruskal.test |
| vs   | 11                  | 1        | 0        | 7                   | 1         | 1         | 14                   | 0         | 0         | 3.2e-05     | kruskal.test |
| am   | 11                  | 1        | 0.5      | 7                   | 0         | 1         | 14                   | 0         | 0         | 0.014       | kruskal.test |
| gear | 11                  | 4        | 0        | 7                   | 4         | 0.5       | 14                   | 3         | 0         | 0.0062      | kruskal.test |
| carb | 11                  | 2        | 1        | 7                   | 4         | 1.5       | 14                   | 3.5       | 1.8       | 0.0017      | kruskal.test |

Also with conditions:

``` r
iris %>%
  group_by(Petal.Length > 5) %>%
  desctable() %>%
  pander()
```

| Â            | Petal.Length \> 5: FALSE (n=108)<br/>N | <br/>% | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | Petal.Length \> 5: TRUE (n=42)<br/>N1 | <br/>%1 | <br/>Mean1 | <br/>sd1 | <br/>Med1 | <br/>IQR1 | tests<br/>p | <br/>test   |
| :----------- | :------------------------------------- | :----- | :-------- | :------ | :------- | :------- | :------------------------------------ | :------ | :--------- | :------- | :-------- | :-------- | :---------- | :---------- |
| Sepal.Length | 108                                    |        |           |         | 5.5      | 1        | 42                                    |         |            |          | 6.7       | 0.85      | 1.6e-15     | wilcox.test |
| Sepal.Width  | 108                                    |        | 3.1       | 0.48    | 3        | 0.6      | 42                                    |         |            |          | 3         | 0.4       | 0.69        | wilcox.test |
| Petal.Length | 108                                    |        |           |         | 3.5      | 3        | 42                                    |         |            |          | 5.6       | 0.67      | 2.1e-21     | wilcox.test |
| Petal.Width  | 108                                    |        |           |         | 1        | 1.2      | 42                                    |         | 2.1        | 0.28     | 2.1       | 0.47      | 1.6e-19     | wilcox.test |
| **Species**  | 108                                    |        |           |         |          |          | 42                                    |         |            |          |           |           | 2.5e-26     | fisher.test |
| setosa       | 50                                     | 46     |           |         |          |          | 0                                     | 0       |            |          |           |           |             |             |
| versicolor   | 49                                     | 45     |           |         |          |          | 1                                     | 2.4     |            |          |           |           |             |             |
| virginica    | 9                                      | 8.3    |           |         |          |          | 41                                    | 98      |            |          |           |           |             |             |

<br>

And even on multiple nested groups:

``` r
mtcars %>%
  dplyr::mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>%
  group_by(vs, am, cyl) %>%
  desctable() %>%
  pander()
```

| Â    | vs: 0 (n=18)<br/>am: Automatic (n=12)<br/>cyl: 8 (n=12)<br/>N | <br/><br/><br/>Med | <br/><br/><br/>IQR | <br/><br/>tests<br/>p | <br/><br/><br/>test | <br/>am: Manual (n=6)<br/>cyl: 4 (n=1)<br/>N3 | <br/><br/><br/>Med3 | <br/><br/><br/>IQR3 | <br/><br/>cyl: 6 (n=3)<br/>N1 | <br/><br/><br/>Med1 | <br/><br/><br/>IQR1 | <br/><br/>cyl: 8 (n=2)<br/>N2 | <br/><br/><br/>Med2 | <br/><br/><br/>IQR2 | <br/><br/>tests<br/>p1 | <br/><br/><br/>test1 | vs: 1 (n=14)<br/>am: Automatic (n=7)<br/>cyl: 4 (n=3)<br/>N4 | <br/><br/><br/>Med4 | <br/><br/><br/>IQR4 | <br/><br/>cyl: 6 (n=4)<br/>N11 | <br/><br/><br/>Med11 | <br/><br/><br/>IQR11 | <br/><br/>tests<br/>p2 | <br/><br/><br/>test2 | <br/>am: Manual (n=7)<br/>cyl: 4 (n=7)<br/>N21 | <br/><br/><br/>Med21 | <br/><br/><br/>IQR21 | <br/><br/>tests<br/>p11 | <br/><br/><br/>test11 |
| :--- | :------------------------------------------------------------ | :----------------- | :----------------- | :-------------------- | :------------------ | :-------------------------------------------- | :------------------ | :------------------ | :---------------------------- | :------------------ | :------------------ | :---------------------------- | :------------------ | :------------------ | :--------------------- | :------------------- | :----------------------------------------------------------- | :------------------ | :------------------ | :----------------------------- | :------------------- | :------------------- | :--------------------- | :------------------- | :--------------------------------------------- | :------------------- | :------------------- | :---------------------- | :-------------------- |
| mpg  | 12                                                            | 15                 | 2.6                |                       | no.test             | 1                                             | 26                  | 0                   | 3                             | 21                  | 0.65                | 2                             | 15                  | 0.4                 | 0.11                   | kruskal.test         | 3                                                            | 23                  | 1.5                 | 4                              | 19                   | 1.7                  | 0.057                  | wilcox.test          | 7                                              | 30                   | 6.3                  |                         | no.test               |
| disp | 12                                                            | 355                | 113                |                       | no.test             | 1                                             | 120                 | 0                   | 3                             | 160                 | 7.5                 | 2                             | 326                 | 25                  | 0.11                   | kruskal.test         | 3                                                            | 141                 | 13                  | 4                              | 196                  | 66                   | 0.05                   | wilcox.test          | 7                                              | 79                   | 24                   |                         | no.test               |
| hp   | 12                                                            | 180                | 44                 |                       | no.test             | 1                                             | 91                  | 0                   | 3                             | 110                 | 32                  | 2                             | 300                 | 36                  | 0.11                   | kruskal.test         | 3                                                            | 95                  | 18                  | 4                              | 116                  | 14                   | 0.05                   | wilcox.test          | 7                                              | 66                   | 36                   |                         | no.test               |
| drat | 12                                                            | 3.1                | 0.11               |                       | no.test             | 1                                             | 4.4                 | 0                   | 3                             | 3.9                 | 0.14                | 2                             | 3.9                 | 0.34                | 0.33                   | kruskal.test         | 3                                                            | 3.7                 | 0.11                | 4                              | 3.5                  | 0.92                 | 0.85                   | wilcox.test          | 7                                              | 4.1                  | 0.2                  |                         | no.test               |
| wt   | 12                                                            | 3.8                | 0.81               |                       | no.test             | 1                                             | 2.1                 | 0                   | 3                             | 2.8                 | 0.13                | 2                             | 3.4                 | 0.2                 | 0.12                   | kruskal.test         | 3                                                            | 3.1                 | 0.36                | 4                              | 3.4                  | 0.061                | 0.05                   | wilcox.test          | 7                                              | 1.9                  | 0.53                 |                         | no.test               |
| qsec | 12                                                            | 17                 | 0.67               |                       | no.test             | 1                                             | 17                  | 0                   | 3                             | 16                  | 0.76                | 2                             | 15                  | 0.05                | 0.17                   | kruskal.test         | 3                                                            | 20                  | 1.4                 | 4                              | 19                   | 0.89                 | 0.23                   | wilcox.test          | 7                                              | 19                   | 0.62                 |                         | no.test               |
| gear | 12                                                            | 3                  | 0                  |                       | no.test             | 1                                             | 5                   | 0                   | 3                             | 4                   | 0.5                 | 2                             | 5                   | 0                   | 0.29                   | kruskal.test         | 3                                                            | 4                   | 0.5                 | 4                              | 3.5                  | 1                    | 0.84                   | wilcox.test          | 7                                              | 4                    | 0                    |                         | no.test               |
| carb | 12                                                            | 3                  | 2                  |                       | no.test             | 1                                             | 2                   | 0                   | 3                             | 4                   | 1                   | 2                             | 6                   | 2                   | 0.26                   | kruskal.test         | 3                                                            | 2                   | 0.5                 | 4                              | 2.5                  | 3                    | 0.85                   | wilcox.test          | 7                                              | 1                    | 1                    |                         | no.test               |

<br>

In the case of nested groups (a.k.a. sub-group analysis), statistical
tests are performed only between the groups of the deepest grouping
level.

Statistical tests are automatically selected depending on the data and
the grouping factor.

## Advanced usage

`desctable` choses the statistical tests using the following algorithm:

  - if the variable is a factor, use `fisher.test`
  - if the grouping factor has only one level, use the provided
    `no.test` (which does nothing)
  - if the grouping factor has two levels
      - and the variable presents homoskedasticity (p value for
        `var.test` \> .1) and normality of distribution in both groups,
        use `t.test(var.equal = T)`
      - and the variable does not present homoskedasticity (p value for
        `var.test` \< .1) but normality of distribution in both groups,
        use `t.test(var.equal = F)`
      - else use `wilcox.test`
  - if the grouping factor has more than two levels
      - and the variable presents homoskedasticity (p value for
        `bartlett.test` \> .1) and normality of distribution in all
        groups, use `oneway.test(var.equal = T)`
      - and the variable does not present homoskedasticity (p value for
        `bartlett.test` \< .1) but normality of distribution in all
        groups, use `oneway.test(var.equal = F)`
      - else use `kruskal.test`

But what if you want to pick a specific test for a specific variable, or
change all the tests altogether?

`desctable` takes an optional *tests* argument. This argument can either
be

  - an automatic function to select appropriate statistical test
    functions
  - or a named list of statistical test functions

### Automatic function

This is the default, using the `tests_auto` function provided in the
package.

You can also provide your own automatic function, which needs to

  - accept a variable and a grouping factor as its arguments, and
  - return a single-term formula containing a statistical test function.

This function will be used on every variable and every grouping factor
to determine the appropriate test.

``` r
# Strictly equivalent to iris %>% group_by(Species) %>% desctable %>% pander
iris %>%
  group_by(Species) %>%
  desctable(tests = tests_auto) %>%
  pander()
```

| Â            | Species: setosa (n=50)<br/>N | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | Species: versicolor (n=50)<br/>N1 | <br/>Mean1 | <br/>sd1 | <br/>Med1 | <br/>IQR1 | Species: virginica (n=50)<br/>N2 | <br/>Mean2 | <br/>sd2 | <br/>Med2 | <br/>IQR2 | tests<br/>p | <br/>test                         |
| :----------- | :--------------------------- | :-------- | :------ | :------- | :------- | :-------------------------------- | :--------- | :------- | :-------- | :-------- | :------------------------------- | :--------- | :------- | :-------- | :-------- | :---------- | :-------------------------------- |
| Sepal.Length | 50                           | 5         | 0.35    | 5        | 0.4      | 50                                | 5.9        | 0.52     | 5.9       | 0.7       | 50                               | 6.6        | 0.64     | 6.5       | 0.67      | 1.5e-28     | . %\>% oneway.test(var.equal = F) |
| Sepal.Width  | 50                           | 3.4       | 0.38    | 3.4      | 0.48     | 50                                | 2.8        | 0.31     | 2.8       | 0.48      | 50                               | 3          | 0.32     | 3         | 0.38      | 4.5e-17     | . %\>% oneway.test(var.equal = T) |
| Petal.Length | 50                           |           |         | 1.5      | 0.18     | 50                                | 4.3        | 0.47     | 4.3       | 0.6       | 50                               | 5.6        | 0.55     | 5.5       | 0.78      | 4.8e-29     | kruskal.test                      |
| Petal.Width  | 50                           |           |         | 0.2      | 0.1      | 50                                |            |          | 1.3       | 0.3       | 50                               |            |          | 2         | 0.5       | 3.3e-29     | kruskal.test                      |

<br>

### List of statistical test functions

You can provide a named list of statistical functions, but here the
mechanism is a bit different from the *stats* argument.

The list must contain either `.auto` or `.default`.

  - `.auto` needs to be an automatic function, such as `tests_auto`. It
    will be used by default on all variables to select a test
  - `.default` needs to be a single-term formula containing a
    statistical test function that will be used on all variables

You can also provide overrides to use specific tests for specific
variables.  
This is done using list items named as the variable and containing a
single-term formula function.

``` r
iris %>%
  group_by(Petal.Length > 5) %>%
  desctable(tests = list(.auto   = tests_auto,
                         Species = ~chisq.test)) %>%
  pander()
```

| Â            | Petal.Length \> 5: FALSE (n=108)<br/>N | <br/>% | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | Petal.Length \> 5: TRUE (n=42)<br/>N1 | <br/>%1 | <br/>Mean1 | <br/>sd1 | <br/>Med1 | <br/>IQR1 | tests<br/>p | <br/>test   |
| :----------- | :------------------------------------- | :----- | :-------- | :------ | :------- | :------- | :------------------------------------ | :------ | :--------- | :------- | :-------- | :-------- | :---------- | :---------- |
| Sepal.Length | 108                                    |        |           |         | 5.5      | 1        | 42                                    |         |            |          | 6.7       | 0.85      | 1.6e-15     | wilcox.test |
| Sepal.Width  | 108                                    |        | 3.1       | 0.48    | 3        | 0.6      | 42                                    |         |            |          | 3         | 0.4       | 0.69        | wilcox.test |
| Petal.Length | 108                                    |        |           |         | 3.5      | 3        | 42                                    |         |            |          | 5.6       | 0.67      | 2.1e-21     | wilcox.test |
| Petal.Width  | 108                                    |        |           |         | 1        | 1.2      | 42                                    |         | 2.1        | 0.28     | 2.1       | 0.47      | 1.6e-19     | wilcox.test |
| **Species**  | 108                                    |        |           |         |          |          | 42                                    |         |            |          |           |           | 2.7e-24     | chisq.test  |
| setosa       | 50                                     | 46     |           |         |          |          | 0                                     | 0       |            |          |           |           |             |             |
| versicolor   | 49                                     | 45     |           |         |          |          | 1                                     | 2.4     |            |          |           |           |             |             |
| virginica    | 9                                      | 8.3    |           |         |          |          | 41                                    | 98      |            |          |           |           |             |             |

<br>

``` r
mtcars %>%
  dplyr::mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>%
  group_by(am) %>%
  desctable(tests = list(.default = ~wilcox.test,
                         mpg      = ~t.test)) %>%
  pander()
```

| Â    | am: Automatic (n=19)<br/>N | <br/>Med | <br/>IQR | am: Manual (n=13)<br/>N1 | <br/>Med1 | <br/>IQR1 | tests<br/>p | <br/>test   |
| :--- | :------------------------- | :------- | :------- | :----------------------- | :-------- | :-------- | :---------- | :---------- |
| mpg  | 19                         | 17       | 4.2      | 13                       | 23        | 9.4       | 0.0014      | t.test      |
| cyl  | 19                         | 8        | 2        | 13                       | 4         | 2         | 0.0039      | wilcox.test |
| disp | 19                         | 276      | 164      | 13                       | 120       | 81        | 0.00055     | wilcox.test |
| hp   | 19                         | 175      | 76       | 13                       | 109       | 47        | 0.046       | wilcox.test |
| drat | 19                         | 3.1      | 0.63     | 13                       | 4.1       | 0.37      | 0.00014     | wilcox.test |
| wt   | 19                         | 3.5      | 0.41     | 13                       | 2.3       | 0.84      | 4.3e-05     | wilcox.test |
| qsec | 19                         | 18       | 2        | 13                       | 17        | 2.1       | 0.27        | wilcox.test |
| vs   | 19                         | 0        | 1        | 13                       | 1         | 1         | 0.36        | wilcox.test |
| gear | 19                         | 3        | 0        | 13                       | 4         | 1         | 7.6e-06     | wilcox.test |
| carb | 19                         | 3        | 2        | 13                       | 2         | 3         | 0.74        | wilcox.test |

<br>

You might wonder why the formula expression. That is needed to capture
the test name, and to provide it in the resulting table.

As with statistical functions, any statistical test function defined in
R can be used.

The conditions are that the function

  - accepts a formula (`variable ~ grouping_variable`) as a first
    positional argument (as is the case with most tests, like `t.test`),
    and
  - returns an object with a `p.value` element.

Several convenience function are provided: formula versions for
`chisq.test` and `fisher.test` using generic S3 methods (thus the
behavior of standard calls to `chisq.test` and `fisher.test` are not
modified), and `ANOVA`, a partial application of `oneway.test` with
parameter *var.equal* = T.

# Tips and tricks

In the *stats* argument, you can not only feed function names, but even
arbitrary function definitions, functional sequences (a feature provided
with the pipe (`%>%`)), or partial applications (with the **purrr**
package):

``` r
mtcars %>%
  desctable(stats = list("N"              = length,
                         "Sum of squares" = function(x) sum(x^2),
                         "Q1"             = . %>% quantile(prob = .25),
                         "Q3"             = purrr::partial(quantile, probs = .75))) %>%
  pander()
```

| Â    | N  | Sum of squares | Q1  | Q3  |
| :--- | :- | :------------- | :-- | :-- |
| mpg  | 32 | 14042          | 15  | 23  |
| cyl  | 32 | 1324           | 4   | 8   |
| disp | 32 | 2179627        | 121 | 326 |
| hp   | 32 | 834278         | 96  | 180 |
| drat | 32 | 423            | 3.1 | 3.9 |
| wt   | 32 | 361            | 2.6 | 3.6 |
| qsec | 32 | 10293          | 17  | 19  |
| vs   | 32 | 14             | 0   | 1   |
| am   | 32 | 13             | 0   | 1   |
| gear | 32 | 452            | 3   | 4   |
| carb | 32 | 334            | 2   | 4   |

<br>

In the *tests* arguments, you can also provide function definitions,
functional sequences, and partial applications in the formulas:

``` r
iris %>%
  group_by(Species) %>%
  desctable(tests = list(.auto = tests_auto,
                         Sepal.Width = ~function(f) oneway.test(f, var.equal = F),
                         Petal.Length = ~. %>% oneway.test(var.equal = T),
                         Sepal.Length = ~purrr::partial(oneway.test, var.equal = T))) %>%
  pander()
```

| Â            | Species: setosa (n=50)<br/>N | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | Species: versicolor (n=50)<br/>N1 | <br/>Mean1 | <br/>sd1 | <br/>Med1 | <br/>IQR1 | Species: virginica (n=50)<br/>N2 | <br/>Mean2 | <br/>sd2 | <br/>Med2 | <br/>IQR2 | tests<br/>p | <br/>test                                  |
| :----------- | :--------------------------- | :-------- | :------ | :------- | :------- | :-------------------------------- | :--------- | :------- | :-------- | :-------- | :------------------------------- | :--------- | :------- | :-------- | :-------- | :---------- | :----------------------------------------- |
| Sepal.Length | 50                           | 5         | 0.35    | 5        | 0.4      | 50                                | 5.9        | 0.52     | 5.9       | 0.7       | 50                               | 6.6        | 0.64     | 6.5       | 0.67      | 1.7e-31     | purrr::partial(oneway.test, var.equal = T) |
| Sepal.Width  | 50                           | 3.4       | 0.38    | 3.4      | 0.48     | 50                                | 2.8        | 0.31     | 2.8       | 0.48      | 50                               | 3          | 0.32     | 3         | 0.38      | 1.4e-14     | function(f) oneway.test(f, var.equal = F)  |
| Petal.Length | 50                           |           |         | 1.5      | 0.18     | 50                                | 4.3        | 0.47     | 4.3       | 0.6       | 50                               | 5.6        | 0.55     | 5.5       | 0.78      | 2.9e-91     | . %\>% oneway.test(var.equal = T)          |
| Petal.Width  | 50                           |           |         | 0.2      | 0.1      | 50                                |            |          | 1.3       | 0.3       | 50                               |            |          | 2         | 0.5       | 3.3e-29     | kruskal.test                               |

<br>

This allows you to modulate the behavior of `desctable` in every detail,
such as using paired tests, or non *htest* tests.

``` r
# This is a contrived example, which would be better solved with a dedicated function
library(survival)

bladder$surv <- Surv(bladder$stop, bladder$event)

bladder %>%
  group_by(rx) %>%
  desctable(tests = list(.default = ~wilcox.test,
                         surv = ~. %>% survdiff %>% .$chisq %>% pchisq(1, lower.tail = F) %>% list(p.value = .))) %>%
  pander()
```

| Â      | rx: 1 (n=188)<br/>N | <br/>Med | <br/>IQR | rx: 2 (n=152)<br/>N1 | <br/>Med1 | <br/>IQR1 | tests<br/>p | <br/>test                                                                          |
| :----- | :------------------ | :------- | :------- | :------------------- | :-------- | :-------- | :---------- | :--------------------------------------------------------------------------------- |
| id     | 188                 | 24       | 24       | 152                  | 66        | 19        | 1.3e-56     | wilcox.test                                                                        |
| number | 188                 | 1        | 2        | 152                  | 1         | 2         | 0.62        | wilcox.test                                                                        |
| size   | 188                 | 1        | 2        | 152                  | 1         | 2         | 0.32        | wilcox.test                                                                        |
| stop   | 188                 | 23       | 20       | 152                  | 25        | 28        | 0.17        | wilcox.test                                                                        |
| event  | 188                 | 0        | 1        | 152                  | 0         | 1         | 0.02        | wilcox.test                                                                        |
| enum   | 188                 | 2.5      | 1.5      | 152                  | 2.5       | 1.5       | 1           | wilcox.test                                                                        |
| surv   | 188                 |          |          | 152                  |           |           | 0.023       | . %\>% survdiff %\>% .$chisq %\>% pchisq(1, lower.tail = F) %\>% list(p.value = .) |
