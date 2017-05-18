Desctable
================

-   [Introduction](#introduction)
-   [Installation](#installation)
-   [Loading](#loading)
-   [Descriptive tables](#descriptive-tables)
    -   [Simple usage](#simple-usage)
    -   [Output](#output)
    -   [Advanced usage](#advanced-usage)
        -   [Automatic function](#automatic-function)
        -   [Statistical functions](#statistical-functions)
        -   [Conditional formulas](#conditional-formulas)
        -   [Labels](#labels)
-   [Comparative tables](#comparative-tables)
    -   [Simple usage](#simple-usage-1)
    -   [Advanced usage](#advanced-usage-1)
        -   [Automatic function](#automatic-function-1)
        -   [List of statistical test functions](#list-of-statistical-test-functions)
-   [Tips and tricks](#tips-and-tricks)

[![Travis-CI Build Status](https://travis-ci.org/MaximeWack/desctable.svg?branch=badges)](https://travis-ci.org/MaximeWack/desctable) [![Coverage Status](https://img.shields.io/codecov/c/github/MaximeWack/desctable/badges.svg)](https://codecov.io/github/MaximeWack/desctable?branch=badges) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/desctable)](https://cran.r-project.org/package=desctable)

Introduction
============

Desctable is a comprehensive descriptive and comparative tables generator for R.

Every person doing data analysis has to create tables for descriptive summaries of data (a.k.a. Table.1), or comparative tables.

Many packages, such as the aptly named **tableone**, address this issue. However, they often include hard-coded behaviors, have outputs not easily manipulable with standard R tools, or their syntax are out-of-style (e.g. the argument order makes them difficult to use with the pipe (`%>%`)).

Enter **desctable**, a package built with the following objectives in mind:

-   generate descriptive and comparative statistics tables with nesting
-   keep the syntax as simple as possible
-   have good reasonable defaults
-   be entirely customizable, using standard R tools and functions
-   produce the simplest (as a data structure) output possible
-   provide helpers for different outputs
-   integrate with "modern" R usage, and the **tidyverse** set of tools
-   apply functional paradigms

Installation
============

Install from CRAN with

    install.packages("desctable")

or install the development version from github with

    devtools::install_github("maximewack/desctable")

Loading
=======

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

------------------------------------------------------------------------

Descriptive tables
==================

Simple usage
------------

**desctable** uses and exports the pipe (`%>%`) operator (from packages **magrittr** and **dplyr** fame), though it is not mandatory to use it.

The single interface to the package is its eponymous `desctable` function.

When used on a data.frame, it returns a descriptive table:

``` r
iris %>%
  desctable
```

    ##                         N    Mean/%        sd  Med IQR
    ## 1        Sepal.Length 150        NA        NA 5.80 1.3
    ## 2         Sepal.Width 150  3.057333 0.4358663   NA  NA
    ## 3        Petal.Length 150        NA        NA 4.35 3.5
    ## 4         Petal.Width 150        NA        NA 1.30 1.5
    ## 5             Species 150        NA        NA   NA  NA
    ## 6     Species: setosa  50 33.333333        NA   NA  NA
    ## 7 Species: versicolor  50 33.333333        NA   NA  NA
    ## 8  Species: virginica  50 33.333333        NA   NA  NA

``` r
desctable(mtcars)
```

    ##          N      Mean        sd     Med       IQR
    ## 1   mpg 32 20.090625 6.0269481      NA        NA
    ## 2   cyl 32        NA        NA   6.000   4.00000
    ## 3  disp 32        NA        NA 196.300 205.17500
    ## 4    hp 32        NA        NA 123.000  83.50000
    ## 5  drat 32  3.596563 0.5346787      NA        NA
    ## 6    wt 32        NA        NA   3.325   1.02875
    ## 7  qsec 32 17.848750 1.7869432      NA        NA
    ## 8    vs 32        NA        NA   0.000   1.00000
    ## 9    am 32        NA        NA   0.000   1.00000
    ## 10 gear 32        NA        NA   4.000   1.00000
    ## 11 carb 32        NA        NA   2.000   2.00000

As you can see with these two examples, `desctable` describes every variable, with individual levels for factors. It picks statistical functions depending on the type and distribution of the variables in the data, and applies those statistical functions only on the relevant variables.

Output
------

The object produced by `desctable` is in fact a list of data.frames, with a "desctable" class.
Methods for reduction to a simple dataframe (`as.data.frame`, automatically used for printing), conversion to markdown (`pander`), and interactive html output with **DT** (`datatable`) are provided:

``` r
iris %>%
  desctable %>%
  pander
```

<table style="width:90%;">
<colgroup>
<col width="48%" />
<col width="5%" />
<col width="12%" />
<col width="6%" />
<col width="8%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">N</th>
<th align="left">Mean/%</th>
<th align="left">sd</th>
<th align="left">Med</th>
<th align="left">IQR</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">5.8</td>
<td align="left">1.3</td>
</tr>
<tr class="even">
<td align="left">Sepal.Width</td>
<td align="left">150</td>
<td align="left">3.1</td>
<td align="left">0.44</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">Petal.Length</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">4.3</td>
<td align="left">3.5</td>
</tr>
<tr class="even">
<td align="left">Petal.Width</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.3</td>
<td align="left">1.5</td>
</tr>
<tr class="odd">
<td align="left"><strong>Species</strong></td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    setosa</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">    versicolor</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    virginica</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
</tbody>
</table>

<br> You need to load these two packages first (and prior to **desctable** for **DT**) if you want to use them.

Calls to `pander` and `datatable` with "regular" dataframes will not be affected by the defaults used in the package, and you can modify these defaults for **desctable** objects.

The `datatable` wrapper function for desctable objects comes with some default options and formatting such as freezing the row names and table header, export buttons, and rounding of values. Both `pander` and `datatable` wrapper take a *digits* argument to set the number of decimals to show. (`pander` uses the *digits*, *justify* and *missing* arguments of `pandoc.table`, whereas `datatable` calls `prettyNum` with the `digits` parameter, and removes `NA` values. You can set `digits = NULL` if you want the full table and format it yourself)

Advanced usage
--------------

`desctable` chooses statistical functions for you using this algorithm:

-   always show N
-   if there are factors, show %
-   if there are normally distributed variables, show Mean and SD
-   if there are non-normally distributed variables, show Median and IQR

For each variable in the table, compute the relevant statistical functions in that list (non-applicable functions will safely return `NA`).

How does it work, and how can you adapt this behavior to your needs?

`desctable` takes an optional *stats* argument. This argument can either be:

-   an automatic function to select appropriate statistical functions
-   or a named list of
    -   statistical functions
    -   formulas describing conditions to use a statistical function.

### Automatic function

This is the default, using the `stats_auto` function provided in the package.

Several other "automatic statistical functions" are defined in this package: `stats_auto`, `stats_default`, `stats_normal`, `stats_nonnormal`.

You can also provide your own automatic function, which needs to

-   accept a dataframe as its argument (whether to use this dataframe or not in the function is your choice), and
-   return a named list of statistical functions to use, as defined in the subsequent paragraphs.

``` r
# Strictly equivalent to iris %>% desctable %>% pander
iris %>%
  desctable(stats = stats_auto) %>%
  pander
```

<table style="width:90%;">
<colgroup>
<col width="48%" />
<col width="5%" />
<col width="12%" />
<col width="6%" />
<col width="8%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">N</th>
<th align="left">Mean/%</th>
<th align="left">sd</th>
<th align="left">Med</th>
<th align="left">IQR</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">5.8</td>
<td align="left">1.3</td>
</tr>
<tr class="even">
<td align="left">Sepal.Width</td>
<td align="left">150</td>
<td align="left">3.1</td>
<td align="left">0.44</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">Petal.Length</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">4.3</td>
<td align="left">3.5</td>
</tr>
<tr class="even">
<td align="left">Petal.Width</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.3</td>
<td align="left">1.5</td>
</tr>
<tr class="odd">
<td align="left"><strong>Species</strong></td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    setosa</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">    versicolor</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    virginica</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
</tbody>
</table>

### Statistical functions

Statistical functions can be any function defined in R that you want to use, such as `length` or `mean`.

The only condition is that they return a single numerical value. One exception is when they return a vector of length `1 + nlevels(x)` when applied to factors, as is needed for the `percent` function.

As mentioned above, they need to be used inside a named list, such as

``` r
mtcars %>%
  desctable(stats = list("N" = length, "Mean" = mean, "SD" = sd)) %>%
  pander
```

<table style="width:33%;">
<colgroup>
<col width="12%" />
<col width="5%" />
<col width="9%" />
<col width="5%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">N</th>
<th align="left">Mean</th>
<th align="left">SD</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">mpg</td>
<td align="left">32</td>
<td align="left">20</td>
<td align="left">6</td>
</tr>
<tr class="even">
<td align="left">cyl</td>
<td align="left">32</td>
<td align="left">6.2</td>
<td align="left">1.8</td>
</tr>
<tr class="odd">
<td align="left">disp</td>
<td align="left">32</td>
<td align="left">231</td>
<td align="left">124</td>
</tr>
<tr class="even">
<td align="left">hp</td>
<td align="left">32</td>
<td align="left">147</td>
<td align="left">69</td>
</tr>
<tr class="odd">
<td align="left">drat</td>
<td align="left">32</td>
<td align="left">3.6</td>
<td align="left">0.53</td>
</tr>
<tr class="even">
<td align="left">wt</td>
<td align="left">32</td>
<td align="left">3.2</td>
<td align="left">0.98</td>
</tr>
<tr class="odd">
<td align="left">qsec</td>
<td align="left">32</td>
<td align="left">18</td>
<td align="left">1.8</td>
</tr>
<tr class="even">
<td align="left">vs</td>
<td align="left">32</td>
<td align="left">0.44</td>
<td align="left">0.5</td>
</tr>
<tr class="odd">
<td align="left">am</td>
<td align="left">32</td>
<td align="left">0.41</td>
<td align="left">0.5</td>
</tr>
<tr class="even">
<td align="left">gear</td>
<td align="left">32</td>
<td align="left">3.7</td>
<td align="left">0.74</td>
</tr>
<tr class="odd">
<td align="left">carb</td>
<td align="left">32</td>
<td align="left">2.8</td>
<td align="left">1.6</td>
</tr>
</tbody>
</table>

<br>

The names will be used as column headers in the resulting table, and the functions will be applied safely on the variables (errors return `NA`, and for factors the function will be used on individual levels).

Several convenience functions are included in this package. For statistical function we have: `percent`, which prints percentages of levels in a factor, and `IQR` which re-implements `stats::IQR` but works better with `NA` values.

Be aware that **all functions will be used on variables stripped of their `NA` values!**
This is necessary for most statistical functions to be useful, and makes **N** (`length`) show only the number of observations in the dataset for each variable.

### Conditional formulas

The general form of these formulas is

``` r
predicate_function ~ stat_function_if_TRUE | stat_function_if_FALSE
```

A predicate function is any function returning either `TRUE` or `FALSE` when applied on a vector, such as `is.factor`, `is.numeric`, and `is.logical`.
**desctable** provides the `is.normal` function to test for normality (it is equivalent to `length(na.omit(x)) > 30 & shapiro.test(x)$p.value > .1`).

The *FALSE* option can be omitted and `NA` will be produced if the condition in the predicate is not met.

These statements can be nested using parentheses.
For example:

`is.factor ~ percent | (is.normal ~ mean)`

will either use `percent` if the variable is a factor, or `mean` if and only if the variable is normally distributed.

You can mix "bare" statistical functions and formulas in the list defining the statistics you want to use in your table.

``` r
iris %>%
  desctable(stats = list("N"      = length,
                         "%/Mean" = is.factor ~ percent | (is.normal ~ mean),
                         "Median" = is.normal ~ NA | median)) %>%
  pander
```

<table style="width:79%;">
<colgroup>
<col width="48%" />
<col width="5%" />
<col width="12%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">N</th>
<th align="left">%/Mean</th>
<th align="left">Median</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left">5.8</td>
</tr>
<tr class="even">
<td align="left">Sepal.Width</td>
<td align="left">150</td>
<td align="left">3.1</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">Petal.Length</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left">4.3</td>
</tr>
<tr class="even">
<td align="left">Petal.Width</td>
<td align="left">150</td>
<td align="left"></td>
<td align="left">1.3</td>
</tr>
<tr class="odd">
<td align="left"><strong>Species</strong></td>
<td align="left">150</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    setosa</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">    versicolor</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    virginica</td>
<td align="left">50</td>
<td align="left">33</td>
<td align="left"></td>
</tr>
</tbody>
</table>

<br>

For reference, here is the body of the `stats_auto` function in the package:

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
    ##             sd, Med = is.normal ~ NA | median, IQR = is.normal ~ 
    ##             NA | IQR)
    ##     else if (!fact & normal & !nonnormal) 
    ##         list(N = length, Mean = mean, sd = stats::sd)
    ##     else if (!fact & !normal & nonnormal) 
    ##         list(N = length, Med = stats::median, IQR = IQR)
    ##     else stats_default(data)
    ## }
    ## <environment: namespace:desctable>

### Labels

It is often the case that variable names are not "pretty" enough to be used as-is in a table.
Although you could still edit the variable labels in the table afterwards using subsetting or string replacement functions, it is possible to mention a **labels** argument.

The **labels** argument is a named character vector associating variable names and labels.
You don't need to provide labels for all the variables, and extra labels will be silently discarded. This allows you to define a "global" labels vector and use it for every table even after variable selections.

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
  pander
```

<table style="width:89%;">
<colgroup>
<col width="47%" />
<col width="5%" />
<col width="12%" />
<col width="6%" />
<col width="8%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">N</th>
<th align="left">Mean/%</th>
<th align="left">sd</th>
<th align="left">Med</th>
<th align="left">IQR</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Miles/(US) gallon</td>
<td align="left">32</td>
<td align="left">20</td>
<td align="left">6</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Number of cylinders</td>
<td align="left">32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">6</td>
<td align="left">4</td>
</tr>
<tr class="odd">
<td align="left">Displacement (cu.in.)</td>
<td align="left">32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">196</td>
<td align="left">205</td>
</tr>
<tr class="even">
<td align="left">Gross horsepower</td>
<td align="left">32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">123</td>
<td align="left">84</td>
</tr>
<tr class="odd">
<td align="left">Rear axle ratio</td>
<td align="left">32</td>
<td align="left">3.6</td>
<td align="left">0.53</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Weight (1000 lbs)</td>
<td align="left">32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">3.3</td>
<td align="left">1</td>
</tr>
<tr class="odd">
<td align="left">¼ mile time</td>
<td align="left">32</td>
<td align="left">18</td>
<td align="left">1.8</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">V/S</td>
<td align="left">32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">0</td>
<td align="left">1</td>
</tr>
<tr class="odd">
<td align="left"><strong>Transmission</strong></td>
<td align="left">32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    Automatic</td>
<td align="left">19</td>
<td align="left">59</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">    Manual</td>
<td align="left">13</td>
<td align="left">41</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Number of forward gears</td>
<td align="left">32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">4</td>
<td align="left">1</td>
</tr>
<tr class="odd">
<td align="left">Number of carburetors</td>
<td align="left">32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">2</td>
<td align="left">2</td>
</tr>
</tbody>
</table>

<br>

------------------------------------------------------------------------

Comparative tables
==================

Simple usage
------------

Creating a comparative table (between groups defined by a factor) using `desctable` is as easy as creating a descriptive table.

It uses the well known `group_by` function from **dplyr**:

``` r
iris %>%
  group_by(Species) %>%
  desctable -> iris_by_Species

iris_by_Species
```

    ##                Species: setosa (n=50) / N Species: setosa (n=50) / Mean
    ## 1 Sepal.Length                         50                         5.006
    ## 2  Sepal.Width                         50                         3.428
    ## 3 Petal.Length                         50                            NA
    ## 4  Petal.Width                         50                            NA
    ##   Species: setosa (n=50) / sd Species: setosa (n=50) / Med
    ## 1                   0.3524897                           NA
    ## 2                   0.3790644                           NA
    ## 3                          NA                          1.5
    ## 4                          NA                          0.2
    ##   Species: setosa (n=50) / IQR Species: versicolor (n=50) / N
    ## 1                           NA                             50
    ## 2                           NA                             50
    ## 3                        0.175                             50
    ## 4                        0.100                             50
    ##   Species: versicolor (n=50) / Mean Species: versicolor (n=50) / sd
    ## 1                             5.936                       0.5161711
    ## 2                             2.770                       0.3137983
    ## 3                             4.260                       0.4699110
    ## 4                                NA                              NA
    ##   Species: versicolor (n=50) / Med Species: versicolor (n=50) / IQR
    ## 1                               NA                               NA
    ## 2                               NA                               NA
    ## 3                               NA                               NA
    ## 4                              1.3                              0.3
    ##   Species: virginica (n=50) / N Species: virginica (n=50) / Mean
    ## 1                            50                            6.588
    ## 2                            50                            2.974
    ## 3                            50                            5.552
    ## 4                            50                               NA
    ##   Species: virginica (n=50) / sd Species: virginica (n=50) / Med
    ## 1                      0.6358796                              NA
    ## 2                      0.3224966                              NA
    ## 3                      0.5518947                              NA
    ## 4                             NA                               2
    ##   Species: virginica (n=50) / IQR    tests / p
    ## 1                              NA 1.505059e-28
    ## 2                              NA 4.492017e-17
    ## 3                              NA 4.803974e-29
    ## 4                             0.5 3.261796e-29
    ##                       tests / test
    ## 1 . %>% oneway.test(var.equal = F)
    ## 2 . %>% oneway.test(var.equal = T)
    ## 3                     kruskal.test
    ## 4                     kruskal.test

The result is a table containing a descriptive subtable for each level of the grouping factor (the statistical functions rules are applied to each subtable independently), with the statistical tests performed, and their p values.

When displayed as a flat dataframe, the grouping header appears in each variable.

You can also see the grouping headers by inspecting the resulting object, which is a deep list of dataframes, each dataframe named after the grouping factor and its levels (with sample size for each).

``` r
str(iris_by_Species)
```

    ## List of 5
    ##  $ Variables                 :'data.frame':  4 obs. of  1 variable:
    ##   ..$ Variables: chr [1:4] "Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width"
    ##  $ Species: setosa (n=50)    :'data.frame':  4 obs. of  5 variables:
    ##   ..$ N   : num [1:4] 50 50 50 50
    ##   ..$ Mean: num [1:4] 5.01 3.43 NA NA
    ##   ..$ sd  : num [1:4] 0.352 0.379 NA NA
    ##   ..$ Med : num [1:4] NA NA 1.5 0.2
    ##   ..$ IQR : num [1:4] NA NA 0.175 0.1
    ##  $ Species: versicolor (n=50):'data.frame':  4 obs. of  5 variables:
    ##   ..$ N   : num [1:4] 50 50 50 50
    ##   ..$ Mean: num [1:4] 5.94 2.77 4.26 NA
    ##   ..$ sd  : num [1:4] 0.516 0.314 0.47 NA
    ##   ..$ Med : num [1:4] NA NA NA 1.3
    ##   ..$ IQR : num [1:4] NA NA NA 0.3
    ##  $ Species: virginica (n=50) :'data.frame':  4 obs. of  5 variables:
    ##   ..$ N   : num [1:4] 50 50 50 50
    ##   ..$ Mean: num [1:4] 6.59 2.97 5.55 NA
    ##   ..$ sd  : num [1:4] 0.636 0.322 0.552 NA
    ##   ..$ Med : num [1:4] NA NA NA 2
    ##   ..$ IQR : num [1:4] NA NA NA 0.5
    ##  $ tests                     :'data.frame':  4 obs. of  2 variables:
    ##   ..$ p   : num [1:4] 1.51e-28 4.49e-17 4.80e-29 3.26e-29
    ##   ..$ test: chr [1:4] ". %>% oneway.test(var.equal = F)" ". %>% oneway.test(var.equal = T)" "kruskal.test" "kruskal.test"
    ##  - attr(*, "class")= chr "desctable"

You can specify groups based on any variable, not only factors:

``` r
# With pander output
mtcars %>%
  group_by(cyl) %>%
  desctable %>%
  pander
```

<table>
<colgroup>
<col width="5%" />
<col width="13%" />
<col width="6%" />
<col width="6%" />
<col width="12%" />
<col width="6%" />
<col width="6%" />
<col width="13%" />
<col width="6%" />
<col width="6%" />
<col width="8%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">cyl: 4 (n=11)<br/>N</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">cyl: 6 (n=7)<br/>N</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">cyl: 8 (n=14)<br/>N</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">tests<br/>p</th>
<th align="left"><br/>test</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">mpg</td>
<td align="left">11</td>
<td align="left">26</td>
<td align="left">7.6</td>
<td align="left">7</td>
<td align="left">20</td>
<td align="left">2.4</td>
<td align="left">14</td>
<td align="left">15</td>
<td align="left">1.8</td>
<td align="left">2.6e-06</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="even">
<td align="left">disp</td>
<td align="left">11</td>
<td align="left">108</td>
<td align="left">42</td>
<td align="left">7</td>
<td align="left">168</td>
<td align="left">36</td>
<td align="left">14</td>
<td align="left">350</td>
<td align="left">88</td>
<td align="left">1.6e-06</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="odd">
<td align="left">hp</td>
<td align="left">11</td>
<td align="left">91</td>
<td align="left">30</td>
<td align="left">7</td>
<td align="left">110</td>
<td align="left">13</td>
<td align="left">14</td>
<td align="left">192</td>
<td align="left">65</td>
<td align="left">3.3e-06</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="even">
<td align="left">drat</td>
<td align="left">11</td>
<td align="left">4.1</td>
<td align="left">0.35</td>
<td align="left">7</td>
<td align="left">3.9</td>
<td align="left">0.56</td>
<td align="left">14</td>
<td align="left">3.1</td>
<td align="left">0.15</td>
<td align="left">0.00075</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="odd">
<td align="left">wt</td>
<td align="left">11</td>
<td align="left">2.2</td>
<td align="left">0.74</td>
<td align="left">7</td>
<td align="left">3.2</td>
<td align="left">0.62</td>
<td align="left">14</td>
<td align="left">3.8</td>
<td align="left">0.48</td>
<td align="left">1.1e-05</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="even">
<td align="left">qsec</td>
<td align="left">11</td>
<td align="left">19</td>
<td align="left">1.4</td>
<td align="left">7</td>
<td align="left">18</td>
<td align="left">2.4</td>
<td align="left">14</td>
<td align="left">17</td>
<td align="left">1.5</td>
<td align="left">0.0062</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="odd">
<td align="left">vs</td>
<td align="left">11</td>
<td align="left">1</td>
<td align="left">0</td>
<td align="left">7</td>
<td align="left">1</td>
<td align="left">1</td>
<td align="left">14</td>
<td align="left">0</td>
<td align="left">0</td>
<td align="left">3.2e-05</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="even">
<td align="left">am</td>
<td align="left">11</td>
<td align="left">1</td>
<td align="left">0.5</td>
<td align="left">7</td>
<td align="left">0</td>
<td align="left">1</td>
<td align="left">14</td>
<td align="left">0</td>
<td align="left">0</td>
<td align="left">0.014</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="odd">
<td align="left">gear</td>
<td align="left">11</td>
<td align="left">4</td>
<td align="left">0</td>
<td align="left">7</td>
<td align="left">4</td>
<td align="left">0.5</td>
<td align="left">14</td>
<td align="left">3</td>
<td align="left">0</td>
<td align="left">0.0062</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="even">
<td align="left">carb</td>
<td align="left">11</td>
<td align="left">2</td>
<td align="left">1</td>
<td align="left">7</td>
<td align="left">4</td>
<td align="left">1.5</td>
<td align="left">14</td>
<td align="left">3.5</td>
<td align="left">1.8</td>
<td align="left">0.0017</td>
<td align="left">kruskal.test</td>
</tr>
</tbody>
</table>

Also with conditions:

``` r
iris %>%
  group_by(Petal.Length > 5) %>%
  desctable %>%
  pander
```

<table style="width:100%;">
<colgroup>
<col width="17%" />
<col width="12%" />
<col width="6%" />
<col width="4%" />
<col width="5%" />
<col width="5%" />
<col width="12%" />
<col width="6%" />
<col width="4%" />
<col width="5%" />
<col width="5%" />
<col width="6%" />
<col width="5%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">Petal.Length &gt; 5: FALSE (n=108)<br/>N</th>
<th align="left"><br/>Mean/%</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">Petal.Length &gt; 5: TRUE (n=42)<br/>N</th>
<th align="left"><br/>Mean/%</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">tests<br/>p</th>
<th align="left"><br/>test</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="left">108</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">5.5</td>
<td align="left">1</td>
<td align="left">42</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">6.7</td>
<td align="left">0.85</td>
<td align="left">1.6e-15</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">Sepal.Width</td>
<td align="left">108</td>
<td align="left">3.1</td>
<td align="left">0.48</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">42</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">3</td>
<td align="left">0.4</td>
<td align="left">0.69</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">Petal.Length</td>
<td align="left">108</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">3.5</td>
<td align="left">3</td>
<td align="left">42</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">5.6</td>
<td align="left">0.67</td>
<td align="left">2.1e-21</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">Petal.Width</td>
<td align="left">108</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1</td>
<td align="left">1.2</td>
<td align="left">42</td>
<td align="left">2.1</td>
<td align="left">0.28</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.6e-19</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left"><strong>Species</strong></td>
<td align="left">108</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">42</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">2.5e-26</td>
<td align="left">fisher.test</td>
</tr>
<tr class="even">
<td align="left">    setosa</td>
<td align="left">50</td>
<td align="left">46</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">0</td>
<td align="left">0</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">    versicolor</td>
<td align="left">49</td>
<td align="left">45</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1</td>
<td align="left">2.4</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    virginica</td>
<td align="left">9</td>
<td align="left">8.3</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">41</td>
<td align="left">98</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
</tbody>
</table>

<br>

And even on multiple nested groups:

``` r
mtcars %>%
  dplyr::mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>%
  group_by(vs, am, cyl) %>%
  desctable %>%
  pander
```

<table style="width:100%;">
<colgroup>
<col width="1%" />
<col width="4%" />
<col width="2%" />
<col width="2%" />
<col width="3%" />
<col width="3%" />
<col width="4%" />
<col width="2%" />
<col width="2%" />
<col width="4%" />
<col width="2%" />
<col width="2%" />
<col width="4%" />
<col width="2%" />
<col width="2%" />
<col width="3%" />
<col width="3%" />
<col width="4%" />
<col width="2%" />
<col width="2%" />
<col width="4%" />
<col width="2%" />
<col width="2%" />
<col width="3%" />
<col width="3%" />
<col width="4%" />
<col width="2%" />
<col width="2%" />
<col width="3%" />
<col width="2%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">vs: 0 (n=18)<br/>am: Automatic (n=12)<br/>cyl: 8 (n=12)<br/>N</th>
<th align="left"><br/><br/><br/>Med</th>
<th align="left"><br/><br/><br/>IQR</th>
<th align="left"><br/><br/>tests<br/>p</th>
<th align="left"><br/><br/><br/>test</th>
<th align="left"><br/>am: Manual (n=6)<br/>cyl: 4 (n=1)<br/>N</th>
<th align="left"><br/><br/><br/>Med</th>
<th align="left"><br/><br/><br/>IQR</th>
<th align="left"><br/><br/>cyl: 6 (n=3)<br/>N</th>
<th align="left"><br/><br/><br/>Med</th>
<th align="left"><br/><br/><br/>IQR</th>
<th align="left"><br/><br/>cyl: 8 (n=2)<br/>N</th>
<th align="left"><br/><br/><br/>Med</th>
<th align="left"><br/><br/><br/>IQR</th>
<th align="left"><br/><br/>tests<br/>p</th>
<th align="left"><br/><br/><br/>test</th>
<th align="left">vs: 1 (n=14)<br/>am: Automatic (n=7)<br/>cyl: 4 (n=3)<br/>N</th>
<th align="left"><br/><br/><br/>Med</th>
<th align="left"><br/><br/><br/>IQR</th>
<th align="left"><br/><br/>cyl: 6 (n=4)<br/>N</th>
<th align="left"><br/><br/><br/>Med</th>
<th align="left"><br/><br/><br/>IQR</th>
<th align="left"><br/><br/>tests<br/>p</th>
<th align="left"><br/><br/><br/>test</th>
<th align="left"><br/>am: Manual (n=7)<br/>cyl: 4 (n=7)<br/>N</th>
<th align="left"><br/><br/><br/>Med</th>
<th align="left"><br/><br/><br/>IQR</th>
<th align="left"><br/><br/>tests<br/>p</th>
<th align="left"><br/><br/><br/>test</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">mpg</td>
<td align="left">12</td>
<td align="left">15</td>
<td align="left">2.6</td>
<td align="left"></td>
<td align="left">no.test</td>
<td align="left">1</td>
<td align="left">26</td>
<td align="left">0</td>
<td align="left">3</td>
<td align="left">21</td>
<td align="left">0.65</td>
<td align="left">2</td>
<td align="left">15</td>
<td align="left">0.4</td>
<td align="left">0.11</td>
<td align="left">kruskal.test</td>
<td align="left">3</td>
<td align="left">23</td>
<td align="left">1.5</td>
<td align="left">4</td>
<td align="left">19</td>
<td align="left">1.7</td>
<td align="left">0.057</td>
<td align="left">wilcox.test</td>
<td align="left">7</td>
<td align="left">30</td>
<td align="left">6.3</td>
<td align="left"></td>
<td align="left">no.test</td>
</tr>
<tr class="even">
<td align="left">disp</td>
<td align="left">12</td>
<td align="left">355</td>
<td align="left">113</td>
<td align="left"></td>
<td align="left">no.test</td>
<td align="left">1</td>
<td align="left">120</td>
<td align="left">0</td>
<td align="left">3</td>
<td align="left">160</td>
<td align="left">7.5</td>
<td align="left">2</td>
<td align="left">326</td>
<td align="left">25</td>
<td align="left">0.11</td>
<td align="left">kruskal.test</td>
<td align="left">3</td>
<td align="left">141</td>
<td align="left">13</td>
<td align="left">4</td>
<td align="left">196</td>
<td align="left">66</td>
<td align="left">0.05</td>
<td align="left">wilcox.test</td>
<td align="left">7</td>
<td align="left">79</td>
<td align="left">24</td>
<td align="left"></td>
<td align="left">no.test</td>
</tr>
<tr class="odd">
<td align="left">hp</td>
<td align="left">12</td>
<td align="left">180</td>
<td align="left">44</td>
<td align="left"></td>
<td align="left">no.test</td>
<td align="left">1</td>
<td align="left">91</td>
<td align="left">0</td>
<td align="left">3</td>
<td align="left">110</td>
<td align="left">32</td>
<td align="left">2</td>
<td align="left">300</td>
<td align="left">36</td>
<td align="left">0.11</td>
<td align="left">kruskal.test</td>
<td align="left">3</td>
<td align="left">95</td>
<td align="left">18</td>
<td align="left">4</td>
<td align="left">116</td>
<td align="left">14</td>
<td align="left">0.05</td>
<td align="left">wilcox.test</td>
<td align="left">7</td>
<td align="left">66</td>
<td align="left">36</td>
<td align="left"></td>
<td align="left">no.test</td>
</tr>
<tr class="even">
<td align="left">drat</td>
<td align="left">12</td>
<td align="left">3.1</td>
<td align="left">0.11</td>
<td align="left"></td>
<td align="left">no.test</td>
<td align="left">1</td>
<td align="left">4.4</td>
<td align="left">0</td>
<td align="left">3</td>
<td align="left">3.9</td>
<td align="left">0.14</td>
<td align="left">2</td>
<td align="left">3.9</td>
<td align="left">0.34</td>
<td align="left">0.33</td>
<td align="left">kruskal.test</td>
<td align="left">3</td>
<td align="left">3.7</td>
<td align="left">0.11</td>
<td align="left">4</td>
<td align="left">3.5</td>
<td align="left">0.92</td>
<td align="left">0.85</td>
<td align="left">wilcox.test</td>
<td align="left">7</td>
<td align="left">4.1</td>
<td align="left">0.2</td>
<td align="left"></td>
<td align="left">no.test</td>
</tr>
<tr class="odd">
<td align="left">wt</td>
<td align="left">12</td>
<td align="left">3.8</td>
<td align="left">0.81</td>
<td align="left"></td>
<td align="left">no.test</td>
<td align="left">1</td>
<td align="left">2.1</td>
<td align="left">0</td>
<td align="left">3</td>
<td align="left">2.8</td>
<td align="left">0.13</td>
<td align="left">2</td>
<td align="left">3.4</td>
<td align="left">0.2</td>
<td align="left">0.12</td>
<td align="left">kruskal.test</td>
<td align="left">3</td>
<td align="left">3.1</td>
<td align="left">0.36</td>
<td align="left">4</td>
<td align="left">3.4</td>
<td align="left">0.061</td>
<td align="left">0.05</td>
<td align="left">wilcox.test</td>
<td align="left">7</td>
<td align="left">1.9</td>
<td align="left">0.53</td>
<td align="left"></td>
<td align="left">no.test</td>
</tr>
<tr class="even">
<td align="left">qsec</td>
<td align="left">12</td>
<td align="left">17</td>
<td align="left">0.67</td>
<td align="left"></td>
<td align="left">no.test</td>
<td align="left">1</td>
<td align="left">17</td>
<td align="left">0</td>
<td align="left">3</td>
<td align="left">16</td>
<td align="left">0.76</td>
<td align="left">2</td>
<td align="left">15</td>
<td align="left">0.05</td>
<td align="left">0.17</td>
<td align="left">kruskal.test</td>
<td align="left">3</td>
<td align="left">20</td>
<td align="left">1.4</td>
<td align="left">4</td>
<td align="left">19</td>
<td align="left">0.89</td>
<td align="left">0.23</td>
<td align="left">wilcox.test</td>
<td align="left">7</td>
<td align="left">19</td>
<td align="left">0.62</td>
<td align="left"></td>
<td align="left">no.test</td>
</tr>
<tr class="odd">
<td align="left">gear</td>
<td align="left">12</td>
<td align="left">3</td>
<td align="left">0</td>
<td align="left"></td>
<td align="left">no.test</td>
<td align="left">1</td>
<td align="left">5</td>
<td align="left">0</td>
<td align="left">3</td>
<td align="left">4</td>
<td align="left">0.5</td>
<td align="left">2</td>
<td align="left">5</td>
<td align="left">0</td>
<td align="left">0.29</td>
<td align="left">kruskal.test</td>
<td align="left">3</td>
<td align="left">4</td>
<td align="left">0.5</td>
<td align="left">4</td>
<td align="left">3.5</td>
<td align="left">1</td>
<td align="left">0.84</td>
<td align="left">wilcox.test</td>
<td align="left">7</td>
<td align="left">4</td>
<td align="left">0</td>
<td align="left"></td>
<td align="left">no.test</td>
</tr>
<tr class="even">
<td align="left">carb</td>
<td align="left">12</td>
<td align="left">3</td>
<td align="left">2</td>
<td align="left"></td>
<td align="left">no.test</td>
<td align="left">1</td>
<td align="left">2</td>
<td align="left">0</td>
<td align="left">3</td>
<td align="left">4</td>
<td align="left">1</td>
<td align="left">2</td>
<td align="left">6</td>
<td align="left">2</td>
<td align="left">0.26</td>
<td align="left">kruskal.test</td>
<td align="left">3</td>
<td align="left">2</td>
<td align="left">0.5</td>
<td align="left">4</td>
<td align="left">2.5</td>
<td align="left">3</td>
<td align="left">0.85</td>
<td align="left">wilcox.test</td>
<td align="left">7</td>
<td align="left">1</td>
<td align="left">1</td>
<td align="left"></td>
<td align="left">no.test</td>
</tr>
</tbody>
</table>

<br>

In the case of nested groups (a.k.a. sub-group analysis), statistical tests are performed only between the groups of the deepest grouping level.

Statistical tests are automatically selected depending on the data and the grouping factor.

Advanced usage
--------------

`desctable` choses the statistical tests using the following algorithm:

-   if the variable is a factor, use `fisher.test`
-   if the grouping factor has only one level, use the provided `no.test` (which does nothing)
-   if the grouping factor has two levels
    -   and the variable presents homoskedasticity (p value for `var.test` &gt; .1) and normality of distribution in both groups, use `t.test(var.equal = T)`
    -   and the variable does not present homoskedasticity (p value for `var.test` &lt; .1) but normality of distribution in both groups, use `t.test(var.equal = F)`
    -   else use `wilcox.test`
-   if the grouping factor has more than two levels
    -   and the variable presents homoskedasticity (p value for `bartlett.test` &gt; .1) and normality of distribution in all groups, use `oneway.test(var.equal = T)`
    -   and the variable does not present homoskedasticity (p value for `bartlett.test` &lt; .1) but normality of distribution in all groups, use `oneway.test(var.equal = F)`
    -   else use `kruskal.test`

But what if you want to pick a specific test for a specific variable, or change all the tests altogether?

`desctable` takes an optional *tests* argument. This argument can either be

-   an automatic function to select appropriate statistical test functions
-   or a named list of statistical test functions

### Automatic function

This is the default, using the `tests_auto` function provided in the package.

You can also provide your own automatic function, which needs to

-   accept a variable and a grouping factor as its arguments, and
-   return a single-term formula containing a statistical test function.

This function will be used on every variable and every grouping factor to determine the appropriate test.

``` r
# Strictly equivalent to iris %>% group_by(Species) %>% desctable %>% pander
iris %>%
  group_by(Species) %>%
  desctable(tests = tests_auto) %>%
  pander
```

<table>
<colgroup>
<col width="4%" />
<col width="11%" />
<col width="4%" />
<col width="3%" />
<col width="4%" />
<col width="4%" />
<col width="8%" />
<col width="4%" />
<col width="3%" />
<col width="4%" />
<col width="4%" />
<col width="8%" />
<col width="4%" />
<col width="3%" />
<col width="4%" />
<col width="4%" />
<col width="5%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">Species: setosa (n=50)<br/>N</th>
<th align="left"><br/>Mean</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">Species: versicolor (n=50)<br/>N</th>
<th align="left"><br/>Mean</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">Species: virginica (n=50)<br/>N</th>
<th align="left"><br/>Mean</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">tests<br/>p</th>
<th align="left"><br/>test</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="left">50</td>
<td align="left">5</td>
<td align="left">0.35</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">5.9</td>
<td align="left">0.52</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">6.6</td>
<td align="left">0.64</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.5e-28</td>
<td align="left">. %&gt;% oneway.test(var.equal = F)</td>
</tr>
<tr class="even">
<td align="left">Sepal.Width</td>
<td align="left">50</td>
<td align="left">3.4</td>
<td align="left">0.38</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">2.8</td>
<td align="left">0.31</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">3</td>
<td align="left">0.32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">4.5e-17</td>
<td align="left">. %&gt;% oneway.test(var.equal = T)</td>
</tr>
<tr class="odd">
<td align="left">Petal.Length</td>
<td align="left">50</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.5</td>
<td align="left">0.18</td>
<td align="left">50</td>
<td align="left">4.3</td>
<td align="left">0.47</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">5.6</td>
<td align="left">0.55</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">4.8e-29</td>
<td align="left">kruskal.test</td>
</tr>
<tr class="even">
<td align="left">Petal.Width</td>
<td align="left">50</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">0.2</td>
<td align="left">0.1</td>
<td align="left">50</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.3</td>
<td align="left">0.3</td>
<td align="left">50</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">2</td>
<td align="left">0.5</td>
<td align="left">3.3e-29</td>
<td align="left">kruskal.test</td>
</tr>
</tbody>
</table>

<br>

### List of statistical test functions

You can provide a named list of statistical functions, but here the mechanism is a bit different from the *stats* argument.

The list must contain either `.auto` or `.default`.

-   `.auto` needs to be an automatic function, such as `tests_auto`. It will be used by default on all variables to select a test
-   `.default` needs to be a single-term formula containing a statistical test function that will be used on all variables

You can also provide overrides to use specific tests for specific variables.
This is done using list items named as the variable and containing a single-term formula function.

``` r
iris %>%
  group_by(Petal.Length > 5) %>%
  desctable(tests = list(.auto   = tests_auto,
                         Species = ~chisq.test)) %>%
  pander
```

<table style="width:100%;">
<colgroup>
<col width="17%" />
<col width="12%" />
<col width="6%" />
<col width="4%" />
<col width="5%" />
<col width="5%" />
<col width="12%" />
<col width="6%" />
<col width="4%" />
<col width="5%" />
<col width="5%" />
<col width="6%" />
<col width="5%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">Petal.Length &gt; 5: FALSE (n=108)<br/>N</th>
<th align="left"><br/>Mean/%</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">Petal.Length &gt; 5: TRUE (n=42)<br/>N</th>
<th align="left"><br/>Mean/%</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">tests<br/>p</th>
<th align="left"><br/>test</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="left">108</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">5.5</td>
<td align="left">1</td>
<td align="left">42</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">6.7</td>
<td align="left">0.85</td>
<td align="left">1.6e-15</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">Sepal.Width</td>
<td align="left">108</td>
<td align="left">3.1</td>
<td align="left">0.48</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">42</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">3</td>
<td align="left">0.4</td>
<td align="left">0.69</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">Petal.Length</td>
<td align="left">108</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">3.5</td>
<td align="left">3</td>
<td align="left">42</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">5.6</td>
<td align="left">0.67</td>
<td align="left">2.1e-21</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">Petal.Width</td>
<td align="left">108</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1</td>
<td align="left">1.2</td>
<td align="left">42</td>
<td align="left">2.1</td>
<td align="left">0.28</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.6e-19</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left"><strong>Species</strong></td>
<td align="left">108</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">42</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">2.7e-24</td>
<td align="left">chisq.test</td>
</tr>
<tr class="even">
<td align="left">    setosa</td>
<td align="left">50</td>
<td align="left">46</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">0</td>
<td align="left">0</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">    versicolor</td>
<td align="left">49</td>
<td align="left">45</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1</td>
<td align="left">2.4</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">    virginica</td>
<td align="left">9</td>
<td align="left">8.3</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left">41</td>
<td align="left">98</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
</tbody>
</table>

<br>

``` r
mtcars %>%
  dplyr::mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>%
  group_by(am) %>%
  desctable(tests = list(.default = ~wilcox.test,
                         mpg      = ~t.test)) %>%
  pander
```

<table>
<colgroup>
<col width="6%" />
<col width="21%" />
<col width="8%" />
<col width="8%" />
<col width="19%" />
<col width="8%" />
<col width="8%" />
<col width="10%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">am: Automatic (n=19)<br/>N</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">am: Manual (n=13)<br/>N</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">tests<br/>p</th>
<th align="left"><br/>test</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">mpg</td>
<td align="left">19</td>
<td align="left">17</td>
<td align="left">4.2</td>
<td align="left">13</td>
<td align="left">23</td>
<td align="left">9.4</td>
<td align="left">0.0014</td>
<td align="left">t.test</td>
</tr>
<tr class="even">
<td align="left">cyl</td>
<td align="left">19</td>
<td align="left">8</td>
<td align="left">2</td>
<td align="left">13</td>
<td align="left">4</td>
<td align="left">2</td>
<td align="left">0.0039</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">disp</td>
<td align="left">19</td>
<td align="left">276</td>
<td align="left">164</td>
<td align="left">13</td>
<td align="left">120</td>
<td align="left">81</td>
<td align="left">0.00055</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">hp</td>
<td align="left">19</td>
<td align="left">175</td>
<td align="left">76</td>
<td align="left">13</td>
<td align="left">109</td>
<td align="left">47</td>
<td align="left">0.046</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">drat</td>
<td align="left">19</td>
<td align="left">3.1</td>
<td align="left">0.63</td>
<td align="left">13</td>
<td align="left">4.1</td>
<td align="left">0.37</td>
<td align="left">0.00014</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">wt</td>
<td align="left">19</td>
<td align="left">3.5</td>
<td align="left">0.41</td>
<td align="left">13</td>
<td align="left">2.3</td>
<td align="left">0.84</td>
<td align="left">4.3e-05</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">qsec</td>
<td align="left">19</td>
<td align="left">18</td>
<td align="left">2</td>
<td align="left">13</td>
<td align="left">17</td>
<td align="left">2.1</td>
<td align="left">0.27</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">vs</td>
<td align="left">19</td>
<td align="left">0</td>
<td align="left">1</td>
<td align="left">13</td>
<td align="left">1</td>
<td align="left">1</td>
<td align="left">0.36</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">gear</td>
<td align="left">19</td>
<td align="left">3</td>
<td align="left">0</td>
<td align="left">13</td>
<td align="left">4</td>
<td align="left">1</td>
<td align="left">7.6e-06</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">carb</td>
<td align="left">19</td>
<td align="left">3</td>
<td align="left">2</td>
<td align="left">13</td>
<td align="left">2</td>
<td align="left">3</td>
<td align="left">0.74</td>
<td align="left">wilcox.test</td>
</tr>
</tbody>
</table>

<br>

You might wonder why the formula expression. That is needed to capture the test name, and to provide it in the resulting table.

As with statistical functions, any statistical test function defined in R can be used.

The conditions are that the function

-   accepts a formula (`variable ~ grouping_variable`) as a first positional argument (as is the case with most tests, like `t.test`), and
-   returns an object with a `p.value` element.

Several convenience function are provided: formula versions for `chisq.test` and `fisher.test` using generic S3 methods (thus the behavior of standard calls to `chisq.test` and `fisher.test` are not modified), and `ANOVA`, a partial application of `oneway.test` with parameter *var.equal* = T.

Tips and tricks
===============

In the *stats* argument, you can not only feed function names, but even arbitrary function definitions, functional sequences (a feature provided with the pipe (`%>%`)), or partial applications (with the **purrr** package):

``` r
mtcars %>%
  desctable(stats = list("N"              = length,
                         "Sum of squares" = function(x) sum(x^2),
                         "Q1"             = . %>% quantile(prob = .25),
                         "Q3"             = purrr::partial(quantile, probs = .75))) %>%
  pander
```

<table style="width:56%;">
<colgroup>
<col width="12%" />
<col width="5%" />
<col width="23%" />
<col width="6%" />
<col width="6%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">N</th>
<th align="left">Sum of squares</th>
<th align="left">Q1</th>
<th align="left">Q3</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">mpg</td>
<td align="left">32</td>
<td align="left">14042</td>
<td align="left">15</td>
<td align="left">23</td>
</tr>
<tr class="even">
<td align="left">cyl</td>
<td align="left">32</td>
<td align="left">1324</td>
<td align="left">4</td>
<td align="left">8</td>
</tr>
<tr class="odd">
<td align="left">disp</td>
<td align="left">32</td>
<td align="left">2179627</td>
<td align="left">121</td>
<td align="left">326</td>
</tr>
<tr class="even">
<td align="left">hp</td>
<td align="left">32</td>
<td align="left">834278</td>
<td align="left">96</td>
<td align="left">180</td>
</tr>
<tr class="odd">
<td align="left">drat</td>
<td align="left">32</td>
<td align="left">423</td>
<td align="left">3.1</td>
<td align="left">3.9</td>
</tr>
<tr class="even">
<td align="left">wt</td>
<td align="left">32</td>
<td align="left">361</td>
<td align="left">2.6</td>
<td align="left">3.6</td>
</tr>
<tr class="odd">
<td align="left">qsec</td>
<td align="left">32</td>
<td align="left">10293</td>
<td align="left">17</td>
<td align="left">19</td>
</tr>
<tr class="even">
<td align="left">vs</td>
<td align="left">32</td>
<td align="left">14</td>
<td align="left">0</td>
<td align="left">1</td>
</tr>
<tr class="odd">
<td align="left">am</td>
<td align="left">32</td>
<td align="left">13</td>
<td align="left">0</td>
<td align="left">1</td>
</tr>
<tr class="even">
<td align="left">gear</td>
<td align="left">32</td>
<td align="left">452</td>
<td align="left">3</td>
<td align="left">4</td>
</tr>
<tr class="odd">
<td align="left">carb</td>
<td align="left">32</td>
<td align="left">334</td>
<td align="left">2</td>
<td align="left">4</td>
</tr>
</tbody>
</table>

<br>

In the *tests* arguments, you can also provide function definitions, functional sequences, and partial applications in the formulas:

``` r
iris %>%
  group_by(Species) %>%
  desctable(tests = list(.auto = tests_auto,
                         Sepal.Width = ~function(f) oneway.test(f, var.equal = F),
                         Petal.Length = ~. %>% oneway.test(var.equal = T),
                         Sepal.Length = ~purrr::partial(oneway.test, var.equal = T))) %>%
  pander
```

<table>
<colgroup>
<col width="4%" />
<col width="11%" />
<col width="4%" />
<col width="3%" />
<col width="4%" />
<col width="4%" />
<col width="8%" />
<col width="4%" />
<col width="3%" />
<col width="4%" />
<col width="4%" />
<col width="8%" />
<col width="4%" />
<col width="3%" />
<col width="4%" />
<col width="4%" />
<col width="5%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">Species: setosa (n=50)<br/>N</th>
<th align="left"><br/>Mean</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">Species: versicolor (n=50)<br/>N</th>
<th align="left"><br/>Mean</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">Species: virginica (n=50)<br/>N</th>
<th align="left"><br/>Mean</th>
<th align="left"><br/>sd</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">tests<br/>p</th>
<th align="left"><br/>test</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="left">50</td>
<td align="left">5</td>
<td align="left">0.35</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">5.9</td>
<td align="left">0.52</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">6.6</td>
<td align="left">0.64</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.7e-31</td>
<td align="left">purrr::partial(oneway.test, var.equal = T)</td>
</tr>
<tr class="even">
<td align="left">Sepal.Width</td>
<td align="left">50</td>
<td align="left">3.4</td>
<td align="left">0.38</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">2.8</td>
<td align="left">0.31</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">3</td>
<td align="left">0.32</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.4e-14</td>
<td align="left">function(f) oneway.test(f, var.equal = F)</td>
</tr>
<tr class="odd">
<td align="left">Petal.Length</td>
<td align="left">50</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.5</td>
<td align="left">0.18</td>
<td align="left">50</td>
<td align="left">4.3</td>
<td align="left">0.47</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">50</td>
<td align="left">5.6</td>
<td align="left">0.55</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">2.9e-91</td>
<td align="left">. %&gt;% oneway.test(var.equal = T)</td>
</tr>
<tr class="even">
<td align="left">Petal.Width</td>
<td align="left">50</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">0.2</td>
<td align="left">0.1</td>
<td align="left">50</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">1.3</td>
<td align="left">0.3</td>
<td align="left">50</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">2</td>
<td align="left">0.5</td>
<td align="left">3.3e-29</td>
<td align="left">kruskal.test</td>
</tr>
</tbody>
</table>

<br>

This allows you to modulate the behavior of `desctable` in every detail, such as using paired tests, or non *htest* tests.

``` r
# This is a contrived example, which would be better solved with a dedicated function
library(survival)

bladder$surv <- Surv(bladder$stop, bladder$event)

bladder %>%
  group_by(rx) %>%
  desctable(tests = list(.default = ~wilcox.test,
                         surv = ~. %>% survdiff %>% .$chisq %>% pchisq(1, lower.tail = F) %>% list(p.value = .))) %>%
  pander
```

<table>
<colgroup>
<col width="6%" />
<col width="15%" />
<col width="7%" />
<col width="7%" />
<col width="15%" />
<col width="7%" />
<col width="7%" />
<col width="9%" />
<col width="21%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"> </th>
<th align="left">rx: 1 (n=188)<br/>N</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">rx: 2 (n=152)<br/>N</th>
<th align="left"><br/>Med</th>
<th align="left"><br/>IQR</th>
<th align="left">tests<br/>p</th>
<th align="left"><br/>test</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="left">188</td>
<td align="left">24</td>
<td align="left">24</td>
<td align="left">152</td>
<td align="left">66</td>
<td align="left">19</td>
<td align="left">1.3e-56</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">number</td>
<td align="left">188</td>
<td align="left">1</td>
<td align="left">2</td>
<td align="left">152</td>
<td align="left">1</td>
<td align="left">2</td>
<td align="left">0.62</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">size</td>
<td align="left">188</td>
<td align="left">1</td>
<td align="left">2</td>
<td align="left">152</td>
<td align="left">1</td>
<td align="left">2</td>
<td align="left">0.32</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">stop</td>
<td align="left">188</td>
<td align="left">23</td>
<td align="left">20</td>
<td align="left">152</td>
<td align="left">25</td>
<td align="left">28</td>
<td align="left">0.17</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">event</td>
<td align="left">188</td>
<td align="left">0</td>
<td align="left">1</td>
<td align="left">152</td>
<td align="left">0</td>
<td align="left">1</td>
<td align="left">0.02</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="even">
<td align="left">enum</td>
<td align="left">188</td>
<td align="left">2.5</td>
<td align="left">1.5</td>
<td align="left">152</td>
<td align="left">2.5</td>
<td align="left">1.5</td>
<td align="left">1</td>
<td align="left">wilcox.test</td>
</tr>
<tr class="odd">
<td align="left">surv</td>
<td align="left">376</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">304</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">0.023</td>
<td align="left">. %&gt;% survdiff %&gt;% .$chisq %&gt;% pchisq(1, lower.tail = F) %&gt;% list(p.value = .)</td>
</tr>
</tbody>
</table>
