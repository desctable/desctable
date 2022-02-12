Desctable
================

[![Travis-CI Build
Status](https://travis-ci.org/MaximeWack/desctable.svg?branch=master)](https://travis-ci.org/MaximeWack/desctable)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/desctable)](https://cran.r-project.org/package=desctable)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/desctable)](https://www.r-pkg.org:443/pkg/desctable)

Introduction
============

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

-   generate descriptive and comparative statistics tables with nesting
-   keep the syntax as simple as possible
-   have good reasonable defaults
-   be entirely customizable, using standard R tools and functions
-   produce the simplest (as a data structure) output possible
-   provide helpers for different outputs
-   integrate with “modern” R usage, and the **tidyverse** set of tools
-   apply functional paradigms

Installation
============

Install from CRAN with

    install.packages("desctable")

or install the development version from github with

    devtools::install_github("maximewack/desctable")

Loading
=======

    library(desctable)

It is recommended to read this manual through its vignette:

    vignette("desctable")

------------------------------------------------------------------------

Descriptive tables
==================

Simple usage
------------

**desctable** uses and exports the pipe (`%>%`) operator (from packages
**magrittr** and **dplyr** fame), though it is not mandatory to use it.

The single interface to the package is its eponymous `desctable`
function.

When used on a data.frame, it returns a descriptive table:

    iris %>%
      desctable()

    ##                         N        %     Mean        sd  Med IQR
    ## 1        Sepal.Length 150       NA       NA        NA 5.80 1.3
    ## 2         Sepal.Width 150       NA 3.057333 0.4358663 3.00 0.5
    ## 3        Petal.Length 150       NA       NA        NA 4.35 3.5
    ## 4         Petal.Width 150       NA       NA        NA 1.30 1.5
    ## 5             Species 150       NA       NA        NA   NA  NA
    ## 6     Species: setosa  50 33.33333       NA        NA   NA  NA
    ## 7 Species: versicolor  50 33.33333       NA        NA   NA  NA
    ## 8  Species: virginica  50 33.33333       NA        NA   NA  NA

    desctable(mtcars)

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

<br>

As you can see with these two examples, `desctable` describes every
variable, with individual levels for factors. It picks statistical
functions depending on the type and distribution of the variables in the
data, and applies those statistical functions only on the relevant
variables.

Output
------

The object produced by `desctable` is in fact a list of data.frames,
with a “desctable” class.  
Methods for reduction to a simple dataframe (`as.data.frame`,
automatically used for printing), conversion to markdown (`pander`), and
interactive html output with **DT** (`datatable`) are provided:

    iris %>%
      desctable() %>%
      pander()

|              | N   | %  | Mean | sd   | Med | IQR |
| :----------- | :-- | :- | :--- | :--- | :-- | :-- |
| Sepal.Length | 150 |    |      |      | 5.8 | 1.3 |
| Sepal.Width  | 150 |    | 3.1  | 0.44 | 3   | 0.5 |
| Petal.Length | 150 |    |      |      | 4.3 | 3.5 |
| Petal.Width  | 150 |    |      |      | 1.3 | 1.5 |
| **Species**  | 150 |    |      |      |     |     |
| setosa       | 50  | 33 |      |      |     |     |
| versicolor   | 50  | 33 |      |      |     |     |
| virginica    | 50  | 33 |      |      |     |     |

<br>

To use `pander` you need to load the package yourself.

Calls to `pander` and `datatable` with “regular” dataframes will not be
affected by the defaults used in the package, and you can modify these
defaults for **desctable** objects.

The `datatable` wrapper function for desctable objects comes with some
default options and formatting such as freezing the row names and table
header, export buttons, and rounding of values. Both `pander` and
`datatable` wrapper take a *digits* argument to set the number of
decimals to show. (`pander` uses the *digits*, *justify* and *missing*
arguments of `pandoc.table`, whereas `datatable` calls `prettyNum` with
the `digits` parameter, and removes `NA` values. You can set
`digits = NULL` if you want the full table and format it yourself)

Subsequent outputs in this README will use **pander**.

## Advanced usage

`desctable` automatically chooses statistical functions if none is
provided, using the following algorithm:

-   always show N
-   if there are factors, show %
-   if there are normally distributed variables, show Mean and SD
-   if there are non-normally distributed variables, show Median and IQR

For each variable in the table, compute the relevant statistical
functions in that list (non-applicable functions will safely return
`NA`).

You can specify the statistical functions yourself with the *stats*
argument. This argument can either be:

  - a function for automatic selection of appropriate statistical
    functions, depending on the data
  - a named list of functions/formulas

The functions/formulas leverage the **tidyverse** way of working with
anonymous functions, i.e.:

If a *function*, is is used as is. If a *formula*, e.g. ‘\~ .x + 1’, it
is converted to a function. There are three ways to refer to the
arguments:

  - For a single argument function, use ‘.’
  - For a two argument function, use ‘.x’ and ‘.y’
  - For more arguments, use ‘..1’, ‘..2’, ‘..3’ etc

This syntax allows you to create very compact anonymous functions.

### Automatic function

The default value for the *stats* argument is `stats_auto`, provided in
the package.

Several other “automatic statistical functions” are defined in this
package: `stats_auto`, `stats_default`, `stats_normal`,
`stats_nonnormal`.

You can also provide your own automatic function, which needs to

-   accept a dataframe as its argument (whether to use this dataframe or
    not in the function is your choice), and
-   return a named list of statistical functions to use, as defined in
    the subsequent paragraphs.

<!-- -->

    # Strictly equivalent to iris %>% desctable() %>% pander()
    iris %>%
      desctable(stats = stats_auto) %>%
      pander()

|              | N   | %  | Mean | sd   | Med | IQR |
| :----------- | :-- | :- | :--- | :--- | :-- | :-- |
| Sepal.Length | 150 |    |      |      | 5.8 | 1.3 |
| Sepal.Width  | 150 |    | 3.1  | 0.44 | 3   | 0.5 |
| Petal.Length | 150 |    |      |      | 4.3 | 3.5 |
| Petal.Width  | 150 |    |      |      | 1.3 | 1.5 |
| **Species**  | 150 |    |      |      |     |     |
| setosa       | 50  | 33 |      |      |     |     |
| versicolor   | 50  | 33 |      |      |     |     |
| virginica    | 50  | 33 |      |      |     |     |

<br>

For reference, here is the body of the `stats_auto` function in the
package:

    ## function (data) 
    ## {
    ##     shapiro <- data %>% Filter(f = is.numeric) %>% lapply(is.normal) %>% 
    ##         unlist()
    ##     if (length(shapiro) == 0) {
    ##         normal <- F
    ##         nonnormal <- F
    ##     }
    ##     else {
    ##         normal <- any(shapiro)
    ##         nonnormal <- any(!shapiro)
    ##     }
    ##     fact <- data %>% lapply(is.factor) %>% unlist() %>% any()
    ##     if (fact & normal & !nonnormal) 
    ##         stats_normal(data)
    ##     else if (fact & !normal & nonnormal) 
    ##         stats_nonnormal(data)
    ##     else if (fact & !normal & !nonnormal) 
    ##         list(N = length, `%` = percent)
    ##     else if (!fact & normal & nonnormal) 
    ##         list(N = length, Mean = ~if (is.normal(.)) mean(.), sd = ~if (is.normal(.)) sd(.), 
    ##             Med = stats::median, IQR = ~if (!is.factor(.)) IQR(.))
    ##     else if (!fact & normal & !nonnormal) 
    ##         list(N = length, Mean = mean, sd = stats::sd)
    ##     else if (!fact & !normal & nonnormal) 
    ##         list(N = length, Med = stats::median, IQR = IQR)
    ##     else stats_default(data)
    ## }
    ## <bytecode: 0x55a17f868778>
    ## <environment: namespace:desctable>

<br>

### Statistical functions

Statistical functions can be **any** function defined in R that you want
to use, such as `length` or `mean`.

The only condition is that they return a single numerical value. One
exception is when they return a vector of length `1 + nlevels(x)` when
applied to factors, as is needed for the `percent` function.

As mentioned above, they need to be used inside a **named list**, such
as

    mtcars %>%
      desctable(stats = list("N" = length, "Mean" = mean, "SD" = sd)) %>%
      pander()

|      | N  | Mean | SD   |
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

Several convenience functions are included in this package.

  - `percent`, which prints percentages of levels in a factor
  - `IQR`, which re-implements `stats::IQR` but works better with `NA`
    values
  - `is.normal`, which tests for normality using the following method:
    `length(na.omit(x)) > 30 & shapiro.test(x)$p.value > .1`

Be aware that **all functions will be used on variables stripped of
their `NA` values\!** This is necessary for most statistical functions
to be useful, and makes **N** (`length`) show only the number of
observations in the dataset for each variable.

### Labels

It is often the case that variable names are not “pretty” enough to be
used as-is in a table.  
Although you could still edit the variable labels in the table
afterwards using sub-setting or string replacement functions, we provide
a facility for this using the **labels** argument.

The **labels** argument is a named character vector associating variable
names and labels.  
You don’t need to provide labels for all the variables, and extra labels
will be silently discarded. This allows you to define a “global” labels
vector and use it for multiple tables even after variable selections.

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

|                         | N  | %  | Mean | sd   | Med | IQR  |
| :---------------------- | :- | :- | :--- | :--- | :-- | :--- |
| Miles/(US) gallon       | 32 |    | 20   | 6    | 19  | 7.4  |
| Number of cylinders     | 32 |    |      |      | 6   | 4    |
| Displacement (cu.in.)   | 32 |    |      |      | 196 | 205  |
| Gross horsepower        | 32 |    |      |      | 123 | 84   |
| Rear axle ratio         | 32 |    | 3.6  | 0.53 | 3.7 | 0.84 |
| Weight (1000 lbs)       | 32 |    |      |      | 3.3 | 1    |
| ¼ mile time             | 32 |    | 18   | 1.8  | 18  | 2    |
| V/S                     | 32 |    |      |      | 0   | 1    |
| **Transmission**        | 32 |    |      |      |     |      |
| Automatic               | 19 | 59 |      |      |     |      |
| Manual                  | 13 | 41 |      |      |     |      |
| Number of forward gears | 32 |    |      |      | 4   | 1    |
| Number of carburetors   | 32 |    |      |      | 2   | 2    |

<br>

------------------------------------------------------------------------

Comparative tables
==================

Simple usage
------------

Creating a comparative table (between groups defined by a factor) using
`desctable` is as easy as creating a descriptive table.

It leverages the `group_by` function from **dplyr**:

    iris %>%
      group_by(Species) %>%
      desctable() -> iris_by_Species

    iris_by_Species

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
    ##   Species: setosa (n=50) / IQR Species: versicolor (n=50) / N
    ## 1                        0.400                             50
    ## 2                        0.475                             50
    ## 3                        0.175                             50
    ## 4                        0.100                             50
    ##   Species: versicolor (n=50) / Mean Species: versicolor (n=50) / sd
    ## 1                             5.936                       0.5161711
    ## 2                             2.770                       0.3137983
    ## 3                             4.260                       0.4699110
    ## 4                                NA                              NA
    ##   Species: versicolor (n=50) / Med Species: versicolor (n=50) / IQR
    ## 1                             5.90                            0.700
    ## 2                             2.80                            0.475
    ## 3                             4.35                            0.600
    ## 4                             1.30                            0.300
    ##   Species: virginica (n=50) / N Species: virginica (n=50) / Mean
    ## 1                            50                            6.588
    ## 2                            50                            2.974
    ## 3                            50                            5.552
    ## 4                            50                               NA
    ##   Species: virginica (n=50) / sd Species: virginica (n=50) / Med
    ## 1                      0.6358796                            6.50
    ## 2                      0.3224966                            3.00
    ## 3                      0.5518947                            5.55
    ## 4                             NA                            2.00
    ##   Species: virginica (n=50) / IQR                             tests / p
    ## 1                           0.675 0.00000000000000000000000000015050590
    ## 2                           0.375 0.00000000000000004492017133309114263
    ## 3                           0.775 0.00000000000000000000000000004803974
    ## 4                           0.500 0.00000000000000000000000000003261796
    ##                 tests / test
    ## 1 oneway.test(var.equal = F)
    ## 2 oneway.test(var.equal = T)
    ## 3               kruskal.test
    ## 4               kruskal.test

<br>

The result is a table containing a descriptive sub-table for each level
of the grouping factor (the statistical functions rules are applied to
each sub-table independently), with the statistical tests performed, and
their p values.

When displayed as a flat dataframe, the grouping header appears in each
variable name.

You can also see the grouping headers by inspecting the resulting
object, which is a nested list of dataframes, each dataframe being named
after the grouping factor and its levels (with sample size for each).

    str(iris_by_Species)

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
    ##   ..$ p   : num [1:4] 0.0000000000000000000000000001505 0.0000000000000000449201713330911 0.000000000000000000000000000048 0.0000000000| __truncated__
    ##   ..$ test: chr [1:4] "oneway.test(var.equal = F)" "oneway.test(var.equal = T)" "kruskal.test" "kruskal.test"
    ##  - attr(*, "class")= chr "desctable"

<br>

You can specify groups based on any variable, not only factors:

    # With pander output
    mtcars %>%
      group_by(cyl) %>%
      desctable() %>%
      pander()

|      | cyl: 4 (n=11)<br/>N | <br/>Med | <br/>IQR | cyl: 6 (n=7)<br/>N | <br/>Med | <br/>IQR | cyl: 8 (n=14)<br/>N | <br/>Med | <br/>IQR | tests<br/>p | <br/>test    |
| :--- | :------------------ | :------- | :------- | :----------------- | :------- | :------- | :------------------ | :------- | :------- | :---------- | :----------- |
| mpg  | 11                  | 26       | 7.6      | 7                  | 20       | 2.4      | 14                  | 15       | 1.8      | 0.0000026   | kruskal.test |
| disp | 11                  | 108      | 42       | 7                  | 168      | 36       | 14                  | 350      | 88       | 0.0000016   | kruskal.test |
| hp   | 11                  | 91       | 30       | 7                  | 110      | 13       | 14                  | 192      | 65       | 0.0000033   | kruskal.test |
| drat | 11                  | 4.1      | 0.35     | 7                  | 3.9      | 0.56     | 14                  | 3.1      | 0.15     | 0.00075     | kruskal.test |
| wt   | 11                  | 2.2      | 0.74     | 7                  | 3.2      | 0.62     | 14                  | 3.8      | 0.48     | 0.000011    | kruskal.test |
| qsec | 11                  | 19       | 1.4      | 7                  | 18       | 2.4      | 14                  | 17       | 1.5      | 0.0062      | kruskal.test |
| vs   | 11                  | 1        | 0        | 7                  | 1        | 1        | 14                  | 0        | 0        | 0.000032    | kruskal.test |
| am   | 11                  | 1        | 0.5      | 7                  | 0        | 1        | 14                  | 0        | 0        | 0.014       | kruskal.test |
| gear | 11                  | 4        | 0        | 7                  | 4        | 0.5      | 14                  | 3        | 0        | 0.0062      | kruskal.test |
| carb | 11                  | 2        | 1        | 7                  | 4        | 1.5      | 14                  | 3.5      | 1.8      | 0.0017      | kruskal.test |

<br>

You can also specify groups based on an expression

    iris %>%
      group_by(Petal.Length > 5) %>%
      desctable() %>%
      pander()

|              | Petal.Length \> 5: FALSE (n=108)<br/>N | <br/>% | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | Petal.Length \> 5: TRUE (n=42)<br/>N | <br/>% | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | tests<br/>p                   | <br/>test   |
| :----------- | :------------------------------------- | :----- | :-------- | :------ | :------- | :------- | :----------------------------------- | :----- | :-------- | :------ | :------- | :------- | :---------------------------- | :---------- |
| Sepal.Length | 108                                    |        |           |         | 5.5      | 1        | 42                                   |        |           |         | 6.7      | 0.85     | 0.0000000000000016            | wilcox.test |
| Sepal.Width  | 108                                    |        | 3.1       | 0.48    | 3        | 0.6      | 42                                   |        |           |         | 3        | 0.4      | 0.69                          | wilcox.test |
| Petal.Length | 108                                    |        |           |         | 3.5      | 3        | 42                                   |        |           |         | 5.6      | 0.67     | 0.0000000000000000000021      | wilcox.test |
| Petal.Width  | 108                                    |        |           |         | 1        | 1.2      | 42                                   |        | 2.1       | 0.28    | 2.1      | 0.47     | 0.00000000000000000016        | wilcox.test |
| **Species**  | 108                                    |        |           |         |          |          | 42                                   |        |           |         |          |          | 0.000000000000000000000000025 | fisher.test |
| setosa       | 50                                     | 46     |           |         |          |          | 0                                    | 0      |           |         |          |          |                               |             |
| versicolor   | 49                                     | 45     |           |         |          |          | 1                                    | 2.4    |           |         |          |          |                               |             |
| virginica    | 9                                      | 8.3    |           |         |          |          | 41                                   | 98     |           |         |          |          |                               |             |

<br>

Multiple nested groups are also possible:

    mtcars %>%
      dplyr::mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>%
      group_by(vs, am, cyl) %>%
      desctable() %>%
      pander()

|      | vs: 0 (n=18)<br/>am: Automatic (n=12)<br/>cyl: 8 (n=12)<br/>N | <br/><br/><br/>Med | <br/><br/><br/>IQR | <br/><br/>tests<br/>p | <br/><br/><br/>test | <br/>am: Manual (n=6)<br/>cyl: 4 (n=1)<br/>N | <br/><br/><br/>Med | <br/><br/><br/>IQR | <br/><br/>cyl: 6 (n=3)<br/>N | <br/><br/><br/>Med | <br/><br/><br/>IQR | <br/><br/>cyl: 8 (n=2)<br/>N | <br/><br/><br/>Med | <br/><br/><br/>IQR | <br/><br/>tests<br/>p | <br/><br/><br/>test | vs: 1 (n=14)<br/>am: Automatic (n=7)<br/>cyl: 4 (n=3)<br/>N | <br/><br/><br/>Med | <br/><br/><br/>IQR | <br/><br/>cyl: 6 (n=4)<br/>N | <br/><br/><br/>Med | <br/><br/><br/>IQR | <br/><br/>tests<br/>p | <br/><br/><br/>test | <br/>am: Manual (n=7)<br/>cyl: 4 (n=7)<br/>N | <br/><br/><br/>Med | <br/><br/><br/>IQR | <br/><br/>tests<br/>p | <br/><br/><br/>test |
| :--- | :------------------------------------------------------------ | :----------------- | :----------------- | :-------------------- | :------------------ | :------------------------------------------- | :----------------- | :----------------- | :--------------------------- | :----------------- | :----------------- | :--------------------------- | :----------------- | :----------------- | :-------------------- | :------------------ | :---------------------------------------------------------- | :----------------- | :----------------- | :--------------------------- | :----------------- | :----------------- | :-------------------- | :------------------ | :------------------------------------------- | :----------------- | :----------------- | :-------------------- | :------------------ |
| mpg  | 12                                                            | 15                 | 2.6                |                       | no.test             | 1                                            | 26                 | 0                  | 3                            | 21                 | 0.65               | 2                            | 15                 | 0.4                | 0.11                  | kruskal.test        | 3                                                           | 23                 | 1.5                | 4                            | 19                 | 1.7                | 0.057                 | wilcox.test         | 7                                            | 30                 | 6.3                |                       | no.test             |
| disp | 12                                                            | 355                | 113                |                       | no.test             | 1                                            | 120                | 0                  | 3                            | 160                | 7.5                | 2                            | 326                | 25                 | 0.11                  | kruskal.test        | 3                                                           | 141                | 13                 | 4                            | 196                | 66                 | 0.05                  | wilcox.test         | 7                                            | 79                 | 24                 |                       | no.test             |
| hp   | 12                                                            | 180                | 44                 |                       | no.test             | 1                                            | 91                 | 0                  | 3                            | 110                | 32                 | 2                            | 300                | 36                 | 0.11                  | kruskal.test        | 3                                                           | 95                 | 18                 | 4                            | 116                | 14                 | 0.05                  | wilcox.test         | 7                                            | 66                 | 36                 |                       | no.test             |
| drat | 12                                                            | 3.1                | 0.11               |                       | no.test             | 1                                            | 4.4                | 0                  | 3                            | 3.9                | 0.14               | 2                            | 3.9                | 0.34               | 0.33                  | kruskal.test        | 3                                                           | 3.7                | 0.11               | 4                            | 3.5                | 0.92               | 0.85                  | wilcox.test         | 7                                            | 4.1                | 0.2                |                       | no.test             |
| wt   | 12                                                            | 3.8                | 0.81               |                       | no.test             | 1                                            | 2.1                | 0                  | 3                            | 2.8                | 0.13               | 2                            | 3.4                | 0.2                | 0.12                  | kruskal.test        | 3                                                           | 3.1                | 0.36               | 4                            | 3.4                | 0.061              | 0.05                  | wilcox.test         | 7                                            | 1.9                | 0.53               |                       | no.test             |
| qsec | 12                                                            | 17                 | 0.67               |                       | no.test             | 1                                            | 17                 | 0                  | 3                            | 16                 | 0.76               | 2                            | 15                 | 0.05               | 0.17                  | kruskal.test        | 3                                                           | 20                 | 1.4                | 4                            | 19                 | 0.89               | 0.23                  | wilcox.test         | 7                                            | 19                 | 0.62               |                       | no.test             |
| gear | 12                                                            | 3                  | 0                  |                       | no.test             | 1                                            | 5                  | 0                  | 3                            | 4                  | 0.5                | 2                            | 5                  | 0                  | 0.29                  | kruskal.test        | 3                                                           | 4                  | 0.5                | 4                            | 3.5                | 1                  | 0.84                  | wilcox.test         | 7                                            | 4                  | 0                  |                       | no.test             |
| carb | 12                                                            | 3                  | 2                  |                       | no.test             | 1                                            | 2                  | 0                  | 3                            | 4                  | 1                  | 2                            | 6                  | 2                  | 0.26                  | kruskal.test        | 3                                                           | 2                  | 0.5                | 4                            | 2.5                | 3                  | 0.85                  | wilcox.test         | 7                                            | 1                  | 1                  |                       | no.test             |

<br>

In the case of nested groups (a.k.a. sub-group analysis), statistical
tests are performed only between the groups of the deepest grouping
level.

Statistical tests are automatically selected depending on the data and
the grouping factor.

Advanced usage
--------------

`desctable` automatically chooses statistical functions if none is
provided, using the following algorithm:

-   if the variable is a factor, use `fisher.test`
-   if the grouping factor has only one level, use the provided
    `no.test` (which does nothing)
-   if the grouping factor has two levels
    -   and the variable presents homoskedasticity (p value for
        `var.test` &gt; .1) and normality of distribution in both
        groups, use `t.test(var.equal = T)`
    -   and the variable does not present homoskedasticity (p value for
        `var.test` &lt; .1) but normality of distribution in both
        groups, use `t.test(var.equal = F)`
    -   else use `wilcox.test`
-   if the grouping factor has more than two levels
    -   and the variable presents homoskedasticity (p value for
        `bartlett.test` &gt; .1) and normality of distribution in all
        groups, use `oneway.test(var.equal = T)`
    -   and the variable does not present homoskedasticity (p value for
        `bartlett.test` &lt; .1) but normality of distribution in all
        groups, use `oneway.test(var.equal = F)`
    -   else use `kruskal.test`

You can specify the statistical test functions yourself with the *tests*
argument. This argument can either be:

  - a function for automatic selection of appropriate statistical test
    functions, depending on the data
  - a named list of statistical test functions

Please note that the statistical test functions **need** to be given as
*formulas* (to capture the name of the test to display in the table)
This also allows to specify optional arguments of such functions, and go
around non-standard test functions (see **Statistical test functions**).

### Automatic function

The default value for the *tests* argument is `tests_auto`, provided in
the package.

You can also provide your own automatic function, which needs to

-   accept a variable and a grouping factor as its arguments, and
-   return a single-term formula containing a statistical test function.

This function will be used on every variable and every grouping factor
to determine the appropriate test.

``` r
# Strictly equivalent to iris %>% group_by(Species) %>% desctable() %>% pander()
iris %>%
  group_by(Species) %>%
  desctable(tests = tests_auto) %>%
  pander()
```

|              | Species: setosa (n=50)<br/>N | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | Species: versicolor (n=50)<br/>N | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | Species: virginica (n=50)<br/>N | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | tests<br/>p                      | <br/>test                  |
| :----------- | :--------------------------- | :-------- | :------ | :------- | :------- | :------------------------------- | :-------- | :------ | :------- | :------- | :------------------------------ | :-------- | :------ | :------- | :------- | :------------------------------- | :------------------------- |
| Sepal.Length | 50                           | 5         | 0.35    | 5        | 0.4      | 50                               | 5.9       | 0.52    | 5.9      | 0.7      | 50                              | 6.6       | 0.64    | 6.5      | 0.67     | 0.00000000000000000000000000015  | oneway.test(var.equal = F) |
| Sepal.Width  | 50                           | 3.4       | 0.38    | 3.4      | 0.48     | 50                               | 2.8       | 0.31    | 2.8      | 0.48     | 50                              | 3         | 0.32    | 3        | 0.38     | 0.000000000000000045             | oneway.test(var.equal = T) |
| Petal.Length | 50                           |           |         | 1.5      | 0.18     | 50                               | 4.3       | 0.47    | 4.3      | 0.6      | 50                              | 5.6       | 0.55    | 5.5      | 0.78     | 0.000000000000000000000000000048 | kruskal.test               |
| Petal.Width  | 50                           |           |         | 0.2      | 0.1      | 50                               |           |         | 1.3      | 0.3      | 50                              |           |         | 2        | 0.5      | 0.000000000000000000000000000033 | kruskal.test               |

<br>

For reference, here is the body of the `tests_auto` function in the
package:

    ## function (var, grp) 
    ## {
    ##     grp <- factor(grp)
    ##     if (nlevels(grp) < 2) 
    ##         ~no.test(.)
    ##     else if (is.factor(var)) {
    ##         if (tryCatch(is.numeric(fisher.test(var ~ grp)$p.value), 
    ##             error = function(e) F)) 
    ##             ~fisher.test(.)
    ##         else ~chisq.test(.)
    ##     }
    ##     else {
    ##         all_normal <- all(tapply(var, grp, is.normal))
    ##         if (nlevels(grp) == 2) {
    ##             if (all_normal) {
    ##                 if (tryCatch(stats::var.test(var ~ grp)$p.value > 
    ##                   0.1, warning = function(e) F, error = function(e) F)) 
    ##                   ~t.test(., var.equal = T)
    ##                 else ~t.test(., var.equal = F)
    ##             }
    ##             else ~wilcox.test(.)
    ##         }
    ##         else {
    ##             if (all_normal) {
    ##                 if (tryCatch(stats::bartlett.test(var ~ grp)$p.value > 
    ##                   0.1, warning = function(e) F, error = function(e) F)) 
    ##                   ~oneway.test(., var.equal = T)
    ##                 else ~oneway.test(., var.equal = F)
    ##             }
    ##             else ~kruskal.test(.)
    ##         }
    ##     }
    ## }
    ## <bytecode: 0x55a17ef0c290>
    ## <environment: namespace:desctable>

<br>

### Statistical test functions

You can provide a named list of statistical functions, but here the
mechanism is a bit different from the *stats* argument.

The list must contain either `.auto` or `.default`.

-   `.auto` needs to be an automatic function, such as `tests_auto`. It
    will be used by default on all variables to select a test
-   `.default` needs to be a single-term formula containing a
    statistical test function that will be used on all variables

You can also provide overrides to use specific tests for specific
variables.  
This is done using list items named as the variable and containing a
single-term formula function.

``` r
iris %>%
  group_by(Petal.Length > 5) %>%
  desctable(tests = list(.auto   = tests_auto,
                         Species = ~chisq.test(.))) %>%
  pander()
```

|              | Petal.Length \> 5: FALSE (n=108)<br/>N | <br/>% | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | Petal.Length \> 5: TRUE (n=42)<br/>N | <br/>% | <br/>Mean | <br/>sd | <br/>Med | <br/>IQR | tests<br/>p                 | <br/>test   |
| :----------- | :------------------------------------- | :----- | :-------- | :------ | :------- | :------- | :----------------------------------- | :----- | :-------- | :------ | :------- | :------- | :-------------------------- | :---------- |
| Sepal.Length | 108                                    |        |           |         | 5.5      | 1        | 42                                   |        |           |         | 6.7      | 0.85     | 0.0000000000000016          | wilcox.test |
| Sepal.Width  | 108                                    |        | 3.1       | 0.48    | 3        | 0.6      | 42                                   |        |           |         | 3        | 0.4      | 0.69                        | wilcox.test |
| Petal.Length | 108                                    |        |           |         | 3.5      | 3        | 42                                   |        |           |         | 5.6      | 0.67     | 0.0000000000000000000021    | wilcox.test |
| Petal.Width  | 108                                    |        |           |         | 1        | 1.2      | 42                                   |        | 2.1       | 0.28    | 2.1      | 0.47     | 0.00000000000000000016      | wilcox.test |
| **Species**  | 108                                    |        |           |         |          |          | 42                                   |        |           |         |          |          | 0.0000000000000000000000027 | chisq.test  |
| setosa       | 50                                     | 46     |           |         |          |          | 0                                    | 0      |           |         |          |          |                             |             |
| versicolor   | 49                                     | 45     |           |         |          |          | 1                                    | 2.4    |           |         |          |          |                             |             |
| virginica    | 9                                      | 8.3    |           |         |          |          | 41                                   | 98     |           |         |          |          |                             |             |

<br>

``` r
mtcars %>%
  dplyr::mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>%
  group_by(am) %>%
  desctable(tests = list(.default = ~wilcox.test(.),
                         mpg      = ~t.test(.))) %>%
  pander()
```

|      | am: Automatic (n=19)<br/>N | <br/>Med | <br/>IQR | am: Manual (n=13)<br/>N | <br/>Med | <br/>IQR | tests<br/>p | <br/>test   |
| :--- | :------------------------- | :------- | :------- | :---------------------- | :------- | :------- | :---------- | :---------- |
| mpg  | 19                         | 17       | 4.2      | 13                      | 23       | 9.4      | 0.0014      | t.test      |
| cyl  | 19                         | 8        | 2        | 13                      | 4        | 2        | 0.0039      | wilcox.test |
| disp | 19                         | 276      | 164      | 13                      | 120      | 81       | 0.00055     | wilcox.test |
| hp   | 19                         | 175      | 76       | 13                      | 109      | 47       | 0.046       | wilcox.test |
| drat | 19                         | 3.1      | 0.63     | 13                      | 4.1      | 0.37     | 0.00014     | wilcox.test |
| wt   | 19                         | 3.5      | 0.41     | 13                      | 2.3      | 0.84     | 0.000043    | wilcox.test |
| qsec | 19                         | 18       | 2        | 13                      | 17       | 2.1      | 0.27        | wilcox.test |
| vs   | 19                         | 0        | 1        | 13                      | 1        | 1        | 0.36        | wilcox.test |
| gear | 19                         | 3        | 0        | 13                      | 4        | 1        | 0.0000076   | wilcox.test |
| carb | 19                         | 3        | 2        | 13                      | 2        | 3        | 0.74        | wilcox.test |

<br>

You might wonder why the formula expression. That is needed to capture
the test name, and to provide it in the resulting table.

As with statistical functions, **any** statistical test function defined
in R can be used.

The conditions are that the function

-   accepts a formula (`variable ~ grouping_variable`) as a first
    positional argument (as is the case with most tests, like `t.test`),
    and
-   returns an object with a `p.value` element.

Several convenience function are provided: formula versions for
`chisq.test` and `fisher.test` using generic S3 methods (thus the
behavior of standard calls to `chisq.test` and `fisher.test` are not
modified), and `ANOVA`, a partial application of `oneway.test` with
parameter *var.equal* = T.
