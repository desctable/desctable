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

# Introduction

Desctable aims to be a simple and expressive interface to building
statistical tables in R.

See [desctable.github.io](https://desctable.github.io) for usage ond
documentation.

# Installation

Install from CRAN (0.1.9) with

    install.packages("desctable")

or install the development version (0.3) from github with

    devtools::install_github("desctable/desctable")

# Roadmap

## 0.3

This new version introduces a new internal representation as well as an
entirely new API for desctable !  
The original `desctable` function and usage remains until 1.0, but
begins deprecation.

This new API is more flexible and more simple at the same time. Combine
`group_by`, `desc_table`, `desc_tests`, and `desc_output` to create
descriptive and comparative statistics tables and output them to various
formats.

The internal representation is now a simple dataframe in the simple
descriptive case, and a nested dataframe with list-columns for
comparative tables, allowing easier manipulation by the user.

## Next

-   Add a `desc_output` for {gt}
-   Implement a way to make tables for survival analysis.
-   Implement a way to make tables for multivariate models.
-   Allow univariate tests for simple tables
-   add a column for totals in grouped tables
