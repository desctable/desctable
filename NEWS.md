# desctable 0.3.0

#### New features

- NEW API: desctable is now `desc_table`, `desc_tests`, and `desc_output`
- New internal format: desctable uses dataframes with list-columns internally instead of nested lists
- New website with *pkgdown*

#### Bugfix

- Conditional formulas were hard-deprecated in 0.2.0, and have been soft-deprecated.

# desctable 0.2.0

#### New features

- Add support for `purrr::map`-like formulas for statistical and test functions
- Conditional formulas will be deprecated in 1.0. For example, replace `is.normal ~ mean | median` with `~ if (is.normal(.)) mean(.) else median(.)`

# desctable 0.1.9

#### Bugfixes

- Fix in default options for datatable output following DT update
- Fix pander export for R ≥ 4.0.0

# desctable 0.1.8

#### Code cleanup

- Use RStudio style guidelines for all code and docs
- Use fewer tidyverse functions internally, drop `purrr` dependancy

# desctable 0.1.7

#### Code cleanup

- Correct vignette and README with RStudio style guidelines

# desctable 0.1.6

#### Bugfixes

- Correct way to re-export `group_by` and `%>%`

# desctable 0.1.4-5

#### Bugfixes

- Documentation fix for group_by import (dplyr 0.8)
- Correct description for Dates

# desctable 0.1.3

#### CRAN publication update

- Add travis CI to repo
- Add CRAN badge to repo
- Correct a typo in the README and vignette
- Bugfix for when a statistical function returns a warning AND an error
- Add NEWS file for future releases
- Modify default and auto statistics for less automation
- Add dummy column name for the variable names column, and make dplyr happy
- Enable character and mixed columns
- Add CRAN mirror downloads badge to repo

# desctable 0.1.2

#### Bugfix

- Fix for a change in evaluation in rlang

# desctable 0.1.1

#### Bugfix

- Fix for changing arguments in fisher.test in upstream

# desctable 0.1.0

- Initial release.
