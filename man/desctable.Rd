% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{desctable}
\alias{desctable}
\alias{desctable.default}
\alias{desctable.grouped_df}
\title{Generate a statistics table}
\usage{
desctable(data, stats, tests, labels)

\method{desctable}{default}(data, stats = stats_auto, tests, labels = NULL)

\method{desctable}{grouped_df}(data, stats = stats_auto, tests = tests_auto, labels = NULL)
}
\arguments{
\item{data}{The dataframe to analyze}

\item{stats}{A list of named statistics to apply to each element of the dataframe, or a function returning a list of named statistics}

\item{tests}{A list of statistical tests to use when calling desctable with a grouped_df}

\item{labels}{A named character vector of labels to use instead of variable names}
}
\value{
A desctable object, which prints to a table of statistics for all variables
}
\description{
Generate a statistics table with the chosen statistical functions, and tests if given a \code{"grouped"} dataframe.
}
\section{Labels}{

labels is an option named character vector used to make the table prettier.

If given, the variable names for which there is a label will be replaced by their corresponding label.

Not all variables need to have a label, and labels for non-existing variables are ignored.

labels must be given in the form c(unquoted_variable_name = "label")
}

\section{Stats}{

The stats can be a function which takes a dataframe and returns a list of statistical functions to use.

stats can also be a named list of statistical functions, or purrr::map like formulas.

The names will be used as column names in the resulting table. If an element of the list is a function, it will be used as-is for the stats.
}

\section{Tests}{

The tests can be a function which takes a variable and a grouping variable, and returns an appropriate statistical test to use in that case.

tests can also be a named list of statistical test functions, associating the name of a variable in the data and a test to use specifically for that variable.

That test name must be expressed as a single-term formula (e.g. \code{~t.test}), or a purrr::map like formula
(e.g. \code{~t.test(., var.equal = T)}). You don't have to specify tests for all the variables: a default test for
all other variables can be defined with the name \code{.default}, and an automatic test can be defined with the name \code{.auto}.

If data is a grouped dataframe (using \code{group_by}), subtables are created and statistic tests are performed over each sub-group.
}

\section{Output}{

The output is a desctable object, which is a list of named dataframes that can be further manipulated. Methods for printing, using in \pkg{pander} and \pkg{DT} are present. Printing reduces the object to a dataframe.
}

\examples{
iris \%>\%
  desctable()

# Does the same as stats_auto here
iris \%>\%
  desctable(stats = list("N"      = length,
                         "Mean"   = ~ if (is.normal(.)) mean(.),
                         "sd"     = ~ if (is.normal(.)) sd(.),
                         "Med"    = stats::median,
                         "IQR"    = ~ if(!is.factor(.)) IQR(.)))

# With labels
mtcars \%>\% desctable(labels = c(hp  = "Horse Power",
                                cyl = "Cylinders",
                                mpg = "Miles per gallon"))

# With grouping on a factor
iris \%>\%
  group_by(Species) \%>\%
  desctable(stats = stats_default)

# With nested grouping, on arbitrary variables
mtcars \%>\%
  group_by(vs, cyl) \%>\%
  desctable()

# With grouping on a condition, and choice of tests
iris \%>\%
  group_by(Petal.Length > 5) \%>\%
  desctable(tests = list(.auto = tests_auto, Species = ~chisq.test))
}
\seealso{
\code{\link{stats_auto}}

\code{\link{tests_auto}}

\code{\link{print.desctable}}

\code{\link{pander.desctable}}

\code{\link{datatable.desctable}}
}
\keyword{deprecated}
