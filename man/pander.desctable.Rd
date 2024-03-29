% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{pander.desctable}
\alias{pander.desctable}
\title{Pander method for desctable}
\usage{
\method{pander}{desctable}(
  x = NULL,
  digits = 2,
  justify = "left",
  missing = "",
  keep.line.breaks = T,
  split.tables = Inf,
  emphasize.rownames = F,
  ...
)
}
\arguments{
\item{x}{A desctable}

\item{digits}{passed to \code{format}. Can be a vector specifying values for each column (has to be the same length as number of columns).}

\item{justify}{defines alignment in cells passed to \code{format}. Can be \code{left}, \code{right} or \code{centre}, which latter can be also spelled as \code{center}. Defaults to \code{centre}. Can be abbreviated to a string consisting of the letters \code{l}, \code{c} and \code{r} (e.g. 'lcr' instead of c('left', 'centre', 'right').}

\item{missing}{string to replace missing values}

\item{keep.line.breaks}{(default: \code{FALSE}) if to keep or remove line breaks from cells in a table}

\item{split.tables}{where to split wide tables to separate tables. The default value (\code{80}) suggests the conventional number of characters used in a line, feel free to change (e.g. to \code{Inf} to disable this feature) if you are not using a VT100 terminal any more :)}

\item{emphasize.rownames}{boolean (default: \code{TRUE}) if row names should be highlighted}

\item{...}{unsupported extra arguments directly placed into \code{/dev/null}}
}
\description{
Pander method to output a desctable
}
\details{
Uses \code{pandoc.table}, with some default parameters (\code{digits = 2}, \code{justify = "left"}, \code{missing = ""}, \code{keep.line.breaks = T}, \code{split.tables = Inf}, and \code{emphasize.rownames = F}), that you can override if needed.
}
\seealso{
\code{\link{pandoc.table}}
}
\keyword{deprecated}
