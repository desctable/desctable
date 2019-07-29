#' Print method for desctable
#'
#' @param x A desctable
#' @param ... Additional print parameters
#' @return A flat dataframe
#' @export
print.desctable <- function(x, ...)
{
  print(as.data.frame(x))
}


#' As.data.frame method for desctable
#'
#' @param x A desctable
#' @param ... Additional as.data.frame parameters
#' @return A flat dataframe
#' @export
as.data.frame.desctable <- function(x, ...)
{
  x$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*", "\\1", x$Variables$Variables)
  x$Variables$Variables <- gsub("\\*(.*?)\\*", "\\1", x$Variables$Variables)

  header <- header(x, "dataframe")

  x %>%
    flatten_desctable() %>%
    data.frame(check.names = F, ...) %>%
    stats::setNames(header)
}


#' Pander method for desctable
#'
#' Pander method to output a desctable
#'
#' Uses \code{pandoc.table}, with some default parameters (\code{digits = 2}, \code{justify = "left"}, \code{missing = ""}, \code{keep.line.breaks = T}, \code{split.tables = Inf}, and \code{emphasize.rownames = F}), that you can override if needed.
#'
#' @param x A desctable
#' @inheritParams pander::pandoc.table
#' @seealso \code{\link{pandoc.table}}
#' @export
pander.desctable <- function(x = NULL,
                             digits = 2,
                             justify = "left",
                             missing = "",
                             keep.line.breaks = T,
                             split.tables = Inf,
                             emphasize.rownames = F,
                             ...)
{
  if (is.null(digits)) digits <- pander::panderOptions("digits")

  x$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "&nbsp;&nbsp;&nbsp;&nbsp;\\2", x$Variables$Variables)

  header <- header(x, "pander")

  x %>%
    flatten_desctable %>%
    data.frame(check.names = F, stringsAsFactors = F) %>%
    stats::setNames(header) %>%
    pander::pandoc.table(justify = justify,
                         digits = digits,
                         missing = missing,
                         keep.line.breaks = keep.line.breaks,
                         split.tables = split.tables,
                         emphasize.rownames = emphasize.rownames,
                         ...)
}


#' Create an HTML table widget using the DataTables library
#'
#' This function creates an HTML widget to display rectangular data (a matrix or data frame) using the JavaScript library DataTables, with a method for \code{desctable} objects.
#'
#' @note
#' You are recommended to escape the table content for security reasons (e.g. XSS attacks) when using this function in Shiny or any other dynamic web applications.
#' @references
#' See \url{http://rstudio.github.io/DT} for the full documentation.
#' @examples
#' library(DT)
#'
#' # see the package vignette for examples and the link to website
#' vignette('DT', package = 'DT')
#'
#' # some boring edge cases for testing purposes
#' m = matrix(nrow = 0, ncol = 5, dimnames = list(NULL, letters[1:5]))
#' datatable(m)  # zero rows
#' datatable(as.data.frame(m))
#'
#' m = matrix(1, dimnames = list(NULL, 'a'))
#' datatable(m)  # one row and one column
#' datatable(as.data.frame(m))
#'
#' m = data.frame(a = 1, b = 2, c = 3)
#' datatable(m)
#' datatable(as.matrix(m))
#'
#' # dates
#' datatable(data.frame(
#'   date = seq(as.Date("2015-01-01"), by = "day", length.out = 5), x = 1:5
#' ))
#' datatable(data.frame(x = Sys.Date()))
#' datatable(data.frame(x = Sys.time()))
#'
#' ###
#' @inheritParams DT::datatable
#' @export
datatable <- function(data, ...)
{
  UseMethod("datatable", data)
}


#' @rdname datatable
#' @export
datatable.default <- function(data,
                              options = list(),
                              class = "display",
                              callback = DT::JS("return table;"),
                              caption = NULL,
                              filter = c("none", "bottom", "top"),
                              escape = TRUE,
                              style = "default",
                              width = NULL,
                              height = NULL,
                              elementId = NULL,
                              fillContainer = getOption("DT.fillContainer", NULL),
                              autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
                              selection = c("multiple", "single", "none"),
                              extensions = list(),
                              plugins = NULL, ...)
{
  DT::datatable(data, options = options, class = class, callback = callback, caption = caption, filter = filter, escape = escape, style = style, width = width, height = height, elementId = elementId, fillContainer = fillContainer, autoHideNavigation = autoHideNavigation, selection = selection, extensions = extensions, plugins = plugins, ...)
}


#' @rdname datatable
#' @inheritParams base::prettyNum
#' @export
datatable.desctable <- function(data,
                                options = list(paging = F,
                                               info = F,
                                               search = F,
                                               dom = "Brtip",
                                               fixedColumns = T,
                                               fixedHeader = T,
                                               buttons = c("copy", "excel")),
                                class = "display",
                                callback = DT::JS("return table;"),
                                caption = NULL,
                                filter = c("none", "bottom", "top"),
                                escape = FALSE,
                                style = "default",
                                width = NULL,
                                height = NULL,
                                elementId = NULL,
                                fillContainer = getOption("DT.fillContainer", NULL),
                                autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
                                selection = c("multiple", "single", "none"),
                                extensions = c("FixedHeader", "FixedColumns", "Buttons"),
                                plugins = NULL,
                                rownames = F,
                                digits = 2, ...)
{
  data$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "&nbsp;&nbsp;&nbsp;&nbsp;\\2", data$Variables$Variables)
  data$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*", "<b>\\1</b>", data$Variables$Variables)

  header <- header(data, "datatable")

  flat <- flatten_desctable(data)

  if (!is.null(digits))
  {
    flat %>%
      lapply(prettyNum, digits = digits) %>%
      lapply(gsub, pattern = "^NA$", replacement = "") -> flat
  }

  flat %>%
    data.frame(check.names = F, stringsAsFactors = F) %>%
    DT::datatable(container = header,
                  options = options,
                  extensions = extensions,
                  escape = escape,
                  class = class,
                  callback = callback,
                  caption = caption,
                  filter = filter,
                  style = style,
                  width = width,
                  height = height,
                  elementId = elementId,
                  fillContainer = fillContainer,
                  autoHideNavigation = autoHideNavigation,
                  selection = selection,
                  plugins = plugins,
                  rownames = rownames, ...)
}
