#' Print method for desctable
#'
#' @param x A desctable
#' @param ... Additional print parameters
#' @return A flat dataframe
#' @export
print.desctable <- function(x, ...)
{
  print(x %>% as.data.frame)
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

  header <- x %>% header("dataframe")

  x[-1] -> df

  df %>%
    flatten_desctable %>%
    data.frame(row.names = x$Variables$Variables, check.names = F, ...) %>%
    stats::setNames(header)
}

#' Pander method for desctable
#'
#' @param x A desctable
#' @inheritParams pander::pandoc.table
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
  if (is.null(digits))
    digits <- pander::panderOptions("digits")

  x$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "\u00A0\u00A0\u00A0\u00A0\\2", x$Variables$Variables)

  header <- x %>% header("pander")

  x[-1] %>%
    flatten_desctable %>%
    data.frame(check.names = F, row.names = x$Variables$Variables, stringsAsFactors = F) %>%
    stats::setNames(header) %>%
    pander::pandoc.table(justify = justify,
                         digits = digits,
                         missing = missing,
                         keep.line.breaks = keep.line.breaks,
                         split.tables = split.tables,
                         emphasize.rownames = emphasize.rownames,
                         ...)
}

#' Datatable
#'
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

#' datatable method for desctable
#'
#' @param data A desctable
#' @param ... Additional datatable parameters
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
                                digits = 2, ...)
{
  data$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "\u00A\u00A\u00A\u00A0\\2", data$Variables$Variables)
  data$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*", "<b>\\1</b>", data$Variables$Variables)

  header <- data %>% header("datatable")

  data[-1] %>%
  flatten_desctable %>%
    lapply(prettyNum, digits = digits, ...) %>%
    lapply(gsub, pattern = "^NA$", replacement = "") %>%
    data.frame(check.names = F, row.names = data$Variables$Variables, stringsAsFactors = F) %>%
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
                  plugins = plugins, ...)
}
