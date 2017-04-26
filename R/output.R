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
#' @param ... Additional pander parameters
#' @inheritParams pander::pandoc.table
#' @inheritParams base::prettyNum
#' @export
pander.desctable <- function(x = NULL, digits = 2, justify = "left", ...)
{
  header <- x %>% header("pander")

  x[-1] %>%
    flatten_desctable %>%
    lapply(prettyNum, digits = digits, ...) %>%
    lapply(gsub, pattern = "^NA$", replacement = "") %>%
    data.frame(check.names = F, row.names = x$Variables$Variables, stringsAsFactors = F) %>%
    stats::setNames(header) %>%
    pander::pandoc.table(justify = justify, keep.line.breaks = T, split.tables = Inf, emphasize.rownames = F, ...)
}

#' Datatable
#'
#' @inheritParams DT::datatable
#' @export
datatable <- function(data, digits = 2, options = list(), class = "display", callback = DT::JS("return table;"), rownames, colnames, container, caption = NULL, filter = c("none", "bottom", "top"), escape = TRUE, style = "default", width = NULL, height = NULL, elementId = NULL, fillContainer = getOption("DT.fillContainer", NULL), autoHideNavigation = getOption("DT.autoHideNavigation", NULL), selection = c("multiple", "single", "none"), extensions = list(), plugins = NULL)
{
  UseMethod("datatable")
}

#' @rdname datatable
#' @export
datatable.default <- function(data, ...)
{
  DT::datatable(data, ...)
}

#' datatable method for desctable
#'
#' @param data A desctable
#' @param ... Additional datatable parameters
#' @rdname datatable
#' @inheritParams base::prettyNum
#' @export
#' @examples
#' iris %>%
#'   desctable %>%
#'   datatable()

datatable.desctable <- function(data = NULL, digits = 2, ...)
{
  data$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*", "<b>\\1</b>", data$Variables$Variables)
  data$Variables$Variables <- gsub("\\*(.*?)\\*", "<i>\\1</i>", data$Variables$Variables)

  header <- data %>% header("datatable")

  data[-1] %>%
  flatten_desctable %>%
    lapply(prettyNum, digits = digits, ...) %>%
    lapply(gsub, pattern = "^NA$", replacement = "") %>%
    data.frame(check.names = F, row.names = data$Variables$Variables, stringsAsFactors = F) %>%
    DT::datatable(container = header,
                  options = list(paging = F,
                                 info = F,
                                 search = F,
                                 dom = "Brtip",
                                 fixedColumns = T,
                                 fixedHeader = T,
                                 buttons = c("copy", "excel")),
                  extensions = c("FixedHeader", "FixedColumns", "Buttons"),
                  escape = F)
}
