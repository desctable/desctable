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
  x[-1] -> df

  df %>%
    flatten_desctable %>%
    data.frame(row.names = x$Variables$Variables, check.names = F, ...)
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
  x$Variables$Variables <- gsub("\\+ (.*)", "**\\1**", x$Variables$Variables)
  x$Variables$Variables <- gsub("\\* (.*)", "- *\\1*", x$Variables$Variables)

  header <- x[-1] %>% header("pander")

  x[-1] %>%
    flatten_desctable %>%
    lapply(prettyNum, digits = digits, ...) %>%
    lapply(gsub, pattern = "^NA$", replacement = "") %>%
    data.frame(check.names = F, row.names = NULL, stringsAsFactors = F) %>%
    pander::pandoc.table(justify = justify, keep.line.breaks = T, split.tables = Inf, ...)
}

#' Datatable
#'
#' @inheritParams DT::datatable
#' @export
datatable <- function(data, options = list(), class = "display", callback = DT::JS("return table;"), rownames, colnames, container, caption = NULL, filter = c("none", "bottom", "top"), escape = TRUE, style = "default", width = NULL, height = NULL, elementId = NULL, fillContainer = getOption("DT.fillContainer", NULL), autoHideNavigation = getOption("DT.autoHideNavigation", NULL), selection = c("multiple", "single", "none"), extensions = list(), plugins = NULL)
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
#' @export
datatable.desctable <- function(data = NULL, ...)
{
  flatten_desctable(data) %>%
    lapply(prettyNum, ...) %>%
    lapply(gsub, pattern = "^NA$", replacement = "") %>%
    data.frame(check.names = F) %>%
    DT::datatable()
}
