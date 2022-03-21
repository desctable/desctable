#' @importFrom pander pander
pander::pander

#' Generate a statistics table
#'
#' Generate a statistics table with the chosen statistical functions, and tests if given a \code{"grouped"} dataframe.
#'
#' @section Labels:
#' labels is an option named character vector used to make the table prettier.
#'
#' If given, the variable names for which there is a label will be replaced by their corresponding label.
#'
#' Not all variables need to have a label, and labels for non-existing variables are ignored.
#'
#' labels must be given in the form c(unquoted_variable_name = "label")
#'
#' @section Stats:
#' The stats can be a function which takes a dataframe and returns a list of statistical functions to use.
#'
#' stats can also be a named list of statistical functions, or purrr::map like formulas.
#'
#' The names will be used as column names in the resulting table. If an element of the list is a function, it will be used as-is for the stats.
#'
#' @section Tests:
#' The tests can be a function which takes a variable and a grouping variable, and returns an appropriate statistical test to use in that case.
#'
#' tests can also be a named list of statistical test functions, associating the name of a variable in the data and a test to use specifically for that variable.
#'
#' That test name must be expressed as a single-term formula (e.g. \code{~t.test}), or a purrr::map like formula
#' (e.g. \code{~t.test(., var.equal = T)}). You don't have to specify tests for all the variables: a default test for
#' all other variables can be defined with the name \code{.default}, and an automatic test can be defined with the name \code{.auto}.
#'
#' If data is a grouped dataframe (using \code{group_by}), subtables are created and statistic tests are performed over each sub-group.
#'
#' @section Output:
#' The output is a desctable object, which is a list of named dataframes that can be further manipulated. Methods for printing, using in \pkg{pander} and \pkg{DT} are present. Printing reduces the object to a dataframe.
#'
#' @param data The dataframe to analyze
#' @param stats A list of named statistics to apply to each element of the dataframe, or a function returning a list of named statistics
#' @param tests A list of statistical tests to use when calling desctable with a grouped_df
#' @param labels A named character vector of labels to use instead of variable names
#' @return A desctable object, which prints to a table of statistics for all variables
#' @seealso \code{\link{stats_auto}}
#' @seealso \code{\link{tests_auto}}
#' @seealso \code{\link{print.desctable}}
#' @seealso \code{\link{pander.desctable}}
#' @seealso \code{\link{datatable.desctable}}
#' @export
#' @examples
#' iris %>%
#'   desctable()
#'
#' # Does the same as stats_auto here
#' iris %>%
#'   desctable(stats = list("N"      = length,
#'                          "Mean"   = ~ if (is.normal(.)) mean(.),
#'                          "sd"     = ~ if (is.normal(.)) sd(.),
#'                          "Med"    = stats::median,
#'                          "IQR"    = ~ if(!is.factor(.)) IQR(.)))
#'
#' # With labels
#' mtcars %>% desctable(labels = c(hp  = "Horse Power",
#'                                 cyl = "Cylinders",
#'                                 mpg = "Miles per gallon"))
#'
#' # With grouping on a factor
#' iris %>%
#'   group_by(Species) %>%
#'   desctable(stats = stats_default)
#'
#' # With nested grouping, on arbitrary variables
#' mtcars %>%
#'   group_by(vs, cyl) %>%
#'   desctable()
#'
#' # With grouping on a condition, and choice of tests
#' iris %>%
#'   group_by(Petal.Length > 5) %>%
#'   desctable(tests = list(.auto = tests_auto, Species = ~chisq.test))
desctable <- function(data, stats, tests, labels) {
  warning("desctable is deprecated and will be removed in 1.0.0.

Please use the `desc_*` family of functions (`desc_table`, `desc_tests`, `desc_output`)")
  UseMethod("desctable", data)
}


#' @rdname desctable
#' @export
desctable.default <- function(data, stats = stats_auto, tests, labels = NULL) {
  # Assemble the Variables and the statTable in a single desctable object
  list(Variables = varColumn(data, labels),
       stats = statTable(data, stats)) %>%
  set_desctable_class()
}


#' @rdname desctable
#' @export
desctable.grouped_df <- function(data, stats = stats_auto, tests = tests_auto, labels = NULL) {
  # Get groups then ungroup dataframe
  grps <- dplyr::groups(data)
  data <- dplyr::ungroup(data)

  # Assemble the Variables (excluding the grouping ones) and the subTables recursively in a single desctable object
  c(Variables = list(varColumn(data[!names(data) %in% (grps %>% lapply(as.character) %>% unlist())], labels)),
    subTable(data, stats, tests, grps)) %>%
    set_desctable_class()
}


#' Create the subtables names
#'
#' Create the subtables names, as
#' factor: level (n=sub-group length)
#'
#' @param grp Grouping factor
#' @param df Dataframe containing the grouping factor
#' @return A character vector with the names for the subtables
subNames <- function(grp, df) {
  paste0(as.character(grp),
         ": ",
         eval(grp, df) %>% factor() %>% levels(),
         " (n=",
         summary(eval(grp, df) %>% factor() %>% stats::na.omit(), maxsum = Inf),
         ")")
}


#' Create a subtable in a grouped desctable
#'
#' @param df Dataframe to use
#' @param stats Stats list/function to use
#' @param tests Tests list/function to use
#' @param grps List of symbols for grouping factors
#' @return A nested list of statTables and testColumns
subTable <- function(df, stats, tests, grps) {
  # Final group, compute tests
  if (length(grps) == 1) {
    group <- factor(eval(grps[[1]], df))

    # Create the subtable stats
    df[!names(df) %in% as.character(grps[[1]])] %>%
      by(group, statTable, stats) %>%
      # Name the subtables with info about group and group size
      stats::setNames(subNames(grps[[1]], df)) -> stats

    # Create the subtable tests
    pvalues <- testColumn(df, tests, grps[[1]])

    c(stats, tests = list(pvalues))
  } else {
    group <- eval(grps[[1]], df)

    # Go through the next grouping levels and build the subtables
    df[!names(df) %in% as.character(grps[[1]])] %>%
      by(group, subTable, stats, tests, grps[-1]) %>%
      # Name the subtables with info about group and group size
      stats::setNames(subNames(grps[[1]], df))
  }
}


#' Print method for desctable
#'
#' @param x A desctable
#' @param ... Additional print parameters
#' @return A flat dataframe
#' @export
print.desctable <- function(x, ...) {
  print(as.data.frame(x))
}


#' As.data.frame method for desctable
#'
#' @param x A desctable
#' @param ... Additional as.data.frame parameters
#' @return A flat dataframe
#' @export
as.data.frame.desctable <- function(x, ...) {
  # Discard "markdown" formatting of variable names
  x$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*", "\\1", x$Variables$Variables)
  x$Variables$Variables <- gsub("\\*(.*?)\\*", "\\1", x$Variables$Variables)

  # Create a dataframe header
  header <- header(x, "dataframe")

  # Make a standard dataframe
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
                             ...) {
  if (is.null(digits)) digits <- pander::panderOptions("digits")

  # Discard "markdown" and insert 4 NbSp before factor levels
  x$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "&nbsp;&nbsp;&nbsp;&nbsp;\\2", x$Variables$Variables)

  # Create a pander header
  header <- header(x, "pander")

  # Make a dataframe and push it to pandoc
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
#' See \url{https://rstudio.github.io/DT/} for the full documentation.
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
datatable <- function(data, ...) {
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
                              plugins = NULL, ...) {
  DT::datatable(data, options = options, class = class, callback = callback, caption = caption, filter = filter, escape = escape, style = style, width = width, height = height, elementId = elementId, fillContainer = fillContainer, autoHideNavigation = autoHideNavigation, selection = selection, extensions = extensions, plugins = plugins, ...)
}


#' @rdname datatable
#' @inheritParams base::prettyNum
#' @export
datatable.desctable <- function(data,
                                options = list(paging = F,
                                               info = F,
                                               search = list(),
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
                                digits = 2, ...) {
  # Discard "markdown" and insert 4 NbSp before factor levels
  data$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "&nbsp;&nbsp;&nbsp;&nbsp;\\2", data$Variables$Variables)
  data$Variables$Variables <- gsub("\\*\\*(.*?)\\*\\*", "<b>\\1</b>", data$Variables$Variables)

  # Create a datatable header
  header <- header(data, "datatable")

  # Flatten desctable
  flat <- flatten_desctable(data)

  # Replace NAs and apply digits arg
  if (!is.null(digits))
  {
    flat %>%
      lapply(prettyNum, digits = digits) %>%
      lapply(gsub, pattern = "^NA$", replacement = "") -> flat
  }

  # Make a dataframe and push it to datatable, with its custom header
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

#' Set the "desctable" class to the passed object
#'
#' @param x Object to set the "desctable" class to
#' @return The object with the class "desctable"
set_desctable_class <- function(x) {
  class(x) <- "desctable"

  x
}


#' Parse a formula
#'
#' Parse a formula defining the conditions to pick a stat/test
#'
#' Parse a formula defining the conditions to pick a stat/test
#' and return the function to use.
#' The formula is to be given in the form of
#' conditional ~ T | F
#' and conditions can be nested such as
#' conditional1 ~ (conditional2 ~ T | F) | F
#' The FALSE option can be omitted, and the TRUE can be replaced with NA
#'
#' @param x The variable to test it on
#' @param f A formula to parse
#' @return A function to use as a stat/test
parse_formula <- function(x, f) {
  parse_f <- function(x) {
    if (length(x) == 1) as.character(x)
    else {
      if (as.character(x[[1]]) == "~") {
        paste0("if (", parse_f(x[[2]]), "(x)) ",
               "{",
               parse_f(x[[3]]),
               "}")
      } else if (as.character(x[[1]]) == "|") {
        paste0(parse_f(x[[2]]),
               "} else ",
               "{",
               parse_f(x[[3]]))
      } else if (as.character(x[[1]]) == "(") {
        parse_f(x[[2]])
      }
    }
  }

  eval(parse(text = parse_f(f)))
}


#' Build the header for pander
#'
#' @param head A headerList object
#' @return A names vector
head_pander <- function(head) {
  if (is.integer(head[[1]])) {
    head %>%
      names %>%
      lapply(function(x){c(x, rep("", head[[x]] - 1))}) %>%
      unlist()
  } else {
    paste(head %>%
            names() %>%
            lapply(function(x){c(x, rep("", attr(head[[x]], "colspan") - 1))}) %>%
            unlist(),
          head %>%
            lapply(head_pander) %>%
            unlist(),
          sep = "<br/>")
  }
}


#' Build the header for datatable
#'
#' @param head A headerList object
#' @return An htmltools$tags object containing the header
head_datatable <- function(head) {
  TRs <- list()

  while (is.list(head[[1]])) {
    TR <- mapply(function(x, y) htmltools::tags$th(x, colspan = y), names(head), lapply(head, attr, "colspan"), SIMPLIFY = F)

    TRs <- c(TRs, list(TR))
    head <- purrr::flatten(head)
  }

  c(TRs, list(mapply(function(x, y) htmltools::tags$th(x, colspan = y), names(head), head, SIMPLIFY = F)))
}


#' Build the header for dataframe
#'
#' @param head A headerList object
#' @return A names vector
head_dataframe <- function(head) {
  if (is.integer(head[[1]])) {
    head %>%
      names() %>%
      lapply(function(x){rep(x, head[[x]])}) %>%
      unlist()
  } else {
    paste(head %>%
            names() %>%
            lapply(function(x){rep(x, attr(head[[x]], "colspan"))}) %>%
            unlist(),
          head %>%
            lapply(head_pander) %>%
            unlist(),
          sep = " / ")
  }
}


#' Build header
#'
#' Take a desctable object and create a suitable header for the mentionned output.
#' Output can be one of "pander", "datatable", or "dataframe".
#'
#' @param desctable A desctable object
#' @param output An output format for the header
#' @return A header object in the output format
header <- function(desctable, output = c("pander", "datatable", "dataframe")) {
  desctable[-1] %>%
    flatten_desctable() %>%
    data.frame(check.names = F) %>%
    names() -> nm

  desctable <- desctable[-1]

  if (length(desctable) == 1) {
    if (output == "datatable") {
      c("\u00A0", nm) %>%
        lapply(htmltools::tags$th) %>%
        htmltools::tags$tr() %>%
        htmltools::tags$thead() %>%
        htmltools::tags$table(class = "display")
    } else c("\u00A0", nm)
  } else {
    head <- headerList(desctable)

    if (output == "pander") {
      c("\u00A0", head_pander(head) %>%
        paste(nm, sep = "<br/>"))
    } else if (output == "datatable") {
      head <- c(head_datatable(head), list(nm %>% lapply(htmltools::tags$th)))
      head[[1]] <- c(list(htmltools::tags$th(rowspan = length(head))), head[[1]])

      head %>%
        lapply(htmltools::tags$tr) %>%
        htmltools::tags$thead() %>%
        htmltools::tags$table(class = "display")
    } else if (output == "dataframe") {
      c("\u00A0", head_dataframe(head) %>% paste(nm, sep = " / "))
    }
  }
}


#' build a header list object
#'
#' @param desctable a desctable
#' @return a nested list of headers with colspans
headerList <- function(desctable) {
  if (is.data.frame(desctable)) length(desctable)
  else {
    rec <- lapply(desctable, headerList)

    if (is.integer(rec[[1]])) attr(rec, "colspan") <- rec %>% unlist() %>% sum()
    else                      attr(rec, "colspan") <- rec %>% lapply(attr, "colspan") %>% unlist() %>% sum()

    rec
  }
}


#' Flatten a desctable to a dataframe recursively
#'
#' @param desctable A desctable object
#' @return A flat dataframe
flatten_desctable <- function(desctable) {
  if (is.data.frame(desctable)) desctable
  else {
    desctable %>%
      lapply(flatten_desctable) %>%
      Reduce(f = cbind)
  }
}

#' Define a list of default statistics
#'
#' @param data A dataframe
#' @return A list of statistical functions
#' @export
#' @keywords deprecated
stats_default <- function(data) {
  list("N" = length,
       "%" = percent,
       "Mean" = ~if (is.normal(.)) mean(.),
       "sd" = ~if (is.normal(.)) sd(.),
       "Med" = stats::median,
       "IQR" = ~if (!is.factor(.)) IQR(.))
}


#' @rdname stats_default
#' @export
stats_normal <- function(data) {
  list("N" = length,
       "%" = percent,
       "Mean" = mean,
       "sd" = stats::sd)
}


#' @rdname stats_default
#' @export
stats_nonnormal <- function(data) {
  list("N" = length,
       "%" = percent,
       "Median" = stats::median,
       "IQR" = ~if (!is.factor(.)) IQR(.))
}
