#' Generate one statistic for all variables
#'
#' Use one stat function (made safe using statify) on all the data
#' to produce a single statistics column.
#'
#' The result is either a numeric vector, or a character vector if
#' the content of the column is not made entirely of numbers.
#'
#' @param stat The statistic to use
#' @param data The dataframe to apply the statistic to
#' @keywords internal
#' @return A vector for one statistic column
statColumn <- function(stat, data) {
  # Apply one statified stat function to every variable in the data
  # Return a simple vector for the column
  # Statify checks types and output for the stat function. Returns a numeric vector or a character vector if needed.
  if (length(stat) == 3)                                                        # remove after 1.0
    warning("Conditional formulas are deprecated and will be removed in 1.0.0

purrr::map style formulas are used now.
For example, `is.normal ~ mean | median` becomes `~ if (is.normal(.)) mean(.) else median(.)`")

  data %>%
    lapply(statify, stat) %>%
    unlist()
}


#' Generate the table of all statistics for all variables
#'
#' If stats is a list of functions or purrr::map like formulas, use them.
#' If it is a single function, use it with the entire data as
#' its argument to produce a list of statistical functions to use.
#'
#' @param data The dataframe to apply the statistic to
#' @param stats A list of named statistics to use
#' @keywords internal
#' @return A dataframe of all statistics for all variables
statTable <- function(data, stats) {
  # If stats is a function, apply it to the data to obtain a list of stat functions
  # Else use the function list as-is
  if (is.function(stats)) stats = stats(data)                                   # remove after 1.0

  # Compute a statColumn for every stat function in stats
  # Assemble the result in a dataframe
  stats %>%
    lapply(statColumn, data) %>%
    data.frame(check.names = F,
               row.names = NULL,
               stringsAsFactors = F)
}


#' Generate the variable column to display as row names
#'
#' Generates the variable column.
#' Replaces the variable names by their label if given in the named character vector labels, and inserts levels for factors.
#'
#' labels is an option named character vector used to make the table prettier.
#' If given, the variable names for which there is a label will be replaced by their corresponding label.
#' Not all variables need to have a label, and labels for non-existing variables are ignored.
#'
#' @param data The dataframe to get the names from
#' @param labels The optional named character vector containing the keypairs var = "Label"
#' @keywords internal
#' @return A dataframe with one variable named "Variables", a character vector of variable names/labels and levels
varColumn <- function(data, labels = NULL) {
  # Every variable name that exists in the labels is to be replaced with its corresponding label
  # Labels for non-existing variables are ignored
  # Variables with no label are not replaced and used as-is
  base_names <- names(data)
  base_names[base_names %in% names(labels)] <- labels[base_names[base_names %in% names(labels)]]

  # Check if there are factors
  data %>%
    lapply(is.factor) %>%
    unlist() -> factors

  # Insert levels for factors after the variable name
  if (any(factors)) {
    factors_idx <- which(factors)

    # Factor names in **bold**
    base_names[factors_idx] <- paste0("**", base_names[factors_idx], "**")

    # Factor levels in *italic*
    factor_levels <- lapply(factors_idx, function(x) paste0(base_names[x], ": ", "*", levels(data[[x]]), "*"))

    # Insert the factor levels after each factor name
    base_names <- insert(x = base_names,
                         y = factor_levels,
                         position = factors_idx)
  }

  data.frame(Variables = base_names,
             check.names = F,
             row.names = NULL,
             stringsAsFactors = F)
}


#' Create the pvalues column
#'
#' @param df Dataframe to use for the tests
#' @param tests Test function or list of functions
#' @param grp Grouping factor
#' @keywords internal
#' @return A numeric vector of pvalues
testColumn <- function(df, tests, grp) {
  group <- eval(grp, df)

  df <- df[!names(df) %in% as.character(grp)]

  # If tests is a function, apply it to the data and the grouping factor to produce a list of tests
  # If there is an .auto element in the list of tests, apply the function as previously to select the relevant test
  # If there is a .default element, use it as tests
  # Else fall back on kruskal.test
  if (is.function(tests)) {                                                     # remove after 1.0
    ftests <- lapply(df, tests, factor(group))
    tests <- ftests
  }  else if (!is.null(tests$.default)) ftests <- lapply(df, function(x){tests$.default})
  else if (!is.null(tests$.auto))       ftests <- lapply(df, tests$.auto, factor(group))
  else                                  ftests <- lapply(df, function(x){stats::kruskal.test})

  # Select the forced (named) tests
  tests %>%
    names() %>%
    setdiff(".auto") %>%
    intersect(names(df)) -> forced_tests

  # Assemble the complete list of tests to compute
  ftests[names(ftests) %in% forced_tests][forced_tests] <- tests[forced_tests]

  # Compute the tests (made safe with testify) on the variable, using the grouping variable
  mapply(testify, df, ftests, MoreArgs = list(group = group), SIMPLIFY = F) %>%
    Reduce(f = rbind)
}


#' Generate a statistics table
#'
#' Generate a statistics table with the chosen statistical functions, nested if called with a grouped dataframe.
#'
#' @section Stats:
#' The statistical functions to use in the table are passed as additional arguments.
#' If the argument is named (eg. \code{N = length}) the name will be used as the column title instead of the function
#' name (here, \strong{N} instead of \strong{length}).
#'
#' Any R function can be a statistical function, as long as it returns only one value when applied to a vector, or as
#' many values as there are levels in a factor, plus one.
#'
#' Users can also use \code{purrr::map}-like formulas as quick anonymous functions (eg. \code{Q1 = ~ quantile(., .25)} to get the first quantile in a
#' column named \strong{Q1})
#'
#' If no statistical function is given to \code{desc_table}, the \code{.auto} argument is used to provide a function
#' that automatically determines the most appropriate statistical functions to use based on the contents of the table.
#'
#' @section Labels:
#' \code{.labels} is a named character vector to provide "pretty" labels to variables.
#'
#' If given, the variable names for which there is a label will be replaced by their corresponding label.
#'
#' Not all variables need to have a label, and labels for non-existing variables are ignored.
#'
#' labels must be given in the form \code{c(unquoted_variable_name = "label")}
#'
#' @section Output:
#' The output is either a dataframe in the case of a simple descriptive table,
#' or nested dataframes in the case of a comparative table.
#'
#' @param data The dataframe to analyze
#' @param ...  A list of named statistics to apply to each element of the dataframe, or a function returning a list of named statistics
#' @param .auto A function to automatically determine appropriate statistics
#' @param .labels A named character vector of variable labels
#' @return A simple or grouped descriptive table
#' @seealso \code{\link{stats_auto}}
#' @seealso \code{\link{IQR}}
#' @seealso \code{\link{percent}}
#' @export
#' @family desc_table core functions
#' @examples
#' iris %>%
#'   desc_table()
#'
#' # Does the same as stats_auto here
#' iris %>%
#'   desc_table("N"      = length,
#'              "Min"    = min,
#'              "Q1"     = ~quantile(., .25),
#'              "Med"    = median,
#'              "Mean"   = mean,
#'              "Q3"     = ~quantile(., .75),
#'              "Max"    = max,
#'              "sd"     = sd,
#'              "IQR"    = IQR)
#'
#' # With grouping on a factor
#' iris %>%
#'   group_by(Species) %>%
#'   desc_table(.auto = stats_auto)
desc_table <- function(data, ..., .auto, .labels) {
  UseMethod("desc_table", data)
}


#' @rdname desc_table
#' @export
desc_table.default <- function(data, ..., .auto, .labels) {
  stop("`desc_table` must be called on a data.frame")
}


#' @rdname desc_table
#' @export
desc_table.data.frame <- function(data, ..., .labels = NULL, .auto = stats_auto) {

  stats <- rlang::dots_list(..., .named = T)

  if (length(stats) == 0 & is.null(.auto)) {
    stop("desc_table needs at least one statistic function, or an automatic function in .stats_auto")
  } else if (length(stats) == 0) {
    stats <- .auto(data)
  }

  # Assemble the Variables and the statTable in a single desctable object
  cbind(varColumn(data, .labels),
        statTable(data, stats))
}


#' @rdname desc_table
#' @export
desc_table.grouped_df <- function(data, ..., .auto = stats_auto, .labels = NULL) {
  # Get groups then ungroup dataframe
  grps <- dplyr::groups(data)

  if (length(grps) > 1) {
    warning("Only the first group will be used")
    data <- dplyr::ungroup(data, !!! grps[-1])
  }

  stats <- rlang::dots_list(..., .named = T)

  desctable <- tidyr::nest(data)

  if (length(stats) == 0 & is.null(.auto)) {
    stop("desc_table needs at least one statistic function, or an automatic function in .stats_auto")
  } else if (length(stats) == 0) {
    stats <- lapply(desctable$data, .auto)
  }

  if (is.list(stats[[1]])) {
    desctable$.stats <- mapply(statTable, desctable$data, stats, SIMPLIFY = F)
  } else {
    desctable$.stats <- lapply(desctable$data, statTable, stats)
  }

  desctable$.vars <- list(varColumn(data[!names(data) %in% (grps %>% lapply(as.character) %>% unlist())], .labels))

  desctable
}


#' Add tests to a desc_table
#'
#' Add test statistics to a grouped desc_table, with the tests specified as \code{variable = test}.
#'
#' @section Tests:
#' The statistical test functions to use in the table are passed as additional named arguments. Tests must be preceded
#' by a formula tilde (\code{~}).
#' \code{name = ~test} will apply test \code{test} to variable \code{name}.
#'
#' Any R test function can be used, as long as it returns an object containing a \code{p.value} element, which is the
#' case for most tests returning an object of class  \code{htest}.
#'
#' Users can also use \code{purrr::map}-like formulas as quick anonymous functions (eg. \code{~ t.test(., var.equal = T)} to
#' compute a t test without the Welch correction.
#'
#' @param desctable A desc_table
#' @param ... A list of statistical tests associated to variable names
#' @param .auto A function to automatically determine the appropriate tests
#' @param .default A default fallback test
#' @seealso \code{\link{tests_auto}}
#' @seealso \code{\link{no.test}}
#' @seealso \code{\link{ANOVA}}
#' @return A desc_table with tests
#' @export
#' @family desc_table core functions
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   desc_table() %>%
#'   desc_tests(Sepal.Length = ~kruskal.test,
#'              Sepal.Width  = ~oneway.test,
#'              Petal.Length = ~oneway.test(., var.equal = T),
#'              Petal.Length = ~oneway.test(., var.equal = F))
desc_tests <- function(desctable, .auto = tests_auto, .default = NULL, ...) {
  if (which.desctable(desctable) != "grouped")
    stop("Unexpected input. `desc_tests` must be used on the output of `desc_table` on a grouped dataframe.\n
For example: iris %>% group_by(Species) %>% desc_table() %>% desc_tests")

  fulldata <- tidyr::unnest(desctable, "data")
  fulldata$.tests <- NULL
  fulldata$.stats <- NULL
  fulldata$.vars <- NULL

  tests <- list(...)

  if (!(all(names(desctable$data[[1]]) %in% names(tests))) & is.null(.auto) & is.null(.default)) {
    stop("desc_tests needs either a full specification of tests, or include a .auto or a .default function for non specified-tests")
  } else {
    tests <- c(list(...), list(.auto = .auto, .default = .default))
  }

  desctable$.tests <- list(testColumn(fulldata, tests, as.symbol(names(desctable)[1])))

  desctable
}
