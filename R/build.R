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
#' @return A vector for one statistic column
statColumn <- function(stat, data)
{
  # Apply one statified stat function to every variable in the data
  # Return a simple vector for the column
  # Statify checks types and output for the stat function. Returns a numeric vector or a character vector if needed.
  data %>%
    lapply(statify, stat) %>%
    unlist()
}


#' Generate the table of all statistics for all variables
#'
#' If stats is a list of functions, use them.
#' If it is a single function, use it with the entire data as
#' its argument to produce a list of statistical functions to use.
#'
#' @param data The dataframe to apply the statistic to
#' @param stats A list of named statistics to use
#' @return A dataframe of all statistics for all variables
statTable <- function(data, stats)
{
  # If stats is a function, apply it to the data to obtain a list of stat functions
  # Else use the function list as-is
  if (is.function(stats)) stats = stats(data)

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
#' @return A dataframe with one variable named "Variables", a character vector of variable names/labels and levels
varColumn <- function(data, labels = NULL)
{
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
  if (any(factors))
  {
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
#' stats can also be a named list of statistical functions, or formulas.
#'
#' The names will be used as column names in the resulting table. If an element of the list is a function, it will be used as-is for the stats. If an element of the list is a formula, it can be used to conditionally use stats depending on the variable.
#'
#' The general form is \code{condition ~ T | F}, and can be nested, such as \code{is.factor ~ percent | (is.normal ~ mean | median)}, for example.
#'
#' @section Tests:
#' The tests can be a function which takes a variable and a grouping variable, and returns an appropriate statistical test to use in that case.
#'
#' tests can also be a named list of statistical test functions, associating the name of a variable in the data, and a test to use specifically for that variable.
#'
#' That test name must be expressed as a single-term formula (e.g. \code{~t.test}). You don't have to specify tests for all the variables: a default test for all other variables can be defined with the name \code{.default}, and an automatic test can be defined with the name \code{.auto}.
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
#'   desctable
#'
#' # Does the same as stats_auto here
#' iris %>%
#'   desctable(stats = list("N"      = length,
#'                          "%/Mean" = is.factor ~ percent | (is.normal ~ mean),
#'                          "sd"     = is.normal ~ sd,
#'                          "Med"    = is.normal ~ NA | median,
#'                          "IQR"    = is.normal ~ NA | IQR))
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
desctable <- function(data, stats, tests, labels)
{
  UseMethod("desctable", data)
}


#' @rdname desctable
#' @export
desctable.default <- function(data, stats = stats_auto, tests, labels = NULL)
{
  # Assemble the Variables and the statTable in a single desctable object
  list(Variables = varColumn(data, labels),
       stats = statTable(data, stats)) %>%
  set_desctable_class()
}


#' @rdname desctable
#' @export
desctable.grouped_df <- function(data, stats = stats_auto, tests = tests_auto, labels = NULL)
{
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
subNames <- function(grp, df)
{
  paste0(as.character(grp),
         ": ",
         eval(grp, df) %>% factor() %>% levels(),
         " (n=",
         summary(eval(grp, df) %>% factor() %>% stats::na.omit(), maxsum = Inf),
         ")")
}


#' Create the pvalues column
#'
#' @param df Dataframe to use for the tests
#' @param tests Test function or list of functions
#' @param grp Grouping factor
#' @return A numeric vector of pvalues
testColumn <- function(df, tests, grp)
{
  group <- eval(grp, df)

  df <- df[!names(df) %in% as.character(grp)]

  # If tests is a function, apply it to the data and the grouping factor to produce a list of tests
  # If there is an .auto element in the list of tests, apply the function as previously to select the relevant test
  # If there is a .default element, use it as tests
  # Else fall back on kruskal.test
  if (is.function(tests))
  {
    ftests <- lapply(df, tests, factor(group))
    tests <- ftests
  } else if (!is.null(tests$.auto)) ftests <- lapply(df, tests$.auto, factor(group))
  else if (!is.null(tests$.default)) ftests <- lapply(df, function(x){tests$.default})
  else ftests <- lapply(df, function(x){stats::kruskal.test})

  # Select the forced (named) tests
  tests %>%
    names() %>%
    setdiff(".auto") %>%
    intersect(names(df)) -> forced_tests

  # Assemble the complete list of tests to compute
  ftests[names(ftests) %in% forced_tests][forced_tests] <- tests[forced_tests]

  # Compute the tests (made safe with testify) on the variable, using the grouping variable
  df %>%
    purrr::map2(ftests, testify, group) %>%
    Reduce(f = cbind)
}


#' Create a subtable in a grouped desctable
#'
#' @param df Dataframe to use
#' @param stats Stats list/function to use
#' @param tests Tests list/function to use
#' @param grps List of symbols for grouping factors
#' @return A nested list of statTables and testColumns
subTable <- function(df, stats, tests, grps)
{
  # Final group, compute tests
  if (length(grps) == 1)
  {
    group <- factor(eval(grps[[1]], df))

    # Create the subtable stats
    df[!names(df) %in% as.character(grps[[1]])] %>%
      by(group, statTable, stats) %>%
      # Name the subtables with info about group and group size
      stats::setNames(subNames(grps[[1]], df)) -> stats

    # Create the subtable tests
    pvalues <- testColumn(df, tests, grps[[1]])

    c(stats, tests = list(pvalues))
  }
  else
  {
    group <- eval(grps[[1]], df)

    # Go through the next grouping levels and build the subtables
    df[!names(df) %in% as.character(grps[[1]])] %>%
      by(group, subTable, stats, tests, grps[-1]) %>%
      # Name the subtables with info about group and group size
      stats::setNames(subNames(grps[[1]], df))
  }
}
