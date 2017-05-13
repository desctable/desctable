#' Generate one statistic for all variables
#'
#' @param stat The statistic to use
#' @param data The dataframe to apply the statistic to
#' @return A vector for one statistic column
statColumn <- function(stat, data)
{
  data %>%
    lapply(statify, stat) %>%
    unlist
}

#' Generate the table of all statistics for all variables
#'
#' @param data The dataframe to apply the statistic to
#' @param stats A list of named statistics to use
#' @return A dataframe of all statistics for all variables
statTable <- function(data, stats)
{
  # Call the stats arg_function passed, or use the provided list as-is
  if (is.function(stats))
    stats = stats(data)

  stats %>%
    lapply(statColumn, data) %>%
    data.frame(check.names = F, row.names = NULL, stringsAsFactors = F)
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
# Replace variable names by their labels, if they exist
  names(data) -> base_names
  base_names[base_names %in% names(labels)] <- labels[base_names[base_names %in% names(labels)]]

  # Insert levels for factors after the variable name
  if (any(data %>% lapply(is.factor) %>% unlist))
  {
    data %>%
      lapply(is.factor) %>%
      unlist %>%
      which -> factors_idx

    base_names[factors_idx] <- paste0("**", base_names[factors_idx], "**")
    factor_levels <-
      factors_idx %>%
      lapply(function(x)
             {
                paste0(base_names[x],
                       ": ",
                       "*",
                       levels(data[[x]]),
                       "*")
             })

    insert(x = base_names,
           y = factor_levels,
           position = factors_idx) -> base_names
  }

  data.frame(Variables = base_names, check.names = F, row.names = NULL, stringsAsFactors = F)
}

#' Generate a statistics table
#'
#' Generate a statistics table with variable names/labels and levels
#'
#' labels is an option named character vector used to make the table prettier.
#' If given, the variable names for which there is a label will be replaced by their corresponding label.
#' Not all variables need to have a label, and labels for non-existing variables are ignored.
#' labels must be given in the form c(unquoted_variable_name = "label")
#'
#' The stats can be a function which takes a dataframe and returns a list of statistical functions to use.
#' stats can also be a named list of statistical functions, or formulas. The names will be used as column names in the resulting table. If an element of the list is a function, it will be used as-is for the stats. If an element of the list is a formula, it can be used to conditionally use stats depending on the variable. The general form is `condition ~ T | F`, and can be nested, such as `is.factor ~ percent | (is.normal ~ mean | median)`, for example.
#'
#' The tests can be a function which takes a variable and a grouping variable, and returns an appropriate statistical test to use in that case.
#' tests can also be a named list of statistical test functions, associating the name of a variable in the data, and a test to use specifically for that variable. That test name must be expressed as a single-term formula (e.g. ~t.test). You don't have to specify tests for all the variables: a default test for all other variables can be defined with the name .default, and an automatic test can be defined with the name .auto.
#'
#' If data is a grouped dataframe (using group_by), subtables are created and statistic tests are performed over each sub-group.
#'
#' The output is a desctable object, which is a list of named dataframes that can be further manipulated. Methods for printing, using in pander and DT::datatable are present. Printing reduces the object to a dataframe.
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
#'   desctable
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
  # Build the complete table
  list(Variables = varColumn(data, labels),
    stats = statTable(data, stats)) %>%
  `class<-`("desctable")
}

#' @rdname desctable
#' @export
desctable.grouped_df <- function(data, stats = stats_auto, tests = tests_auto, labels = NULL)
{
  # Get groups then ungroup dataframe
  grps <- data %>% dplyr::groups()
  data <- dplyr::ungroup(data)

  # Build the complete table recursively, assign "desctable" class
  c(Variables = list(varColumn(data[!names(data) %in% (grps %>% lapply(as.character) %>% unlist)], labels)),
    subTable(data, stats, tests, grps)) %>%
  `class<-`("desctable")
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
         eval(grp, df) %>% factor %>% levels,
         " (n=",
         summary(eval(grp, df) %>% factor %>% stats::na.omit(), maxsum = Inf),
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

  df <- df %>%
      dplyr::select(- eval(grp))

  if (is.function(tests))
  {
    ftests <- df %>%
      lapply(tests, group %>% factor)
    tests <- ftests
  } else if (!is.null(tests$.auto))
  {
    ftests <- df %>%
      lapply(tests$.auto, group %>% factor)
  } else if (!is.null(tests$.default))
  {
    ftests <- df %>%
      lapply(function(x){tests$.default})
  } else
  {
    ftests <- df %>%
      lapply(function(x){stats::kruskal.test})
  }

  names(tests) %>% setdiff(".auto") %>% intersect(names(df)) -> forced_tests
  ftests[names(ftests) %in% forced_tests][forced_tests] <- tests[forced_tests]

  df %>%
    purrr::map2(ftests, testify, group) %>%
    dplyr::bind_rows()
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
  # Final group, make tests
  if (length(grps) == 1)
  {
    group <- eval(grps[[1]], df) %>% factor

    # Create the subtable stats
    df %>%
      dplyr::select(- eval(grps[[1]])) %>%
      by(group, statTable, stats) %>%
      # Name the subtables with info about group and group size
      stats::setNames(subNames(grps[[1]], df)) -> stats

    # Create the subtable tests
    testColumn(df, tests, grps[[1]]) -> pvalues

    c(stats, tests = list(pvalues))
  }
  else
  {
    group <- eval(grps[[1]], df)

    # Go through the next grouping levels and build the subtables
    df %>%
      dplyr::select(- eval(grps[[1]])) %>%
      by(group, subTable, stats, tests, grps[-1]) %>%
      # Name the subtables with info about group and group size
      stats::setNames(subNames(grps[[1]], df))
  }
}
