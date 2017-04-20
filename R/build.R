#' Generate one statistic for all variables
#'
#' @param stat The statistic to use
#' @param data The dataframe to apply the statistic to
#' @return A vector for one statistic column
statColumn <- function(stat, data)
{
  data %>%
    purrr::map(~ statify(., stat)) %>%
    purrr::flatten_dbl()
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
    sapply(statColumn, data) %>%
    tibble::as_data_frame()
}

#' Generate the variable column to display as row names
#'
#' Generates the variable column.
#' Replaces the variable names by their label if given in the named character vector labels
#' Inserts levels for factors
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
  if (any(data %>% purrr::map_lgl(is.factor)))
  {
    data %>%
      purrr::map_lgl(is.factor) %>%
      which -> factors_idx

    insert(x = base_names,
           y = dplyr::select(data, factors_idx) %>% purrr::map(levels) %>% purrr::at_depth(1, ~ stringr::str_c("* ", .x)),
           position = factors_idx) -> base_names
  }

  tibble::data_frame(Variables = base_names)
}

#' Generate a statistics table
#'
#' Generate a statistics table with variable names/labels and levels
#'
#' labels is an option named character vector used to make the table prettier.
#' If given, the variable names for which there is a label will be replaced by their corresponding label.
#' Not all variables need to have a label, and labels for non-existing variables are ignored.
#'
#' If data is a grouped dataframe (using group_by), subtables are created and statistic tests are perfored over each sub-group.
#'
#' For a simple descriptive table (without groups), the resulting dataframe is directly pipe-able to pander or DT, or can be exported like any dataframe to csv, etc.
#' For a grouped table, the output is list of dataframes that can be manipulated, prior to passing to the petrify() function which will make it pipe-able to pander or DT.
#' labels must be given in the form c(unquoted_variable_name = "label")
#'
#' @param data The dataframe to analyse
#' @param stats A list of named statistics to apply to each element of the dataframe, or a function returning a list of named statistics
#' @param tests A list of statistcal tests to use when calling desctable with a grouped_df
#' @param labels A named character vector of labels to use instead of variable names
#' @return A table of statistics for all variables
#' @seealso \code{\link{petrify}}
#' @export
desctable <- function(data, stats = stats_auto, tests = NULL, labels = NULL)
{
  # Replace every logical vector with a factor and nice labels
  if (any(data %>% purrr::map(is.logical) %>% purrr::flatten_lgl()))
    data %>% purrr::dmap_if(is.logical, factor, levels = c(F, T), labels = c("No", "Yes")) -> data

  NextMethod("desctable", data, stats = stats, tests = tests, labels = labels)
}

desctable.default <- function(data, stats, tests, labels)
{
  # Build the complete table
  varColumn(data, labels) %>%
    dplyr::bind_cols(statTable(data, stats))
}

desctable.grouped_df <- function(data, stats, tests, labels)
{
  # Get groups then ungroup dataframe
  grps <- data %>% dplyr::groups()
  data <- dplyr::ungroup(data)

  # Build the complete table recursively, assign "desctable" class
  c(Variables = list(varColumn(data[!names(data) %in% (grps %>% purrr::map_chr(as.character))], labels)),
    subTable(data, stats, tests, grps)) %>%
  `class<-`("desctable")
}

subNames <- function(grp, df)
{
  paste0(as.character(grp),
         ": ",
         eval(grp, df) %>% factor %>% levels,
         " (n=",
         summary(eval(grp, df) %>% factor),
         ")")
}

testColumn <- function(df, tests, grp)
{
  group <- eval(grp, df)

  df <- df %>% 
      dplyr::select(- eval(grp))

  if (is.function(tests))
  {
    ftests <- df %>%
      purrr::map(tests, group)
  } else if (!is.null(tests$.auto))
  {
    ftests <- df %>%
      purrr::map(tests$.auto, group)
  } else
  {
    ftests <- df %>%
      purrr::map(function(x){kruskal.test})
  }

  names(tests) %>% setdiff(".auto") -> forced_tests
  ftests[names(ftests) %in% forced_tests] <- tests[forced_tests %in% names(df)]
  ftests <- ftests[names(ftests) %in% names(df)]

  df %>%
    purrr::map2_dbl(.y = ftests, function(x, f) {f(x ~ group)$p.value})
}

subTable <- function(df, stats, tests, grps)
{
  # Final group, make tests
  if (length(grps) == 1)
  {
    group <- eval(grps[[1]], df)

    # Create the subtable stats
    df %>%
      dplyr::select(- eval(grps[[1]])) %>%
      by(group, statTable, stats) %>%
      # Name the subtables with info about group and group size
      stats::setNames(subNames(grps[[1]], df)) -> stats

    # Create the subtable tests
    testColumn(df, tests, grps[[1]]) -> pvalues

    c(stats, pvalues = list(tibble::data_frame(p = pvalues)))
  }
  else
  {
    group <- eval(grps[[1]], df)

    # Recursively go through the grouping levels and build the subtables
    df %>%
      dplyr::select(- eval(grps[[1]])) %>%
      by(group, subTable, stats, tests, grps[-1]) %>%
      # Name the subtables with info about group and group size
      stats::setNames(subNames(grps[[1]], df))
  }
}

#' Petrifies a table for output
#'
#' Using prettyNum, it turns a desctable to stone as a character dataframe
#'
#' This function turns a desctable into stone as a single character dataframe.
#' It combines the multiple tables for a group'ed desctable into one, with headers.
#' The ... arguments are passed to prettyNum to pretty-print the numeric columns.
#' The 'digits' argument is passed with a value of 2 by default, but can be overriden.
#'
#' The result being a less mutable dataframe (thus turned to stone), with prettified numbers,
#' it is 'petrified'.
#'
#' @param data The structure returned by desctable
#' @param digits Argument for prettyNum. Defaults to 2
#' @param ... Arguments to pass to prettyNum
#' @return A petrified desctable
#' @seealso \code{\link{desctable}}
#' @export
petrify <- function(data, digits = 2, ...)
{
  data %>% purrr::dmap(prettyNum, digits = digits, ...) %>% purrr::dmap(base::gsub, pattern = "^NA$", replacement = "")
}

print.desctable <- function(df)
{
  print(df %>% purrr::reduce(dplyr::bind_cols))
}
