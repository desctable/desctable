#' Generate one statistic for all variables
#'
#' @param stat The statistic to use
#' @param data The dataframe to apply the statistic to
#' @return A vector for one statistic column
#' @importFrom purrr map flatten_dbl
statColumn <- function(stat, data)
{
  data %>%
    map(stat) %>%
    flatten_dbl
}

#' Generate the table of all statistics for all variables
#'
#' @param data The dataframe to apply the statistic to
#' @param stat A list of statistics to use
#' @return A dataframe of all statistics for all variables
statTable <- function(data, stats)
{
  stats %>%
    sapply(statColumn, data) -> tbl
  stats %>%
    sapply(attr, "label") -> colnames(tbl)

  tbl
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
#' @return A character vector of variable names/labels/levels
#' @importFrom purrr map map_lgl
varColumn <- function(data, labels = NULL)
{
# Replace variable names by their labels, if they exist
  names(data) -> base_names
  base_names[base_names %in% names(labels)] <- labels[base_names[base_names %in% names(labels)]]

  # Insert levels for factors after the variable name
  if (any(data %>% map_lgl(is.factor)))
  {
    data %>%
      map_lgl(is.factor) %>%
      which %>%
      insert(x = base_names,
             y = select(data, .) %>% map(levels),
             position = .)
  } else
  {
    base_names
  }
}
