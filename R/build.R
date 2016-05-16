statColumn <- function(stat, data)
{
  data %>%
    lapply(stat) %>%
    unlist
}

statTable <- function(data, stats)
{
  stats %>%
    sapply(statColumn, data) -> tbl
  stats %>%
    sapply(attr, "label") -> colnames(tbl)

  tbl
}

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
      which %>%
      insert(x = base_names,
             y = select(data, .) %>% lapply(levels),
             position = .)
  } else
  {
    base_names
  }
}

