one_column <- function(stat, data)
{
  data %>%
    lapply(stat) %>%
    unlist
}

one_table <- function(data, stats)
{
  stats %>%
    sapply(one_column, data) -> tbl
  stats %>%
    sapply(attr, "label") -> colnames(tbl)

  tbl
}
