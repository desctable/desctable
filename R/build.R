stat_column <- function(stat, data)
{
  data %>%
    lapply(stat) %>%
    unlist
}

stat_table <- function(data, stats)
{
  stats %>%
    sapply(stat_column, data) -> tbl
  stats %>%
    sapply(attr, "label") -> colnames(tbl)

  tbl
}
