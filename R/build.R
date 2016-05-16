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
