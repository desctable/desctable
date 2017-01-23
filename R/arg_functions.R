#' Functions to create a list of statistics to use in desctable
#'
#' @param data The dataframe to apply the statistic to
#' @return A list of statistics to use, potentially assessed from the dataframe
#' @export
stats_default <- function(data)
{
  list("N" = length, "Mean/%" = mean ~ percent, "sd" = sd, "Med" = median, "IQR" = IQR)
}

stats_param <- function(data)
{
  list("N" = length, "Mean/%" = mean ~ percent, "sd" = sd)
}

stats_nonparam <- function(data)
{
  list("N" = length, "Median/%" = median ~ percent, "IQR" = IQR)
}

stats_auto <- function(data)
{
  data %>%
    purrr::keep(is.numeric) %>%
    purrr::map_dbl(is.param) -> shapiro

  any(shapiro) -> param
  any(!shapiro) -> nonparam
  data %>% purrr::map(is.factor) %>% flatten_lgl %>% any -> fact

  if (fact & param & !nonparam)
    stats_param(data)
  else if (fact & !param & nonparam)
    stats_nonparam(data)
  else if (fact & !param & !nonparam)
    list("N" = length, "%" = percent)
  else if (!fact & param & nonparam)
    list("N" = length, "Mean" = mean, "sd" = sd, "Med" = median, "IQR" = IQR)
  else if (!fact & param & !nonparam)
    list("N" = length, "Mean" = mean, "sd" = sd)
  else if (!fact & !param & nonparam)
    list("N" = length, "Med" = median, "IQR" = IQR)
  else
    stats_default(data)
}
