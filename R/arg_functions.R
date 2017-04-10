#' Functions to create a list of statistics to use in desctable
#'
#' @param data The dataframe to apply the statistic to
#' @return A list of statistics to use, potentially assessed from the dataframe
#' @export
stats_default <- function(data)
{
  list("N" = length, "Mean/%" = mean ~ percent, "sd" = sd, "Med" = median, "IQR" = IQR)
}

#' @rdname stats_default
#' @export
stats_normal <- function(data)
{
  list("N" = length, "Mean/%" = mean ~ percent, "sd" = sd)
}

#' @rdname stats_default
#' @export
stats_nonnormal <- function(data)
{
  list("N" = length, "Median/%" = median ~ percent, "IQR" = IQR)
}

#' @rdname stats_default
#' @export
stats_auto <- function(data)
{
  data %>%
    purrr::keep(is.numeric) %>%
    purrr::map(is.normal) %>%
    purrr::flatten_lgl() -> shapiro

  any(shapiro) -> normal
  any(!shapiro) -> nonnormal
  data %>% purrr::map(is.factor) %>% purrr::flatten_lgl() %>% any -> fact

  if (fact & normal & !nonnormal)
    stats_normal(data)
  else if (fact & !normal & nonnormal)
    stats_nonnormal(data)
  else if (fact & !normal & !nonnormal)
    list("N" = length, "%" = percent)
  else if (!fact & normal & nonnormal)
    list("N" = length, "Mean" = mean, "sd" = sd, "Med" = median, "IQR" = IQR)
  else if (!fact & normal & !nonnormal)
    list("N" = length, "Mean" = mean, "sd" = sd)
  else if (!fact & !normal & nonnormal)
    list("N" = length, "Med" = median, "IQR" = IQR)
  else
    stats_default(data)
}
