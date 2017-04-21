#' Functions to create a list of statistics to use in desctable
#'
#' @param data The dataframe to apply the statistic to
#' @return A list of statistics to use, potentially assessed from the dataframe
#' @export
stats_default <- function(data)
{
  list("N" = length,
       "Mean/%" = is.factor ~ percent | (is.normal ~ mean),
       "sd" = is.normal ~ sd,
       "Med" = is.normal ~ NA | median,
       "IQR" = is.normal ~ NA | IQR)
}

#' @rdname stats_default
#' @export
stats_normal <- function(data)
{
  list("N" = length,
       "Mean/%" = is.factor ~ percent | mean,
       "sd" = stats::sd)
}

#' @rdname stats_default
#' @export
stats_nonnormal <- function(data)
{
  list("N" = length,
       "Median/%" = is.factor ~ percent | median,
       "IQR" = IQR)
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
    list("N" = length,
         "%" = percent)
  else if (!fact & normal & nonnormal)
    list("N" = length,
         "Mean" = is.normal ~ mean,
         "sd" = is.normal ~ sd,
         "Med" = is.normal ~ NA | median,
         "IQR" = is.normal ~ NA | IQR)
  else if (!fact & normal & !nonnormal)
    list("N" = length,
         "Mean" = mean,
         "sd" = stats::sd)
  else if (!fact & !normal & nonnormal)
    list("N" = length,
         "Med" = stats::median,
         "IQR" = IQR)
  else
    stats_default(data)
}

#' Functions to choose a statistical test
#'
#' @param var The variable to test
#' @param grp The variable for the groups
#' @export
tests_auto <- function(var, grp)
{
  if (is.factor(var))
  {
    stats::fisher.test
  } else
  {
    if ((var %>% tapply(grp, is.normal) %>% all) & tryCatch(stats::bartlett.test(var ~ grp) > .1, warning = function(e) F, error = function(e) F))
    {
      if (nlevels(grp) == 2)
        stats::t.test
      else
        ANOVA
    } else if (nlevels(grp) == 2)
      stats::wilcox.test
    else
      stats::kruskal.test
  }
}
