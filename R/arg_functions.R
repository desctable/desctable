#' Functions to create a list of statistics to use in desctable
#'
#' These functions take a dataframe as argument and return a list of statistcs in the form accepted by desctable.
#'
#' Already defined are
#' - stats_default with length, mean/%, sd, med and IQR
#' - stats_normal with length, mean/% and sd
#' - stats_nonnormal with length, median/% and IQR
#' - stats_auto, which picks stats depending of the data
#'
#' You can define your own automatic functions, as long as they take a dataframe as argument and return a list of functions or formulas defining conditions to use a stat function.
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
       "IQR" = is.factor ~ NA | (is.normal ~ NA | IQR))
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
       "IQR" = is.factor ~ NA | IQR)
}

#' @rdname stats_default
#' @export
stats_auto <- function(data)
{
  data %>%
    Filter(f = is.numeric) %>%
    lapply(is.normal) %>%
    unlist -> shapiro

  any(shapiro) -> normal
  any(!shapiro) -> nonnormal
  any(data %>% lapply(is.factor) %>% unlist) -> fact

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
         "IQR" = stats::IQR)
  else
    stats_default(data)
}

#' Functions to choose a statistical test
#'
#' These functions take a variable and a grouping variable as arguments, and return a statistcal test to use, expressed as a single-term formula.
#'
#' Currently, only tests_auto is defined, and picks between t test, wilcoxon, anova, kruskal-wallis and fisher depending on the number of groups, the type of the variable, the normality and homoskedasticity of the distributions.
#'
#' @param var The variable to test
#' @param grp The variable for the groups
#' @return A statistical test function
#' @export
tests_auto <- function(var, grp)
{
  grp <- grp %>% factor
  if (var %>% is.factor)
    ~fisher.test
  else
  {
    if (all(var %>% tapply(grp, is.normal)) & tryCatch(stats::bartlett.test(var ~ grp)$p.value > .1, warning = function(e) F, error = function(e) F))
    {
      if (nlevels(grp) == 2)
        ~t.test
      else
        ~ANOVA
    } else if (nlevels(grp) == 2)
      ~wilcox.test
    else
      ~kruskal.test
  }
}
