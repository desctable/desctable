#' Transform any test function into a valid test function for the table
#'
#' Transform a function into a valid test function for the table
#' Applying the function on a numerical vector should return one value
#' Applying the function on a factor should return nlevels + 1 value, or one value per factor level
#' @param x A vector
#' @param f The function to try to apply, or a formula combining two functions
#' @param group Grouping factor
#' @return The results for the function applied on the vector, compatible with the format of the result table
testify <- function(x, f, group)
{
  fun <- f %>% deparse %>% Reduce(f = paste0) %>% substring(2)
  f <- eval(f[[2]])
  p <- tryCatch(f(x ~ group)$p.value[1],
                error = function(e) {message(e);NaN})
  if (is.factor(x))
    data.frame(p = c(p, NA %>% rep(nlevels(x))),
               test = c(fun, NA %>% rep(nlevels(x))),
               row.names = NULL, check.names = F, stringsAsFactors = F)
  else
    data.frame(p = p,
               test = fun,
               row.names = NULL, check.names = F, stringsAsFactors = F)
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
  if (nlevels(grp) < 2)
    ~no.test
  else if (var %>% is.factor)
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
