#' Transform any test function into a valid test function for the table
#'
#' Transform a function into a valid test function for the table
#' Applying the function on a numerical vector should return one value
#' Applying the function on a factor should return nlevels + 1 value, or one value per factor level
#' @param x A vector
#' @param f The function to try to apply, or a formula combining two functions
#' @param group Grouping factor
#' @return The results for the function applied on the vector, compatible with the format of the result table
testify <- function(x, f, group) {
  # Extract the name of the function
  f %>%
    deparse() %>%
    Reduce(f = paste0) %>%
    substring(2) -> fun

  # If eval(f[[2]]) throws an error, then we may be in an rlang-formula
  tryCatch(f <- eval(f[[2]]),
           error = function(e) {f <<- rlang::as_function(f)})

  # Try the function
  p <- tryCatch(f(x ~ group)$p.value[1],
                error = function(e) {message(e);NaN})

  # Return the correct number of rows depending on the variable type
  if (is.factor(x)) data.frame(p = c(p, rep(NA, nlevels(x))),
                               test = c(fun, rep(NA, nlevels(x))),
                               row.names = NULL, check.names = F, stringsAsFactors = F)
  else              data.frame(p = p,
                               test = fun,
                               row.names = NULL, check.names = F, stringsAsFactors = F)
}


#' Function to choose a statistical test
#'
#' This function takes a variable and a grouping variable as arguments, and returns a statistcal test to use, expressed as a single-term formula.
#'
#' This function uses appropriate non-parametric tests depending on the number of levels (wilcoxon.test for two levels
#' and kruskal.test for more), and fisher.test with fallback on chisq.test on error for factors.
#'
#' @param var The variable to test
#' @param grp The variable for the groups
#' @return A statistical test function
#' @export
tests_auto <- function(var, grp) {
  grp <- factor(grp)

  if (nlevels(grp) < 2)
    ~no.test
  else if (is.factor(var)) {
    if (tryCatch(is.numeric(fisher.test(var ~ grp)$p.value), error = function(e) F))
      ~fisher.test
    else
      ~chisq.test
  } else if (nlevels(grp) == 2)
    ~wilcox.test
  else
    ~kruskal.test
}
