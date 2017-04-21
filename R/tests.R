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
  p <- tryCatch(f(x ~ group)$p.value[1],
                error = function(e) {message(e);NaN})
  if (is.factor(x))
    c(p, rep(NA, nlevels(x)))
  else
    p
}

#' Fisher test
#'
#' @inheritParams stats::fisher.test
#' @seealso stats::fisher.test
#' @export
fisher.test <- function(x, y = NULL, workspace = 200000, hybrid = FALSE, control = list(), or = 1, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95, simulate.p.value = FALSE, B = 2000)
{
  UseMethod("fisher.test")
}

fisher.test.default <- stats::fisher.test

fisher.test.formula <- function(x, ...)
{
  fisher.test.default(x = eval(x[[2]], envir = parent.frame()), y = eval(x[[3]], envir = parent.frame()), ...)
}

#' Chi-square test
#'
#' @inheritParams stats::chisq.test
#' @export
chisq.test <- function(x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)), rescale.p = FALSE, simulate.p.value = FALSE, B = 2000)
{
  UseMethod("chisq.test")
}

chisq.test.default <- stats::chisq.test

chisq.test.formula <- function(x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)), rescale.p = FALSE, simulate.p.value = FALSE, B = 2000, ...)
{
  chisq.test.default(x = eval(x[[2]], envir = parent.frame()), y = eval(x[[3]], envir = parent.frame()), ...)
}

#' Wrapper for summary(aov)
#'
#' @param formula An anova formula (variable ~ grouping variable)
#' @export
ANOVA <- function(formula)
{
  summary(stats::aov(formula))[[1]] %>%
    stats::setNames(c("Df", "Sum Sq", "Mean Sq", "F value", "p.value"))
}
