#' Return the percentages for the levels of a factor
#'
#' Return a compatible vector of length nlevels(x) + 1
#' to print the percentages of each level of a factor
#' @param x A factor
#' @export
#' @return A nlevels(x) + 1 length vector of percentages
percent <- function(x)
{
  c(NA, summary(x) / length(x)) * 100
}

#' Return the inter-quartile range
#'
#' Safe version of IQR for statify
#' @param x A vector
#' @return The IQR
#' @export
IQR <- function(x)
{
  base::diff(stats::quantile(x, c(0.25, 0.75), na.rm = T))
}

#' Test if distribution is normal
#'
#' Test if distribution is normal.
#' The condition for normality is length > 30 and non-significant Shapiro-Wilks test with p > .1
#'
#' @param x A numerical vector
#' @export
#' @return A boolean
is.normal <- function(x)
{
  if (! x %>% is.numeric) 
    F
  else if (length(x %>% stats::na.omit()) >= 30)
    stats::shapiro.test(x)$p.value > .1
  else
    F
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
#' @seealso stats::chisq.test
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
#' @seealso stats::aov
#' @export
ANOVA <- function(formula)
{
  summary(stats::aov(formula))[[1]] %>%
    stats::setNames(c("Df", "Sum Sq", "Mean Sq", "F value", "p.value"))
}

#' No test
#'
#' An empty test
#' @param formula A formula
no.test <- function(formula)
{
  data.frame(p.value = NA)
}
