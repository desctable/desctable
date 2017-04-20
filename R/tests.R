fisher.test.default <- stats::chisq.test

fisher.test <- function(...)
{
  UseMethod("fisher.test")
}

fisher.test.formula <- function(formula, ...)
{
  fisher.test.default(x = eval(formula[[2]]), y = eval(formula[[3]]), ...)
}

chisq.test.default <- stats::chisq.test

chisq.test <- function(...)
{
  UseMethod("chisq.test")
}

chisq.test.formula <- function(formula, ...)
{
  chisq.test.default(x = eval(formula[[2]]), y = eval(formula[[3]]), ...)
}
