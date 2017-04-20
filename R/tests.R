fisher.test.default <- stats::chisq.test

fisher.test <- function(...)
{
  UseMethod("fisher.test")
}

fisher.test.formula <- function(formula, ...)
{
  fisher.test.default(x = eval(formula[[2]]), y = eval(formula[[3]]), ...)
}
