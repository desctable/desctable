testify <- function(x, f, group)
{
  p <- tryCatch(f(x ~ group)$p.value[1],
                error = function(e) {message(e);NaN})
  if (is.factor(x))
    c(p, rep(NA, nlevels(x)))
  else
    p
}

fisher.test.default <- stats::fisher.test

fisher.test <- function(...)
{
  UseMethod("fisher.test")
}

fisher.test.formula <- function(formula, ...)
{
  fisher.test.default(x = eval(formula[[2]], envir = parent.frame()), y = eval(formula[[3]], envir = parent.frame()), ...)
}

chisq.test.default <- stats::chisq.test

chisq.test <- function(...)
{
  UseMethod("chisq.test")
}

chisq.test.formula <- function(formula, ...)
{
  chisq.test.default(x = eval(formula[[2]], envir = parent.frame()), y = eval(formula[[3]], envir = parent.frame()), ...)
}

ANOVA <- function(formula)
{
  summary(aov(formula))[[1]] %>%
    setNames(c("Df", "Sum Sq", "Mean Sq", "F value", "p.value"))
}
