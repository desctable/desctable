#' Return the percentages for the levels of a factor
#'
#' Return a compatible vector of length nlevels(x) + 1
#' to print the percentages of each level of a factor
#' @param x A factor
#' @export
#' @return A nlevels(x) + 1 length vector of percentages
percent <- function(x)
{
  c(NA, summary(x, maxsum = Inf) / length(x)) * 100
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
    tryCatch(stats::shapiro.test(x)$p.value > .1,
             error = function(e) F)
  else
    F
}

#' Fisher's Exact Test for Count Data
#'
#' Performs Fisher's exact test for testing the null of independence
#' of rows and columns in a contingency table with fixed marginals, or with a formula expression.
#'
#' If \code{x} is a matrix, it is taken as a two-dimensional contingency
#' table, and hence its entries should be nonnegative integers.
#' Otherwise, both \code{x} and \code{y} must be vectors of the same length.
#' Incomplete cases are removed, the vectors are coerced into factor
#' objects, and the contingency table is computed from these.
#'
#' For 2 by 2 cases, p-values are obtained directly using the
#' (central or non-central) hypergeometric distribution. Otherwise,
#' computations are based on a C version of the FORTRAN subroutine
#' FEXACT which implements the network developed by Mehta and Patel
#' (1986) and improved by Clarkson, Fan and Joe (1993).  The FORTRAN
#' code can be obtained from \url{http://www.netlib.org/toms/643}.
#' Note this fails (with an error message) when the entries of the
#' table are too large.  (It transposes the table if necessary so it
#' has no more rows than columns.  One constraint is that the product
#' of the row marginals be less than 2^31 - 1.)
#'
#' For 2 by 2 tables, the null of conditional independence is
#' equivalent to the hypothesis that the odds ratio equals one.
#' \code{Exact} inference can be based on observing that in general, given
#' all marginal totals fixed, the first element of the contingency
#' table has a non-central hypergeometric distribution with
#' non-centrality parameter given by the odds ratio (Fisher, 1935).
#' The alternative for a one-sided test is based on the odds ratio,
#' so \code{alternative = "greater"} is a test of the odds ratio being
#' bigger than \code{or}.
#'
#' Two-sided tests are based on the probabilities of the tables, and
#' take as \code{more extreme} all tables with probabilities less than or
#' equal to that of the observed table, the p-value being the sum of
#' such probabilities.
#'
#' For larger than 2 by 2 tables and \code{hybrid = TRUE}, asymptotic
#' chi-squared probabilities are only used if the ‘Cochran
#' conditions’ are satisfied, that is if no cell has count zero, and
#' more than 80% of the cells have counts at least 5: otherwise the
#' exact calculation is used.
#'
#' Simulation is done conditional on the row and column marginals,
#' and works only if the marginals are strictly positive.  (A C
#' translation of the algorithm of Patefield (1981) is used.)
#' @param x either a two-dimensional contingency table in matrix form, a factor object, or a formula of the form \code{lhs ~ rhs} where \code{lhs} and \code{rhs} are factors.
#' @param y a factor object; ignored if \code{x} is a matrix or a formula.
#' @inheritParams stats::fisher.test
#' @return A list with class \code{"htest"} containing the following components:
#'
#' p.value: the p-value of the test.
#'
#' conf.int: a confidence interval for the odds ratio.  Only present in
#'           the 2 by 2 case and if argument \code{conf.int = TRUE}.
#'
#' estimate: an estimate of the odds ratio.  Note that the _conditional_
#'           Maximum Likelihood Estimate (MLE) rather than the
#'           unconditional MLE (the sample odds ratio) is used.  Only
#'           present in the 2 by 2 case.
#'
#' null.value: the odds ratio under the null, \code{or}.  Only present in the 2
#'           by 2 case.
#'
#' alternative: a character string describing the alternative hypothesis.
#'
#' method: the character string \code{"Fisher's Exact Test for Count Data"}.
#'
#' data.name: a character string giving the names of the data.
#' @references
#' Agresti, A. (1990) _Categorical data analysis_.  New York: Wiley.
#' Pages 59-66.
#'
#' Agresti, A. (2002) _Categorical data analysis_. Second edition.
#' New York: Wiley.  Pages 91-101.
#'
#' Fisher, R. A. (1935) The logic of inductive inference.  _Journal
#' of the Royal Statistical Society Series A_ *98*, 39-54.
#'
#' Fisher, R. A. (1962) Confidence limits for a cross-product ratio.
#' _Australian Journal of Statistics_ *4*, 41.
#'
#' Fisher, R. A. (1970) _Statistical Methods for Research Workers._
#' Oliver & Boyd.
#'
#' Mehta, C. R. and Patel, N. R. (1986) Algorithm 643. FEXACT: A
#' Fortran subroutine for Fisher's exact test on unordered r*c
#' contingency tables.  _ACM Transactions on Mathematical Software_,
#' *12*, 154-161.
#'
#' Clarkson, D. B., Fan, Y. and Joe, H. (1993) A Remark on Algorithm
#' 643: FEXACT: An Algorithm for Performing Fisher's Exact Test in r
#' x c Contingency Tables.  _ACM Transactions on Mathematical
#' Software_, *19*, 484-488.
#'
#' Patefield, W. M. (1981) Algorithm AS159.  An efficient method of
#' generating r x c tables with given row and column totals.
#' _Applied Statistics_ *30*, 91-97.
#' @seealso
#' \code{\link{chisq.test}}
#'
#' \code{fisher.exact} in package \pkg{kexact2x2} for alternative
#' interpretations of two-sided tests and confidence intervals for 2
#' by 2 tables.
#' @examples
#' \dontrun{
#' ## Agresti (1990, p. 61f; 2002, p. 91) Fisher's Tea Drinker
#' ## A British woman claimed to be able to distinguish whether milk or
#' ##  tea was added to the cup first.  To test, she was given 8 cups of
#' ##  tea, in four of which milk was added first.  The null hypothesis
#' ##  is that there is no association between the true order of pouring
#' ##  and the woman's guess, the alternative that there is a positive
#' ##  association (that the odds ratio is greater than 1).
#' TeaTasting <-
#' matrix(c(3, 1, 1, 3),
#'        nrow = 2,
#'        dimnames = list(Guess = c("Milk", "Tea"),
#'                        Truth = c("Milk", "Tea")))
#' fisher.test(TeaTasting, alternative = "greater")
#' ## => p = 0.2429, association could not be established
#'
#' ## Fisher (1962, 1970), Criminal convictions of like-sex twins
#' Convictions <-
#' matrix(c(2, 10, 15, 3),
#'        nrow = 2,
#'        dimnames =
#'        list(c("Dizygotic", "Monozygotic"),
#'             c("Convicted", "Not convicted")))
#' Convictions
#' fisher.test(Convictions, alternative = "less")
#' fisher.test(Convictions, conf.int = FALSE)
#' fisher.test(Convictions, conf.level = 0.95)$conf.int
#' fisher.test(Convictions, conf.level = 0.99)$conf.int
#'
#' ## A r x c table  Agresti (2002, p. 57) Job Satisfaction
#' Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
#' dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
#'                 satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))
#' fisher.test(Job)
#' fisher.test(Job, simulate.p.value = TRUE, B = 1e5)
#'
#' ###
#' }
#' @export
fisher.test <- function(x, y, workspace, hybrid, control, or, alternative, conf.int, conf.level, simulate.p.value, B)
{
  UseMethod("fisher.test")
}

#' @rdname fisher.test
fisher.test.default <- stats::fisher.test

#' @rdname fisher.test
fisher.test.formula <- function(x,
                                y = NULL,
                                workspace = 200000,
                                hybrid = F,
                                control = list(),
                                or = 1,
                                alternative = "two.sided",
                                conf.int = T,
                                conf.level = .95,
                                simulate.p.value = F,
                                B = 2000)
{
  stats::fisher.test(x = eval(x[[2]], envir = parent.frame()),
                      y = eval(x[[3]], envir = parent.frame()),
                      workspace = workspace,
                      hybrid = hybrid,
                      control = control,
                      or = or,
                      alternative = alternative,
                      conf.int = conf.int,
                      conf.level = conf.level,
                      simulate.p.value = simulate.p.value,
                      B = B)
}

#' Pearson's Chi-squared Test for Count Data
#'
#' \code{chisq.test} performs chi-squared contingency table tests and goodness-of-fit tests, with an added method for formulas.
#'
#' If \code{x} is a matrix with one row or column, or if \code{x} is a vector
#' and \code{y} is not given, then a _goodness-of-fit test_ is performed
#' (\code{x} is treated as a one-dimensional contingency table).  The
#' entries of \code{x} must be non-negative integers.  In this case, the
#' hypothesis tested is whether the population probabilities equal
#' those in \code{p}, or are all equal if \code{p} is not given.
#'
#' If \code{x} is a matrix with at least two rows and columns, it is taken
#' as a two-dimensional contingency table: the entries of \code{x} must be
#' non-negative integers.  Otherwise, \code{x} and \code{y} must be vectors or
#' factors of the same length; cases with missing values are removed,
#' the objects are coerced to factors, and the contingency table is
#' computed from these.  Then Pearson's chi-squared test is performed
#' of the null hypothesis that the joint distribution of the cell
#' counts in a 2-dimensional contingency table is the product of the
#' row and column marginals.
#'
#' If \code{simulate.p.value} is \code{FALSE}, the p-value is computed from the
#' asymptotic chi-squared distribution of the test statistic;
#' continuity correction is only used in the 2-by-2 case (if
#' \code{correct} is \code{TRUE}, the default).  Otherwise the p-value is
#' computed for a Monte Carlo test (Hope, 1968) with \code{B} replicates.
#'
#' In the contingency table case simulation is done by random
#' sampling from the set of all contingency tables with given
#' marginals, and works only if the marginals are strictly positive.
#' Continuity correction is never used, and the statistic is quoted
#' without it.  Note that this is not the usual sampling situation
#' assumed for the chi-squared test but rather that for Fisher's
#' exact test.
#'
#' In the goodness-of-fit case simulation is done by random sampling
#' from the discrete distribution specified by \code{p}, each sample being
#' of size \code{n = sum(x)}.  This simulation is done in R and may be
#' slow.
#' @param x a numeric vector, or matrix, or formula of the form \code{lhs ~ rhs} where \code{lhs} and \code{rhs} are factors. \code{x} and \code{y} can also both be factors.
#' @param y a numeric vector; ignored if \code{x} is a matrix or a formula. If \code{x} is a factor, \code{y} should be a factor of the same length.
#' @inheritParams stats::chisq.test
#' @return A list with class \code{"htest"} containing the following components:
#' statistic: the value the chi-squared test statistic.
#'
#' parameter: the degrees of freedom of the approximate chi-squared
#'           distribution of the test statistic, \code{NA} if the p-value is
#'           computed by Monte Carlo simulation.
#'
#'  p.value: the p-value for the test.
#'
#'   method: a character string indicating the type of test performed, and
#'           whether Monte Carlo simulation or continuity correction was
#'           used.
#'
#' data.name: a character string giving the name(s) of the data.
#'
#' observed: the observed counts.
#'
#' expected: the expected counts under the null hypothesis.
#'
#' residuals: the Pearson residuals, ‘(observed - expected) /
#'           sqrt(expected)’.
#'
#'   stdres: standardized residuals, \code{(observed - expected) / sqrt(V)},
#'           where \code{V} is the residual cell variance (Agresti, 2007,
#'           section 2.4.5 for the case where \code{x} is a matrix, ‘n * p * (1
#'           - p)’ otherwise).
#' @source The code for Monte Carlo simulation is a C translation of the Fortran algorithm of Patefield (1981).
#' @references
#' Hope, A. C. A. (1968) A simplified Monte Carlo significance test
#' procedure.  _J. Roy, Statist. Soc. B_ *30*, 582-598.
#'
#' Patefield, W. M. (1981) Algorithm AS159.  An efficient method of
#' generating r x c tables with given row and column totals.
#' _Applied Statistics_ *30*, 91-97.
#'
#' Agresti, A. (2007) _An Introduction to Categorical Data Analysis,
#' 2nd ed._, New York: John Wiley & Sons.  Page 38.
#' @seealso For goodness-of-fit testing, notably of continuous distributions, \code{\link{ks.test}}.
#' @examples
#' \dontrun{
#' ## From Agresti(2007) p.39
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(gender = c("F", "M"),
#'                     party = c("Democrat","Independent", "Republican"))
#' (Xsq <- chisq.test(M))  # Prints test summary
#' Xsq$observed   # observed counts (same as M)
#' Xsq$expected   # expected counts under the null
#' Xsq$residuals  # Pearson residuals
#' Xsq$stdres     # standardized residuals
#'
#'
#' ## Effect of simulating p-values
#' x <- matrix(c(12, 5, 7, 7), ncol = 2)
#' chisq.test(x)$p.value           # 0.4233
#' chisq.test(x, simulate.p.value = TRUE, B = 10000)$p.value
#'                                 # around 0.29!
#'
#' ## Testing for population probabilities
#' ## Case A. Tabulated data
#' x <- c(A = 20, B = 15, C = 25)
#' chisq.test(x)
#' chisq.test(as.table(x))             # the same
#' x <- c(89,37,30,28,2)
#' p <- c(40,20,20,15,5)
#' try(
#' chisq.test(x, p = p)                # gives an error
#' )
#' chisq.test(x, p = p, rescale.p = TRUE)
#'                                 # works
#' p <- c(0.40,0.20,0.20,0.19,0.01)
#'                                 # Expected count in category 5
#'                                 # is 1.86 < 5 ==> chi square approx.
#' chisq.test(x, p = p)            #               maybe doubtful, but is ok!
#' chisq.test(x, p = p, simulate.p.value = TRUE)
#'
#' ## Case B. Raw data
#' x <- trunc(5 * runif(100))
#' chisq.test(table(x))            # NOT 'chisq.test(x)'!
#'
#' ###
#' }
#' @export
chisq.test <- function(x, y, correct, p, rescale.p, simulate.p.value, B)
{
  UseMethod("chisq.test")
}

#' @rdname chisq.test
chisq.test.default <- stats::chisq.test

#' @rdname chisq.test
chisq.test.formula <- function(x,
                               y = NULL,
                               correct = T,
                               p = rep(1/length(x), length(x)),
                               rescale.p = F,
                               simulate.p.value = F,
                               B = 2000)
{
  stats::chisq.test(x = eval(x[[2]], envir = parent.frame()),
                     y = eval(x[[3]], envir = parent.frame()),
                     correct = correct,
                     p = p,
                     rescale.p = rescale.p,
                     simulate.p.value = simulate.p.value,
                     B = B)
}

#' Wrapper for oneway.test(var.equal = T)
#'
#' @param formula An anova formula (\code{variable ~ grouping variable})
#' @seealso \code{\link{oneway.test}}
#' @export
ANOVA <- function(formula)
{
  stats::oneway.test(formula, var.equal = T)
}

#' No test
#'
#' An empty test
#' @param formula A formula
no.test <- function(formula)
{
  data.frame(p.value = NA)
}
