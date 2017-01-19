#' Transform any function into a valid stat function for the table
#'
#' Transform a function into a valid stat function for the table
#' NA values are removed from the data
#' Applying the function on a numerical vector should return one value
#' Applying the function on a factor should return nlevels + 1 value, or one value per factor level
#' If a formula is provided as lhs ~ rhs, the rhs function will be applied for factors, and the lhs function for all other types'
#' @param f The function to try to apply, or a formula combining two functions
#' @param x A vector
#' @return The results for the function applied on the vector, compatible with the format of the result table
statify <- function(x, f)
{
  UseMethod("statify", f)
}

statify.default <- function(x, f)
{
  x <- x %>% stats::na.omit()

  res <- tryCatch(x %>% f,
                  error = function(e) NA,
                  warning = function(e) suppressWarnings(x %>% f))

  if (x %>% is.factor)
  {
    if (res %>% length == nlevels(x) + 1)
      res
    else if (res %>% length == 1 & res %>% is.numeric | res %>% is.na)
      c(res, purrr::map_dbl(x %>% levels, . %>% {
                              tryCatch(x[x == .] %>% f,
                                       error = function(e) NA,
                                       warning = function(e) suppressWarnings(x[x == .] %>% f))}))
    else
      rep(NA, nlevels(x) + 1)
  } else
  {
    if (res %>% length == 1 & res %>% is.numeric | res %>% is.na)
      res %>% as.double
    else
      NA
  }
}

statify.formula <- function(x, f)
{
  fns <- f %>% terms %>% attr("variables") %>% eval

  if (x %>% is.factor)
    statify.default(x, fns[[2]])
  else
    statify.default(x, fns[[1]])
}

#' Return the percentages for the levels of a factor
#'
#' Return a compatible vector of length nlevels(x) + 1
#' to print the percentages of each level of a factor
#' @param x A factor
#' @export
#' @return A nlevels(x) + 1 length vector of percentages
percent <- function(x)
{
  c(NA, x %>% summary / x %>% length) * 100
}

#' Return the range
#'
#' Return the range of a numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The range of the vector
#' @export
Range <- function(x)
{
  max(x) - min(x)
}

#' Return the first quartile
#'
#' Return the first quartile of a numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The first quartile of the vector
#' @export
Q1 <- function(x)
{
  stats::quantile(x, .25)
}

#' Return the third quartile
#'
#' Return the third quartile of a numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The third quartile of the vector
#' @export
Q3 <- function(x)
{
  stats::quantile(x, .75, na.rm = T)
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
