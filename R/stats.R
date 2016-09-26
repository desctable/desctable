#' Transform any function into a valid stat function for the table
#'
#' Transform a function into a valid stat function for the table
#' NA values are removed from the data
#' Applying the function on a numerical vector should return one value
#' Applying the function on a factor should return nlevels + 1 value, or one value per factor level
#' @param x A vector
#' @param f The function to try to apply
#' @return The results for the function applied on the vector, compatible with the format of the result table
statify <- function(x, f)
{
  x <- x %>% stats::na.omit()

  res <- tryCatch(x %>% f,
                  error = function(e) NA,
                  warning = function(e) suppressWarnings(x %>% f))

  if (x %>% is.factor)
  {
    if (res %>% length == nlevels(x) + 1)
      res
    else if (res %>% length == 1)
      c(res, purrr::map_dbl(x %>% levels, . %>% {
                              tryCatch(x[x == .] %>% f,
                                       error = function(e) NA,
                                       warning = function(e) suppressWarnings(x[x == .] %>% f))}))
    else
      NA
  } else
  {
    if (res %>% length == 1)
      res
    else
      NA
  }
}

#' Return the percentages for the levels of a factor
#'
#' Return a compatible vector of length nlevels(x) + 1
#' to print the percentages of each level of a factor
#' @param x A factor
#' @return A nlevels(x) + 1 length vector of percentages
percent <- function(x)
{
  c(NA, x %>% summary / x %>% length) * 100
}

#' Return the number of observations / mean
#'
#' Return the mean of a numeric vector.
#' NAs are removed by default.
#' For factors, return the sample size for each level
#' @param x A vector
#' @return The mean of the vector / sample size without missing values
#' @export
N_Mean <- function(x)
{
  if (is.double(x))
  {
    mean(x)
  } else
  {
    length(x)
  }
}

#' Return the percentage / sd
#'
#' Return the sd of a numeric vector.
#' NAs are removed by default.
#' For factors, return the percentage of each level
#' @param x A vector
#' @return The sd of the vector / percentages of each level of the factor
#' @export
Sd_Pct <- function(x)
{
  if (is.double(x))
  {
    sd(x)
  } else
  {
    percent(x)
  }
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
