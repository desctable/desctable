#' Return the number of observations
#'
#' Return the number of observations.
#' For numerical values, return the sample size. Missing values are omitted.
#' For factors, return the sample size for each level.
#' @param x A vector
#' @return A vector of sample sizes
N_ <- function(x)
{
  if (is.numeric(x))
  {
    x %>% na.omit %>% length
  } else if (is.factor(x))
  {
    c(x %>% na.omit %>% length, x %>% na.omit %>% summary)
  } else
  {
    NA
  }
}

#' Return the mean
#'
#' Return the mean of the numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The mean of the vector
mean_ <- function(x)
{
  if (is.numeric(x))
  {
    mean(x, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}

#' Return the number of observations / mean
#'
#' Return the mean of a numeric vector.
#' For numerical values, return the sample size
#' NAs are removed by default.
#' For factors, return the sample size for each level
#' @param x A vector
#' @return A vector of sample sizes
N_mean_ <- function(x)
{
  if (is.numeric(x))
  {
    mean(x, na.rm = T)
  } else if (is.factor(x))
  {
    c(x %>% na.omit %>% length, x %>% na.omit %>% summary)
  } else
  {
    NA
  }
}
