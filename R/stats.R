#' Return the number of observations
#'
#' Return the number of observations.
#' For numerical values, return the sample size. Missing values are omitted.
#' For factors, return the sample size for each level.
#' @param x A vector
#' @return A vector of sample sizes
N <- function(x)
{
  x %>% na.omit %>% length -> out

  if (is.factor(x))
  {
    c(out, x %>% na.omit %>% summary) -> out
  }

  out
}
attr(N, "label") <- "N"

#' Return the mean
#'
#' Return the mean of the numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The mean of the vector
Mean <- function(x)
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
attr(Mean, "label") <- "Mean"

#' Return the number of observations / mean
#'
#' Return the mean of a numeric vector.
#' For numerical values, return the sample size
#' NAs are removed by default.
#' For factors, return the sample size for each level
#' @param x A vector
#' @return A vector of sample sizes
N_Mean <- function(x)
{
  if (is.numeric(x))
  {
    Mean(x)
  } else
  {
    N(x)
  }
}
attr(N_Mean, "label") <- "N / mean"
