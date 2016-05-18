#' Return the number of observations
#'
#' Return the number of observations.
#' For numerical values, return the sample size. Missing values are omitted.
#' For factors, return the sample size for each level.
#' @param x A vector
#' @return A vector of sample sizes
#' @export
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
#' @export
Mean <- function(x)
{
  if (is.double(x))
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
#' NAs are removed by default.
#' For factors, return the sample size for each level
#' @param x A vector
#' @return A vector of means / sample sizes
#' @export
N_Mean <- function(x)
{
  if (is.double(x))
  {
    Mean(x)
  } else
  {
    N(x)
  }
}
attr(N_Mean, "label") <- "N / Mean"

#' Return the percentage of observations
#'
#' Return the percentage of observations.
#' For numerical values, return NA.
#' For factors, return the percentage of each level.
#' @param x A vector
#' @return A vector of percentages
#' @export
Pct <- function(x)
{
  if (is.factor(x))
  {
    c(NA, x %>% na.omit %>% summary / x %>% na.omit %>% length)
  } else
  {
    NA
  }
}
attr(Pct, "label") <- "%"

#' Return the standard deviation
#'
#' Return the standard deviation of a numeric vectorr
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The standard deviation of the vector
#' @export
Sd <- function(x)
{
  if (is.double(x))
  {
    sd(x, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}
attr(Sd, "label") <- "Sd"

#' Return the percentage / sd
#'
#' Return the sd of a numeric vector.
#' NAs are removed by default.
#' For factors, return the percentage of each level
#' @param x A vector
#' @return A vector of sd / percentages
#' @export
Sd_Pct <- function(x)
{
  if (is.double(x))
  {
    Sd(x)
  } else
  {
    Pct(x)
  }
}
attr(Sd_Pct, "label") <- "Sd / %"

