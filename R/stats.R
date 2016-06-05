#' Return the number of observations
#'
#' Return the number of observations.
#' For numerical values, return the sample size. Missing values are omitted.
#' For factors, return the sample size for each level.
#' @param x A vector
#' @return The sample size without missing values
#' @export
N <- function(x)
{
  x %>% stats::na.omit() %>% length -> out

  if (is.factor(x))
  {
    c(out, x %>% stats::na.omit() %>% summary) -> out
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
#' @return The mean of the vector / sample size without missing values
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
#' @return The percentages of observations for each level of a factor
#' @export
Pct <- function(x)
{
  if (is.factor(x))
  {
    c(NA, x %>% stats::na.omit() %>% summary / x %>% stats::na.omit() %>% length) * 100
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
    stats::sd(x, na.rm = T)
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
#' @return The sd of the vector / percentages of each level of the factor
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

#' Return the minimal value
#'
#' Return the minimal value of a numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The minimal value of the vector
#' @export
Min <- function(x)
{
  if (is.double(x))
  {
    min(x, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}
attr(Min, "label") <- "Min"

#' Return the maximal value
#'
#' Return the maximal value of a numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The maximal value of the vector
#' @export
Max <- function(x)
{
  if (is.double(x))
  {
    max(x, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}
attr(Max, "label") <- "Max"

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
  if (is.double(x))
  {
    max(x, na.rm = T) - min(x, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}
attr(Range, "label") <- "Range"

#' Return the median
#'
#' Return the median of a numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The median of the vector
#' @export
Med <- function(x)
{
  if (is.double(x))
  {
    stats::median(x, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}
attr(Med, "label") <- "Median"

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
  if (is.numeric(x))
  {
    stats::quantile(x, .25, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}
attr(Q1, "label") <- "Q1"

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
  if (is.numeric(x))
  {
    stats::quantile(x, .75, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}
attr(Q3, "label") <- "Q3"

#' Return the inter-quartile range
#'
#' Return the inter-quartile range of a numeric vector.
#' NAs are removed by default.
#' For non-numerical vectors, return NA.
#' @param x A vector
#' @return The inter-quartile range of the vector
#' @export
IQR <- function(x)
{
  if (is.numeric(x))
  {
    stats::quantile(x, .75, na.rm = T) - stats::quantile(x, .25, na.rm = T)
  } else if (is.factor(x))
  {
    rep(NA, 1 + nlevels(x))
  } else
  {
    NA
  }
}
attr(IQR, "label") <- "IQR"
