#' Transform any function into a valid stat function for the table
#'
#' Transform a function into a valid stat function for the table
#'
#' NA values are removed from the data
#'
#' Applying the function on a numerical vector should return one value
#'
#' Applying the function on a factor should return nlevels + 1 value, or one value per factor level
#'
#' See \code{parse_formula} for the usage for formulaes.
#' @param f The function to try to apply, or a formula combining two functions
#' @param x A vector
#' @export
#' @return The results for the function applied on the vector, compatible with the format of the result table
statify <- function(x, f) {
  # Discard NA values
  x <- stats::na.omit(x)

  ## Deprecate conditional formula
  if (length(f) == 3)                                                           # remove after 1.0
    f <- parse_formula(x, f)
  else
    f <- rlang::as_function(f)

  # Try f(x), silent warnings and fail with NA
  res <- tryCatch(f(x),
                  warning = function(e) suppressWarnings(f(x)),
                  error = function(e) NA)

  # If x is a factor and f(x) behaves as expected (nlevel + total value), return f(x), or apply f(x) on each level, or fail with n+1 NA
  if (is.factor(x)) {
    if (length(res) == nlevels(x) + 1) res
    else if (length(res) == 1) {
      c(res, lapply(levels(x), function(lvl) {
                      tryCatch(f(x[x == lvl]),
                               warning = function(e) suppressWarnings(f(x[x == lvl])),
                               error = function(e) NA)
                    }) %>% unlist)
    }
    else rep(NA, nlevels(x) + 1)
  # If it is a numeric, return f(x) if it behaves as expected (ONE value), or fail with NA
  } else {
    if (length(res) == 1) {
      if (is.numeric(res) | is.na(res)) res
      else as.character(res)
    }
    else NA
  }
}


#' Function to create a list of statistics to use in desctable
#'
#' This function takes a dataframe as argument and returns a list of statistcs in the form accepted by desctable.
#'
#' You can define your own automatic function, as long as it takes a dataframe as argument and returns a list of functions, or formulas defining conditions to use a stat function.
#'
#' @param data The dataframe to apply the statistic to
#' @return A list of statistics to use, assessed from the content of the dataframe
#' @export
stats_auto <- function(data) {
  data %>%
    lapply(is.numeric) %>%
    unlist() %>%
    any -> numeric

  data %>%
    lapply(is.factor) %>%
    unlist() %>%
    any() -> fact

  stats <- list("Min"  = min,
                "Q1"   = ~quantile(., .25),
                "Med"  = stats::median,
                "Mean" = mean,
                "Q3"   = ~quantile(., .75),
                "Max"  = max,
                "sd"   = stats::sd,
                "IQR"  = IQR)

  if (fact & numeric)
    c(list("N" = length,
           "%" = percent),
      stats)
  else if (fact & !numeric)
    list("N" = length,
         "%" = percent)
  else if (!fact & numeric)
    stats
}
