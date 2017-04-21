#' Transform any function into a valid stat function for the table
#'
#' Transform a function into a valid stat function for the table
#' NA values are removed from the data
#' Applying the function on a numerical vector should return one value
#' Applying the function on a factor should return nlevels + 1 value, or one value per factor level
#' See `parse_formula` for the usage for formulaes.
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

  # Try f(x), silent warnings and fail with NA
  res <- tryCatch(x %>% f,
                  error = function(e) NA,
                  warning = function(e) suppressWarnings(x %>% f))

  # If x is a factor and f(x) behaves as expected (nlevel + total value), return f(x), or apply f(x) on each level, or fail with n+1 NA
  # If it is a numeric, return f(x) if it behaves as expected (ONE value), or fail with NA
  if (x %>% is.factor)
  {
    if (length(res) == nlevels(x) + 1)
      res
    else if (length(res) == 1 & res %>% is.numeric | res %>% is.na)
      c(res, lapply(levels(x), function(lvl)
                    {
                      tryCatch(f(x[x == lvl]),
                               error = function(e) NA,
                               warning = function(e) suppressWarnings(f(x[x == lvl])))
                    }) %>% unlist)
    else
      rep(NA, nlevels(x) + 1)
  } else
  {
    if (length(res) == 1 & res %>% is.numeric | res %>% is.na)
      res %>% as.double
    else
      NA
  }
}

statify.formula <- function(x, f)
{
  # if expression quoted with ~, evaluate the expression
  if (length(f) == 2)
    eval(f[[2]])
    # statify.default(x, eval(f[[2]]))
  # else parse the formula (cond ~ T | F)
  else
    statify.default(x, parse_formula(x, f))
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
  c(NA, summary(x) / length(x)) * 100
}
