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
statify <- function(x, f)
{
  UseMethod("statify", f)
}

#' @rdname statify
#' @export
statify.default <- function(x, f)
{
  x <- x %>% stats::na.omit()

  # Try f(x), silent warnings and fail with NA
  res <- tryCatch(x %>% f,
                  warning = function(e) suppressWarnings(x %>% f),
                  error = function(e) NA)

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
                               warning = function(e) suppressWarnings(f(x[x == lvl])),
                               error = function(e) NA)
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

#' @rdname statify
#' @export
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

#' Functions to create a list of statistics to use in desctable
#'
#' These functions take a dataframe as argument and return a list of statistcs in the form accepted by desctable.
#'
#' Already defined are
#' \enumerate{
#' \item stats_default with length, mean/\%, sd, med and IQR
#' \item stats_normal with length, mean/\% and sd
#' \item stats_nonnormal with length, median/\% and IQR
#' \item stats_auto, which picks stats depending of the data
#' }
#'
#' You can define your own automatic functions, as long as they take a dataframe as argument and return a list of functions or formulas defining conditions to use a stat function.
#'
#' @param data The dataframe to apply the statistic to
#' @return A list of statistics to use, potentially assessed from the dataframe
#' @export
stats_default <- function(data)
{
  list("N" = length,
       "Mean/%" = is.factor ~ percent | (is.normal ~ mean),
       "sd" = is.normal ~ sd,
       "Med" = is.normal ~ NA | median,
       "IQR" = is.factor ~ NA | (is.normal ~ NA | IQR))
}

#' @rdname stats_default
#' @export
stats_normal <- function(data)
{
  list("N" = length,
       "Mean/%" = is.factor ~ percent | mean,
       "sd" = stats::sd)
}

#' @rdname stats_default
#' @export
stats_nonnormal <- function(data)
{
  list("N" = length,
       "Median/%" = is.factor ~ percent | median,
       "IQR" = is.factor ~ NA | IQR)
}

#' @rdname stats_default
#' @export
stats_auto <- function(data)
{
  data %>%
    Filter(f = is.numeric) %>%
    lapply(is.normal) %>%
    unlist -> shapiro

  if (length(shapiro) == 0)
  {
    normal <- F
    nonnormal <- F
  }
  else
  {
    any(shapiro) -> normal
    any(!shapiro) -> nonnormal
  }

  any(data %>% lapply(is.factor) %>% unlist) -> fact

  if (fact & normal & !nonnormal)
    stats_normal(data)
  else if (fact & !normal & nonnormal)
    stats_nonnormal(data)
  else if (fact & !normal & !nonnormal)
    list("N" = length,
         "%" = percent)
  else if (!fact & normal & nonnormal)
    list("N" = length,
         "Mean" = is.normal ~ mean,
         "sd" = is.normal ~ sd,
         "Med" = is.normal ~ NA | median,
         "IQR" = is.normal ~ NA | IQR)
  else if (!fact & normal & !nonnormal)
    list("N" = length,
         "Mean" = mean,
         "sd" = stats::sd)
  else if (!fact & !normal & nonnormal)
    list("N" = length,
         "Med" = stats::median,
         "IQR" = IQR)
  else
    stats_default(data)
}
