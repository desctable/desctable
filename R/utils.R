#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL

#' Group a tbl by one or more variables.
#'
#' @name group_by
#' @keywords internal
#' @export
#' @importFrom dplyr group_by
#' @usage group_by(.data, ..., add = FALSE)
NULL

#' Insert a vector y inside another vector x at position
#'
#' @param x A vector
#' @param y A vector or list of vectors
#' @param position The position / vector of positions to insert vector(s) y in vector x
#' @return The combined vector
insert <- function(x, y, position)
{
  if (! y %>% is.list)
    y <- list(y)

  stopifnot(length(y) == length(position))

  result <- vector("list", 2 * length(position) + 1)
  old <- split(x, cumsum(seq_along(x) %in% (position + 1)))
  result[seq(from = 1, by = 2, length.out = length(old))] <- old
  result[c(F, T)] <- y

  unlist(result)
}

#' Test if distribution is normal
#'
#' @param x A numerical vector
#' @return A boolean
is.normal <- function(x)
{
  if (! x %>% is.numeric) 
    F
  else if (length(x) >= 30)
    stats::shapiro.test(x)$p.value > .1
  else
    F
}

#' Parse a formula
#'
#' Parse a formula defining the conditions to pick a stat/test
#'
#' Parse a formula defining the conditions to pick a stat/test
#' and return the function to use.
#' The formula is to be given in the form of
#' conditional ~ T | F
#' and conditions can be nested such as
#' conditional1 ~ (conditional2 ~ T | F) | F
#' The FALSE option can be omitted, and the TRUE can be replaced with NA
#'
#' @param x The variable to test it on
#' @param f A formula to parse
#' @return A function to use as a stat/test
#' @examples
#' # To use one column for different statistics depending on the variable
#' parse_formula(is.factor ~ percent | (is.normal ~ mean | median), factor(rep(LETTERS[1:3], 5)))
#' 
#' # To use one column selectively for a type of variable
#' parse_formula(is.normal ~ mean, rnorm(1000))
parse_formula <- function(x, f)
{
  parse_f <- function(x)
  {
    if (length(x) == 1)
      x %>% as.character
    else
    {
      if (x[[1]] %>% as.character == "~") 
      {
        paste0("if (", x[[2]] %>% parse_f, "(x)) ",
               "{",
               x[[3]] %>% parse_f,
               "}")
      } else if (x[[1]] %>% as.character == "|")
      {
        paste0(x[[2]] %>% parse_f,
               "} else ",
               "{",
               x[[3]] %>% parse_f)
      } else if (x[[1]] %>% as.character == "(")
      {
        x[[2]] %>% parse_f
      }
    }
  }
  parse(text = parse_f(f)) %>% eval
}
