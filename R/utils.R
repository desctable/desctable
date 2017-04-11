#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL

#' Insert a vector y inside another vector x at position
#'
#' @param x A vector
#' @param y A vector or list of vectors
#' @param position The position / vector of positions to insert vector(s) y in vector x
#' @return The combined vector
insert <- function(x, y, position)
{
  if (!is.list(y))
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
  if (!is.numeric(x)) 
    F
  else if (length(x) >= 30)
    stats::shapiro.test(x)$p.value > .1
  else
    F
}

#' List the normal variables in the dataframe
#'
#' @param data A dataframe
#' @return A list of variable names
#' @export
list_normal <- function(data)
{
  data %>%
    purrr::keep(is.numeric) %>%
    purrr::map_lgl(is.normal) %>%
    purrr::keep(`==`,T) %>%
    names
}
