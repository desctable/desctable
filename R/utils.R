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

#' Test if distribution is parametric
#'
#' @param x A numerical vector
#' @return A boolean
is.param <- function(x)
{
  (shapiro.test(x)$p.value > .1) & (length(x) >= 30)
}

#' List the parametric variables in the dataframe
#'
#' @param data A dataframe
#' @return A list of variable names
#' @export
list_param <- function(data)
{
  data %>%
    purrr::keep(is.numeric) %>%
    purrr::map_lgl(is.param) %>%
    purrr::keep(`==`,T) %>%
    names
}
