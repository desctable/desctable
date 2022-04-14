#' Insert a vector y inside another vector x at position
#'
#' The vectors in the y list will be inserted
#' at positions respectively *after* the x[position] element of x
#'
#' @param x A vector to be inserted into
#' @param y A vector or list of vectors to insert into x
#' @param position The position / vector of positions to insert vector(s) y in vector x
#' @return The combined vector
#' @keywords internal
insert <- function(x, y, position) {
  # y is supposed to be a list of vectors. If it is a single vector, make it a simple list containing that vector
  if (!is.list(y)) y <- list(y)

  # Stop if there is not as many positions as vectors to insert
  stopifnot(length(y) == length(position))

  # Create an empty return vector that will contain the partition of x and the inserts
  result <- vector("list", 2 * length(position) + 1)

  # Split x in groups between the insert positions
  old <- split(x, cumsum(seq_along(x) %in% (position + 1)))

  # Insert the x splits at odd positions in result
  result[seq(from = 1, by = 2, length.out = length(old))] <- old

  # Insert the y inserts at even positions in results
  result[c(F, T)] <- y

  # Return a simple vector
  unlist(result)
}

#' Is the object possibly a desctable?
#'
#' Check if the object is produced by desc_table.
#' Return a string:
#' - simple
#' - grouped
#' or FALSE if not a desctable
#'
#' @param desctable A potential desctable to check
#' @return The type of desctable or FALSE
#' @keywords internal
which.desctable <- function(desctable)
{
  attributes <- list()

  if (all(c("data", ".stats", ".vars") %in% names(desctable)))
    "grouped"
  else if (is.data.frame(desctable) & ("Variables" %in% names(desctable)))
    "simple"
  else
    ""
}

##' Quote an argument in a formula if it is not already a formula
##'
##' The argument must have been captured unevaluated first
##' 
##' @param deparsed The unevaluated argument (with rlang::enexprs)
##' @param orig The evaluated argument
##' @return A formula
formula_quote <- function(deparsed, orig)
{
  if (as.character(deparsed)[1] == "~")
    orig
  else
    as.formula(paste0("~", as.character(deparsed)))
}
