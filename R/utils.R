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

#' Build header
#'
#' Take a desctable object and create a suitable header for the mentionned output.
#' Output can be one of "pander" or "datatable".
#'
#' @param desctable A desctable object
#' @param output An output format for the header
#' @return A header object in the output format
header <- function(desctable, output = c("pander", "datatable"))
{
  if (length(desctable) == 1)
    desctable %>% as.data.frame.desctable %>% names
  else
  {
    head <- headerList(desctable)

    if (output == "pander")
    {
      head_pander <- function(head)
      {
        if (head[[1]] %>% is.integer)
        {
          head %>% names %>% lapply(function(x){c(x, rep("", head[[x]] - 1))}) %>% unlist
        } else
        {
          paste(head %>% names %>% lapply(function(x){c(x, rep("", attr(head[[x]], "colspan") - 1))}) %>% unlist,
                head %>% lapply(head_pander) %>% unlist,
                sep = "<br/>")
        }
      }
      head_pander(head)
    }
  }
}

#' Build a header list object
#'
#' @param desctable A desctable
#' @return A nested list of headers with colspans
headerList <- function(desctable)
{
  if (desctable %>% is.data.frame)
  {
    length(desctable)
  }
  else
  {
    lapply(desctable, headerList) -> rec
    # desctable %>% lapply(headerList) -> rec
    if (is.integer(rec[[1]]))
      attr(rec, "colspan") <- rec %>% unlist %>% sum
    else
      attr(rec, "colspan") <- rec %>% lapply(attr, "colspan") %>% unlist %>% sum

    rec
  }
}

#' Flatten a desctable to a dataframe recursively
#'
#' @param desctable A desctable object
#' @return A flat dataframe
flatten_desctable <- function(desctable)
{
  if (desctable %>% is.data.frame)
    desctable
  else
    desctable %>% lapply(flatten_desctable) %>% dplyr::bind_cols()
}
