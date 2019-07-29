#' Insert a vector y inside another vector x at position
#'
#' The vectors in the y list will be inserted
#' at positions respectively *after* the x[position] element of x
#'
#' @param x A vector to be inserted into
#' @param y A vector or list of vectors to insert into x
#' @param position The position / vector of positions to insert vector(s) y in vector x
#' @return The combined vector
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


#' Set the "desctable" class to the passed object
#'
#' @param x Object to set the "desctable" class to
#' @return The object with the class "desctable"
set_desctable_class <- function(x) {
  class(x) <- "desctable"

  x
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
parse_formula <- function(x, f) {
  parse_f <- function(x) {
    if (length(x) == 1) as.character(x)
    else {
      if (as.character(x[[1]]) == "~") {
        paste0("if (", parse_f(x[[2]]), "(x)) ",
               "{",
               parse_f(x[[3]]),
               "}")
      } else if (as.character(x[[1]]) == "|") {
        paste0(parse_f(x[[2]]),
               "} else ",
               "{",
               parse_f(x[[3]]))
      } else if (as.character(x[[1]]) == "(") {
        parse_f(x[[2]])
      }
    }
  }

  eval(parse(text = parse_f(f)))
}


#' Build the header for pander
#'
#' @param head A headerList object
#' @return A names vector
head_pander <- function(head) {
  if (is.integer(head[[1]])) {
    head %>%
      names %>%
      lapply(function(x){c(x, rep("", head[[x]] - 1))}) %>%
      unlist()
  } else {
    paste(head %>%
            names() %>%
            lapply(function(x){c(x, rep("", attr(head[[x]], "colspan") - 1))}) %>%
            unlist(),
          head %>%
            lapply(head_pander) %>%
            unlist(),
          sep = "<br/>")
  }
}


#' Build the header for datatable
#'
#' @param head A headerList object
#' @return An htmltools$tags object containing the header
head_datatable <- function(head) {
  TRs <- list()

  while (is.list(head[[1]])) {
    TR <- purrr::map2(names(head), lapply(head, attr, "colspan"), ~htmltools::tags$th(.x, colspan = .y))

    TRs <- c(TRs, list(TR))
    head <- purrr::flatten(head)
  }

  c(TRs, list(purrr::map2(names(head), head, ~htmltools::tags$th(.x, colspan = .y))))
}


#' Build the header for dataframe
#'
#' @param head A headerList object
#' @return A names vector
head_dataframe <- function(head) {
  if (is.integer(head[[1]])) {
    head %>%
      names() %>%
      lapply(function(x){rep(x, head[[x]])}) %>%
      unlist()
  } else {
    paste(head %>%
            names() %>%
            lapply(function(x){rep(x, attr(head[[x]], "colspan"))}) %>%
            unlist(),
          head %>%
            lapply(head_pander) %>%
            unlist(),
          sep = " / ")
  }
}


#' Build header
#'
#' Take a desctable object and create a suitable header for the mentionned output.
#' Output can be one of "pander", "datatable", or "dataframe".
#'
#' @param desctable A desctable object
#' @param output An output format for the header
#' @return A header object in the output format
header <- function(desctable, output = c("pander", "datatable", "dataframe")) {
  desctable[-1] %>%
    flatten_desctable() %>%
    data.frame(check.names = F) %>%
    names() -> nm

  desctable <- desctable[-1]

  if (length(desctable) == 1) {
    if (output == "datatable") {
      c("\u00A0", nm) %>%
        lapply(htmltools::tags$th) %>%
        htmltools::tags$tr() %>%
        htmltools::tags$thead() %>%
        htmltools::tags$table(class = "display")
    } else c("\u00A0", nm)
  } else {
    head <- headerList(desctable)

    if (output == "pander") {
      c("\u00A0", head_pander(head) %>%
        paste(nm, sep = "<br/>"))
    } else if (output == "datatable") {
      head <- c(head_datatable(head), list(nm %>% lapply(htmltools::tags$th)))
      head[[1]] <- c(list(htmltools::tags$th(rowspan = length(head))), head[[1]])

      head %>%
        lapply(htmltools::tags$tr) %>%
        htmltools::tags$thead() %>%
        htmltools::tags$table(class = "display")
    } else if (output == "dataframe") {
      c("\u00A0", head_dataframe(head) %>% paste(nm, sep = " / "))
    }
  }
}


#' Build a header list object
#'
#' @param desctable A desctable
#' @return A nested list of headers with colspans
headerList <- function(desctable) {
  if (is.data.frame(desctable)) length(desctable)
  else {
    rec <- lapply(desctable, headerList)

    if (is.integer(rec[[1]])) attr(rec, "colspan") <- rec %>% unlist() %>% sum()
    else                      attr(rec, "colspan") <- rec %>% lapply(attr, "colspan") %>% unlist() %>% sum()

    rec
  }
}


#' Flatten a desctable to a dataframe recursively
#'
#' @param desctable A desctable object
#' @return A flat dataframe
flatten_desctable <- function(desctable) {
  if (is.data.frame(desctable)) desctable
  else {
    desctable %>%
      lapply(flatten_desctable) %>%
      Reduce(f = cbind)
  }
}
