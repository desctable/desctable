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
    TR <- mapply(function(x, y) htmltools::tags$th(x, colspan = y), names(head), lapply(head, attr, "colspan"), SIMPLIFY = F)

    TRs <- c(TRs, list(TR))
    head <- purrr::flatten(head)
  }

  c(TRs, list(mapply(function(x, y) htmltools::tags$th(x, colspan = y), names(head), head, SIMPLIFY = F)))
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


#' build a header list object
#'
#' @param desctable a desctable
#' @return a nested list of headers with colspans
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
