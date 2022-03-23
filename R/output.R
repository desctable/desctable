##' Output a desctable to the desired target format
##'
##' Output a simple or grouped desctable to a different formats.
##' Currently available formats are\itemize{
##'   \item data.frame ("df")
##'   \item pander ("pander")
##'   \item datatable ("DT")
##' }
##'
##' All numerical values will be rounded to the digits argument.
##' If statistical tests are presents, p values below 1E-digits will be replaced with "≤ 1E-digits"
##' (eg. "≤ 0.01" for values below 0.01 when digits = 2)
##'
##' @title desc_output
##' @param desctable The desctable to output
##' @param target The desired target. One of "df", "pander", or "DT".
##' @param digits The number of digits to display. The p values will be simplified under 1E-digits
##' @param ... Other arguments to pass to \code{data.frame}, \code{pander::pander}, or \code{DT::datatable}
##' @return The output object (or corresponding side effect)
##' @export
##' @seealso \code{\link[DT]{datatable}}
##' @seealso \code{\link[pander]{pander}}
##' @family desc_table core functions
desc_output <- function(desctable, target = c("df", "pander", "DT"), digits = 2, ...) {
  switch(which.desctable(desctable),
         simple = switch(target,
                         df = output_df_simple(desctable, digits, ...),
                         pander = output_pander_simple(desctable, digits, ...),
                         DT = output_DT_simple(desctable, digits, ...),
                         stop("target must be one of \"df\", \"pander\", or \"DT\"")),
         grouped = switch(target,
                          df = output_df_grouped(desctable, digits, ...),
                          pander = output_pander_grouped(desctable, digits, ...),
                          DT = output_DT_grouped(desctable, digits, ...),
                          stop("target must be one of \"df\", \"pander\", or \"DT\"")),
         stop("Unexpected input. `desc_output` must be used on the output of `desc_table` or `desc_table` and `desc_tests`")) 
}


output_df_simple <- function(desctable, digits, ...) {
  # Fix variables
  variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "\u00A0\u00A0\u00A0\u00A0\\2", desctable$Variables)

  desctable$Variables <- NULL

  # Round to digits and set row names
  desctable %>%
    lapply(prettyNum, digits = digits) %>%
    lapply(gsub, pattern = "^NA$", replacement = "") %>%
    as.data.frame(check.names = F,
                  stringsAsFactors = F,
                  row.names = variables,
                  ...)
}


output_df_grouped <- function(desctable, digits, ...) {
  # Fix variables
  variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "\u00A0\u00A0\u00A0\u00A0\\2", desctable$.vars[[1]]$Variables)

  # Add tests and round p values
  table <- Reduce(desctable$.stats, f = cbind)
  if (desctable %>% utils::hasName(".tests")) {
    tests <- desctable$.tests[[1]]
    tests$p[tests$p < 10^-digits] <- 10^-digits
    prettyNum(tests$p, digits = digits) %>%
      gsub(pattern = "^NA$", replacement = "") %>%
      gsub(pattern = "^(0.0*1)$", replacement = "\u2264 \\1") -> tests$p

    table <- cbind(table, tests)
  }

  # Build header
  indices <- cumsum(c(1, sapply(desctable$.stats, length)))
  indices <- indices[1:length(indices) - 1]
  nmtoreplace <- names(table)[indices]
  names(table)[indices] <- paste0(names(desctable)[1], " = ", desctable[[1]], " (N = ", sapply(desctable$data, nrow), ")\n", nmtoreplace)

  # Round to digits and set row names
  table %>%
    lapply(prettyNum, digits = digits) %>%
    lapply(gsub, pattern = "^NA$", replacement = "") %>%
    as.data.frame(check.names = F,
                  stringsAsFactors = F,
                  row.names = variables,
                  ...)
}


output_pander_simple <- function(desctable, digits, ...) {
  # Fix variables
  variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "&nbsp;&nbsp;&nbsp;&nbsp;\\2", desctable$Variables)

  desctable$Variables <- NULL

  # Round to digits and set row names
  desctable %>%
    `row.names<-`(variables) %>%
    pander(digits = digits,
           justify = "left",
           missing = "" ,
           keep.line.breaks = T,
           split.tables = Inf,
           emphasize.rownames = F,
           ...)
}


output_pander_grouped <- function(desctable, digits, ...) {
  # Fix variables
  variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "&nbsp;&nbsp;&nbsp;&nbsp;\\2", desctable$.vars[[1]]$Variables)

  # Add tests and round p values
  table <- Reduce(desctable$.stats, f = cbind)
  if (desctable %>% utils::hasName(".tests")) {
    tests <- desctable$.tests[[1]]
    tests$p[tests$p < 10^-digits] <- 10^-digits
    prettyNum(tests$p, digits = digits) %>%
      gsub(pattern = "^NA$", replacement = "") %>%
      gsub(pattern = "^(0.0*1)$", replacement = "\u2264 \\1") -> tests$p

    table <- cbind(table, tests)
  }

  # Build header
  indices <- cumsum(c(1, sapply(desctable$.stats, length)))
  indices <- indices[1:length(indices) - 1]
  nmtoreplace <- names(table)[indices]
  names(table)[indices] <- paste0(names(desctable)[1], " = ", desctable[[1]], "</br>\n(N = ", sapply(desctable$data, nrow), ")</br>\n", nmtoreplace)

  # Round to digits and set row names
  table %>%
    `row.names<-`(variables) %>%
    pander(digits = digits,
           justify = "left",
           missing = "" ,
           keep.line.breaks = T,
           split.tables = Inf,
           emphasize.rownames = F,
           ...)
}


output_DT_simple <- function(desctable, digits, ...) {
  # Fix variables
  variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "&nbsp;&nbsp;&nbsp;&nbsp;\\2", desctable$Variables)
  variables <- gsub("\\*\\*(.*?)\\*\\*", "<b>\\1</b>", variables)

  desctable$Variables <- NULL

  # Round to digits and set row names
  desctable %>%
    sapply(function(x) !(is.integer(x) | is.character(x)), simplify = T) %>%
    which -> toRound

  DT::datatable(desctable,
                options = list(paging = F,
                               info = F,
                               search = list(),
                               dom = "Brtip",
                               fixedColumns = T,
                               fixedHeader = T,
                               buttons = c("copy", "excel")),
                rownames = variables,
                escape = F,
                style = "default",
                extensions = c("FixedHeader", "FixedColumns", "Buttons"),
                ...) %>%
    DT::formatRound(digits = digits, columns = toRound)
}


output_DT_grouped <- function(desctable, digits, ...) {
  # Fix variables
  variables <- gsub("\\*\\*(.*?)\\*\\*: \\*(.*?)\\*", "&nbsp;&nbsp;&nbsp;&nbsp;\\2", desctable$.vars[[1]]$Variables)
  variables <- gsub("\\*\\*(.*?)\\*\\*", "<b>\\1</b>", variables)

  # Add tests and round p values
  table <- Reduce(desctable$.stats, f = cbind)
  if (desctable %>% utils::hasName(".tests")) {
    tests <- desctable$.tests[[1]]
    tests$p[tests$p < 10^-digits] <- 10^-digits
    prettyNum(tests$p, digits = digits) %>%
      gsub(pattern = "^NA$", replacement = "") %>%
      gsub(pattern = "^(0.0*1)$", replacement = "\u2264 \\1") -> tests$p

    table <- cbind(table, tests)
  }

  # Build header
  header <- htmltools::tags$table(
    htmltools::tags$thead(
      htmltools::tags$tr(
        htmltools::tags$th(rowspan = 2, ""),
        mapply(htmltools::tags$th,
               colspan = sapply(desctable$.stats, length),
               paste0(names(desctable)[1], " = ", desctable[[1]], " (N = ", sapply(desctable$data, nrow) , ")"),
               SIMPLIFY = F)),
      htmltools::tags$tr(
        lapply(unlist(names(table)),htmltools::tags$th))),
    class = "display")

  # Round to digits and set row names
  table %>%
    sapply(function(x) !(is.integer(x) | is.character(x)), simplify = T) %>%
    which -> toRound

  DT::datatable(table,
                container = header,
                options = list(paging = F,
                               info = F,
                               search = list(),
                               dom = "Brtip",
                               fixedColumns = T,
                               fixedHeader = T,
                               buttons = c("copy", "excel")),
                rownames = variables,
                escape = F,
                style = "default",
                extensions = c("FixedHeader", "FixedColumns", "Buttons"),
                ...) %>%
    DT::formatRound(digits = digits, columns = toRound)
}
