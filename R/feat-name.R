#' Generate Name Features from a character vector
#'
#' used stringr functions.
#'
#' @param df a data.frame
#' @param colname the column name of the date column
#' @return a data.frame with expanded feature columns
#' @export
getDateFeatures <- function(df, colname) {

  stopifnot(is.data.frame(df), length(colname) > 0, colname %in% names(df))

  if (is.null(df)) {
    return(NULL)
  }

  if (length(colname) > 1) {
    colname <- colname[1]
    warning("Multiple colname were supplied, only the first one was used!")
  }

  df$name_nchar <- nchar(df[, colname])
  df$name_nchar_onlyletter <- nchar(gsub("[^a-zA-Z0-9]", "", df[, colname]))
  df$name_nwords <- sapply(strsplit(df[, colname], " "), length)
  df$name_nnumber <- str_count(df[, colname], "[0-9]")
  df$name_nsymbol <- str_count(df[, colname], "[^a-zA-Z0-9]")
  df$name_nvowel <- str_count(df[, colname], "[aeiou]")
  df$name_nendeiy <- str_count(df[, colname], "(e|i|y)$")
  df$name_isempty <- as.numeric(is.na(df[, colname]) | df[, colname] == "" | df[, colname] == " ")  # TODO: regular expression for only " " characters

  return(df)
}