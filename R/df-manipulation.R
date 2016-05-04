#' Subset a data.frame by give column names
#'
#' Cut columns from data.frame that are not matched with given desirable column
#' names. Tip: always cut first before fill.
#'
#' @param df a data.frame
#' @param fillnames a character vector of kept column names
#' @return a data.frame
#' @author Siyu Sun
#' @export
cutColbyName <- function(df, fillnames) {
  stopifnot(inherits(df, "data.frame"))
  setDF(df)
  x <- which(!names(df) %in% fillnames)
  if (length(x) > 0) {
    setDT(df)
    df[, c(x) := NULL]
    setDF(df)
  }
  return(df)
}

#' Fill a data.frame by give column names
#'
#' Bind columns of NA_character_ to data.frame that are not matched with given
#' desirable column names. Tip: always cut first before fill.
#'
#' @param df a data.frame
#' @param fillnames a character vector of kept column names
#' @return a data.frame
#' @author Siyu Sun
#' @export
fillColbyName <- function(df, fillnames) {
  stopifnot(inherits(df, "data.frame"))
  setDF(df)
  x <- which(!fillnames %in% names(df))
  if (length(x) > 0) {
    setDT(df)
    x <- fillnames[x]
    df[, c(x) := NA_character_, with = F]
    setDF(df)
  }
  return(df)
}


#' Rename one column to another column's name.
#'
#' This function is needed when there is an obvious colname error/mispell with
#' some observations. This observation does not have a variable name like most
#' other observations, but has an abnormal one that can be confirmed to be the
#' same as another variable.
#'
#' @param df a data.frame
#' @param old a character vector of old name
#' @param new a character vector of new name
#' @return a data.frame
#' @author Siyu Sun
#' @export
renameOneCol <- function(df, old, new) {
  stopifnot(inherits(df, "data.frame"))
  setDF(df)
  old <- as.character(old)
  new <- as.character(new)
  stopifnot(length(old) == length(new))
  if (old %in% names(df)) {
    setDT(df)
    setnames(df, old = old, new = new)
    setDF(df)
  }
  return(df)
}

#' Convert all columns to character type.
#'
#' Convert all columns in a data.frame to character type. Simply does not make
#' sense to convert to other types, and good practice to keep the original types
#' somewhere.
#'
#' @param df a data.frame
#' @return a character data.frame
#' @author Siyu Sun
#' @export
convertAllToCharacter <- function(df) {
  stopifnot(inherits(df, "data.frame"))
  setDF(df)
  i <- 1:ncol(df)
  m <- apply(df[, i], 2, function(x) as.character(x))
  return(as.data.frame(t(m), stringsAsFactors = F))
}



