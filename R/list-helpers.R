#' Find all unique variable names with a list of list elements.
#'
#' Find all unique variable names with a list of list elements (named vector,
#' named matrix, data.frame with column names, etc.). The union of all variable
#' names will be returned as a character vector. Note: only supply a list with
#' list elements of same class, otherwise NULL will be returned.
#'
#' @param l a list
#' @return a character vector
#' @family list helpers
#' @export
findAllVarNames <- function(l, onlyunique = T) {
  stopifnot(is.list(l), !is.data.frame(l))
  if (all(sapply(l, class) == "matrix")) {
    lnames <- lapply(l, colnames)
  } else {
    lnames <- lapply(l, names)
  }
  varnames <- unlist(lnames)
  if (onlyunique) {
    return(unique(varnames))
  } else {
    return(varnames)
  }
}

#' Calculate variable name frequency with a list of list elements.
#'
#' Find out which variable names are most shared by all list elements, which are
#' not. If a variable name is with very low frequency, it should probably be
#' regarded (but check for exceptions).
#'
#' @param l a list
#' @return a data.frame with two columns: varnames and Freq
#' @family list helpers
#' @export
getVarNameFreq<- function(l) {
  stopifnot(is.list(l), !is.data.frame(l))
  varnames <- findAllVarNames(l, onlyunique = F)
  res <- as.data.frame(table(varnames))
  res$varnames <- as.character(res$varnames)
  res <- res[order(-res$Freq), ]
  return(res)
}


#' Find all unique values within each column of one data.frame
#'
#' Find all unique values within each column of one data.frame. The union of all
#' values will be returned as a character vector. Note: only supply a list with
#' list elements of same class, otherwise NULL will be returned.
#'
#' @param l a list
#' @return a character vector
#' @family list helpers
#' @export
findAllUniqueValues <- function(l, n = 1, onlyunique = T) {
  stopifnot(is.list(l), !is.data.frame(l))

  if (n < 1) {
    return(NULL)
  }

  varnames <- unlist(lnames)
  if (onlyunique) {
    return(unique(varnames))
  } else {
    return(varnames)
  }
}