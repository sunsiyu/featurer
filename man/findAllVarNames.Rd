% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list-helpers.R
\name{findAllVarNames}
\alias{findAllVarNames}
\title{Find all unique variable names with a list of list elements.}
\usage{
findAllVarNames(l, onlyunique = T)
}
\arguments{
\item{l}{a list}
}
\value{
a character vector
}
\description{
Find all unique variable names with a list of list elements (named vector,
named matrix, data.frame with column names, etc.). The union of all variable
names will be returned as a character vector. Note: only supply a list with
list elements of same class, otherwise NULL will be returned.
}
\seealso{
Other list helpers: \code{\link{findAllUniqueValues}},
  \code{\link{getVarNameFreq}}
}

