#' Prints accelerate raw feature dataset pre-checks
#'
#' Refer to Cookbook Feature for this print out workflow. Note: currently the
#' strict rule is that only a list can be accepted as input.
#'
#' @param l a list
#' @return invisible NULL
#' @author Siyu Sun
#' @family print checks
#' @examples
#' \dontrun{
#' printFeaturePreChecks(lmxburo)
#' }
#' @export
printFeaturePreChecks <- function(l) {

  if (length(l) < 1) {
    warning("The list is empty")
    return(invisible(NULL))
  }

  # 01-length of list
  cat("The length of the list is: ", length(l), fill=T)

  # 02-class of list elements
  classes <- unlist(sapply(l, class))
  cat("The original classes of list elements include: ", unique(classes), fill=T)

  # 03-count empty list elements and get rid of them  (classes "NULL")
  ind_null <- which(classes == "NULL")
  cat("Out of",length(l),",",length(ind_null),"are NULL (",length(ind_null)/length(l),").",fill=T)
  if (length(ind_null) > 0) {
    l <- l[-ind_null]
  }

  #04a-list elements are all vectors of length 1
  if (all(sapply(l, function(x) is.atomic(x) && length(x) <=1))) {
    cat("All list elements are vectors of length 1, now perform 1-vector-related checks", fill = T)
    vclasses <- sapply(l, class)
    cat("The list elements can have", length(unique(vclasses)), "unique types:", unique(vclasses), fill=T)
    vnames <- lapply(l, names)
    nnull_vnames <- sum(sapply(x, is.null))
    cat(length(l)-nnull_vnames, "list elements are named vectors.", fill = T)

    if (length(vclasses) <= 1 && (length(l)-nnull_vnames < 1)) {
      cat("All scalars are of the same type and unnamed scalars, please unlist to a vector...", fill = T)
    } else if (length(vclasses) > 1) {
      cat("Some scalars are of different types, try to coerce all to one type \n",
          "integer,logical or/and numeric -> numeric \n",
          "all others and character -> character", fill = T)
    } else if (length(l)-nnull_vnames > 0 && length(unique(unlist(vnames))) > 1) {
      cat("Some are named vector, and are of different names, please go to cookbook 1b for solution", fill = T)
    } else {
      cat("Named vector, if all with same name, combine them to a named vector of a data.frame with ncol==1", fill = T)
    }

  }

  # 04b-list elements are all vectors
  if (all(sapply(l, is.atomic))) {
    cat("All list elements are vectors, now perform vector-related checks", fill = T)
    vclasses <- sapply(l, class)
    cat("The list elements can have", length(unique(vclasses)), "unique types:", unique(vclasses), fill=T)
    vnames <- lapply(l, names)
    nnull_vnames <- sum(sapply(vnames, is.null))
    cat(length(l)-nnull_vnames, "list elements are named vectors.", fill = T)
  }




  # 04c-list elements are all data.frames
  if (all(sapply(l, class) %in% "data.frame" )) {
    cat("All list elements are data.frames, now perform df-related checks", fill = T)
    ncols <- sapply(l, ncol)
    cat("The list elements(data.frames) can have", length(unique(ncols)), "unique variables(ncols): ",
        unique(ncols), fill=T)
    unicolnames <- findAllVarNames(l)
    cat("In total", length(unique(unicolnames)), "unique variables(columns) names:", unicolnames, fill=T)
    nrows <- sapply(l, nrow)
    cat("The list elements(data.frames) can have", length(unique(nrows)), "unique entries(nrows): ",
        unique(nrows), fill=T)
  }

  return(invisible(NULL))
}





#' Prints accelerate pre-checks of a single feature
#'
#' Refer to Cookbook Feature for this print out workflow. Note: currently only
#' for lists with same data.frame variables/columns
#'
#' @param l a list, contains
#' @return invisible NULL
#' @author Siyu Sun
#' @family print checks
#' @examples
#' \dontrun{
#' printOneFeatureChecks(lmxburo)
#' }
#' @export
printOneFeatureChecks <- function(l)
{
  nncol <- ncol(l[[1]])
  # lclass <- vector("list", nncol)
  for(i in names(l[[1]])) {
    cat("column:", i, fill = T)
    print(table(unlist(lapply(l, function(x) x[,i])), useNA = "ifany"))
  }
  return(invisible(NULL))
}

