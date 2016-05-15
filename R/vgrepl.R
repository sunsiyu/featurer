#' vectorize grepl
#'
#' @param vpattern vector of patterns
#' @param vx vector of x
#' @param operator either "or" or "and"
#' @return logical vector of same length of vx
#' @export
vgrepl <- function(vpattern, vx, operator = "or", ...) {

  stopifnot(length(vpattern) > 0, length(vx) > 0)

  vpattern <- as.character(vpattern)
  vx <- as.character(vx)

  lind <- lapply(vpattern, function(x) grepl(x, vx, ...))

  if (operator == "or") {
    res <- Reduce("|", lind)
  } else if (operator == "and") {
    res <- Reduce("&", lind)
  }

  return(res)
}
