#' Create Statistical Summary Features
#'
#' create row-aggregated features rowsum, mean, median, mode, sd, max, min,
#' time: first last,
#' counts: >0, nonNA,
#'
#' @param m
#' @param names
#' @param verbose
createStatSumFeatures <- function(dt, cnames = NULL, newname = NULL, verbose = T) {

  stopifnot(!is.null(dt), is.data.frame(dt))
  cnames <- as.character(cnames)
  if (is.null(cnames) || length(cnames) < 1) {
    cnames <- names(dt)
  }
  setDT(dt)

  newname <- paste0(newname, c("sum", "mean", "median", "sd", "max", "min", "nonzeromin",
                               "ratiomaxmin", "ratiomaxnonzeromin"))

  dt[,newname := list(rowSums(.SD, na.rm = T),
                      rowMeans(.SD, na.rm = T),
                      apply(.SD, 1, median, na.rm = T),
                      apply(.SD, 1, sd, na.rm = T),
                      apply(.SD, 1, max, na.rm = T),
                      apply(.SD, 1, min, na.rm = T),
                      apply(.SD, 1, function(x) min(x[x > 0], na.rm = T)),
                      apply(.SD, 1, function(x) max(x, na.rm = T) / min(x, na.rm = T)),
                      apply(.SD, 1, function(x) max(x, na.rm = T) / min(x[x > 0], na.rm = T))),
     .SDcol = cnames, with = F]
  return(invisible(NULL))
}



#' Create Statistical Summary Features
#'
#' create row-aggregated features rowsum, mean, median, mode, sd, max, min,
#' time: first last,
#' counts: >0, nonNA,
#'
#' @param m
#' @param names
#' @param verbose
createBinaryFeatures <- function(dt, cnames = NULL, operator = "-", verbose = T) {

  stopifnot(!is.null(dt), is.data.frame(dt))
  cnames <- as.character(cnames)
  if (is.null(cnames) || length(cnames) < 1) {
    cnames <- names(dt)
  }
  setDT(dt)
  n <- length(cnames) - 1
  for (i in 1:n) {
    for (j in (i+1):(n+1)) {
      newname <- paste("diff", cnames[i], cnames[j], sep = "_")
      dt[, newname := apply(.SD, 1, diff), .SDcol = c(cnames[i], cnames[j]), with = F]
    }
  }

  return(invisible(NULL))
}


#' Create Statistical Summary Features
#'
#' create row-aggregated features rowsum, mean, median, mode, sd, max, min,
#' time: first last,
#' counts: >0, nonNA,
#'
#' @param m
#' @param names
#' @param verbose
createDiffFeatures <- function(dt, cnames = NULL, verbose = T) {

  stopifnot(!is.null(dt), is.data.frame(dt))
  cnames <- as.character(cnames)
  if (is.null(cnames) || length(cnames) < 1) {
    cnames <- names(dt)
  }
  setDT(dt)
  n <- length(cnames) - 1
  for (i in 1:n) {
    for (j in (i+1):(n+1)) {
      newname <- paste("diff", cnames[i], cnames[j], sep = "_")
      dt[, newname := apply(.SD, 1, diff), .SDcol = c(cnames[i], cnames[j]), with = F]
    }
  }

  return(invisible(NULL))
}



### logical computation
# create_feat_xor <- function(df) {
#   stopifnot(is.data.frame(df))
#   stopifnot(ncol(df) > 1)
#   stopifnot(is.numeric(as.matrix(df)))
#   n <- ncol(df) - 1
#   for (i in 1:n) {
#     for (j in (i+1):(n+1)) {
#       eval(parse(text = paste0('df$xor_', i, '_', j, ' <- as.integer(xor(df[, i], df[, j]))')))
#     }
#   }
#   return(df)
# }

### arithmetic computation
create_feat_diff <- function(df) {
  stopifnot(is.data.frame(df))
  stopifnot(ncol(df) > 1)
  stopifnot(is.numeric(as.matrix(df)))
  n <- ncol(df) - 1
  for (i in 1:n) {
    for (j in (i+1):(n+1)) {
      eval(parse(text = paste0('df$diff_', i, '_', j, ' <- df[, i] - df[, j]')))
    }
  }
  return(df)
}





