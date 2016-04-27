#' Get nzv as a nice formatted data.frame
#'
#' This function is needed simply because I would like my own nzv and am too
#' lazy to type the same code every time.
#'
#' @param df the dataset as a data.frame or matrix
#' @param the_nzv default NULL, the already calculated nzv by \code{caret::nzv}
#' @param onlyzv logical, if TRUE, only zero variance will be returned
#' @param onlynzv logical, if TRUE, only nzv but not zv will be returned
#' @param verbose logical
#' @return a data.frame of customized nzv
#' @export
getnzvasdf <- function(df, the_nzv = NULL, onlyzv = F, onlynzv = T, verbose = T)
{
  if (is.null(the_nzv)) {
    df_nzv_all <- nzv(df, saveMetrics = T)
  } else {
    df_nzv_all <- the_nzv
  }
  df_nzv_all$featurename <- rownames(df_nzv_all)
  df_nzv_all <- df_nzv_all[, c(5, 1:4)]
  rownames(df_nzv_all) <- 1:nrow(df_nzv_all)

  if (onlyzv) {
    df_zv <- df_nzv_all[df_nzv_all$zeroVar, ]
    if (verbose) {
      cat("In total", nrow(df_zv), "(", nrow(df_zv)/nrow(df_nzv_all), ") features are constant (zero variance)", fill = T)
    }
    return(df_zv)

  } else if (onlynzv) {
    df_nzv <- df_nzv_all[!df_nzv_all$zeroVar & df_nzv_all$nzv, ]
    df_nzv <- df_nzv[order(df_nzv$percentUnique), ]
    if (verbose) {
      cat("In total", nrow(df_nzv), "(", nrow(df_nzv)/nrow(df_nzv_all), ") features are only near zero variance", fill = T)
    }
    return(df_nzv)

  } else {
    return(df_nzv_all)
  }

}


#' Check nzv
#'
#' This function is needed also because I would like to have the insight of what
#' nzv can contribute to feature selection / filtering. Some interesting topics:
#' do we always want to drop zv? when do we want to drop some nzv also?
#'
#' @param df the dataset as a data.frame or matrix
#' @param the_nzv default NULL, the already calculated nzv by \code{caret::nzv}
#' @param n first n features with high nzv
#' @param v_label vector of lable
#' @param verbose logical
#' @param ... other parameters
#' @return a list of features with high nzv checks
#' @export
checknzv <- function(df, the_nzv = NULL, n = min(10,round(ncol(df)/10)), v_label = f_train_label, verbose = T, ...)
{
  if (ncol(df) < 1 || nrow(df) < 1) {
    return(NULL)
  }

  if (is.null(n) || !is.numeric(n) || length(n) < 1) {
    n <- min(10,round(ncol(df)/10))
  }

  if (is.null(the_nzv)) {
    the_nzv <- getnzvasdf(df, onlynzv = T, verbose = verbose)
  }

  # only interest in the first n with high nzv in case of a lot of features
  the_nzv <- the_nzv[1:n, ]

  ltbl <- vector("list", nrow(the_nzv))

  
  ltbl <- lapply(the_nzv$featurename, function(x) getcrosstabledf(df = df, feature = x, v_label = v_label, breaks = n+1))
  # for (i in 1:nrow(the_nzv)) {
  #   ltbl[[i]] <- getcrosstabledf(df, feature = the_nzv$featurename[i], v_label = v_label, breaks = n + 1)
  #   if (verbose) {
  #     cat(rownames(the_nzv)[i], fill = T)
  #     print(summary(df[, the_nzv$featurename[i]]))
  #     print(ltbl[[i]])
  #   }
  # }
  return(ltbl)
}


#' Get Cross Table as customized data.frame
#'
#' When training model or examining features, one trick is to check the cross
#' table of one feature and the label column. Too lazy to do this every time.
#'
#' @param v_feature a feature vector
#' @param v_label a lable vector
#' @param breaks bar to avoid features with too many unique values, if exceed
#' the bar, \code{cut} the feature to n==breaks categories.
#' @return a data.frame
#' @export
getcrosstabledf <- function(df, feature, v_label, breaks = 20, useNA = "ifany") {

  v_feature <- df[, feature]
  stopifnot(length(v_feature) > 0, length(v_label) > 0)

  if (length(unique(v_feature)) > breaks) {
    v_feature <- cut(v_feature, breaks = breaks)
  }

  tbl <- table(v_feature, v_label, useNA = useNA)
  df <- as.data.frame(tbl)
  df <- reshape(df, timevar = colnames(df)[2], idvar = colnames(df)[1], direction = "wide")
  df$ratio <- df[, 2] / df[, 3]
  df$featurename <- feature

  return(df)
}
