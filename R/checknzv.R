checknzv <- function(df_nzv,df, label = 1, ...) {
  
#   df_nzv_all <- nzv(df, saveMetrics = T)
#   df_zv <- df_nzv_all[df_nzv_all$zeroVar, ]
#   df_nzv <- df_nzv_all[!df_nzv_all$zeroVar & df_nzv_all$nzv, ]
#   
#   if (nrow(df_nzv) < 1) {
#     return(invisible(NULL))
#   }
#   df_nzv <- df_nzv[order(df_nzv$percentUnique), ]

  for (i in 1:nrow(df_nzv)) {
    print(summary(df[, rownames(df_nzv)[i]]))
    df_tbl <- as.data.frame(table(df[, rownames(df_nzv)[i]], useNA = "ifany"))
    # names(df_tbl) <- c("var", "label1", "label2")
    df_tbl <- df_tbl[order(df_tbl[,2], decreasing = T), ]
    print(head(df_tbl))
  }
  
  return(invisible(NULL))
}