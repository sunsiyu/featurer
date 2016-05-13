#' Get Max. Date Offset
#'
#' Calculate the maximum date offset between a date column in a data.frame and
#' the current date.
#'
#' @param df a data.frame
#' @param date_colname the column name of the date column
#' @param units expected units, "years" or all other units accepted by \code{difftime}
#' @param curr_date default, \code{Sys.Date()}
#' @return a numeric value
#' @export
getDateOffset <- function(df, date_colname, units = "years", format = "%d%m%Y", curr_date = Sys.Date()) {
  stopifnot(is.data.frame(df), date_colname %in% names(df), is.Date(curr_date))

  i <- which(names(df) == date_colname)
  val <- df[, i]
  if (!is.Date(val)) {
    val <- as.Date(val, format = format)
  }

  if (units == "years") {
    return(max(as.numeric(year(curr_date) - year(val))))
  } else {
    return(max(as.numeric(difftime(curr_date, val, units = units))))
  }
}