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


#' Get Date Features from a vector of dates
#'
#' used lubridate functions.
#'
#' @param df a data.frame
#' @param colname the column name of the date column
#' @param curr_date default, \code{Sys.Date()}
#' @return a data.frame with expanded feature columns
#' @export
getDateFeatures <- function(df, colname, tz = "UTC", cutdate = Sys.Date()) {

  stopifnot(is.data.frame(df), colname %in% names(df), is.Date(as.Date(cutdate)))

  thetime <- ymd_hms(df[, colname], tz = tz)
  df$date_year <- year(thetime)
  df$date_month <- month(thetime)
  df$date_week <- week(thetime)
  df$date_yday <- yday(thetime)
  df$date_wday <- wday(thetime)
  df$date_day <- day(thetime)
  df$time_hour <- hour(thetime)
  df$time_minute <- minute(thetime)
  df$time_minute <- second(thetime)
  df$date_diffinyrs <- year(thetime) - year(cutdate)
  df$date_diffinmths <- difftime(thetime, cutdate, "mins")
  df$date_diffindays <- as.numeric(cutdate - as.Date(thetime))

  return(df)
}