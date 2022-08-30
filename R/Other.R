# Other Functions

## Working with GPS collar data ####
### Converting time zones to local time (Added 2022-08-25)
##' @description This function converts date-time information from a GPS collar from UTC time to your local time zone. This should be used when GPS collar data is not already converted and you need to keep separate "date" and "time" columns in your data.
##'
##' @title Convert Time Zones from GPS collar data
##'
##' @param ds The original GPS collar data.
##' @param date.col The column containing the original dates that need to be converted.
##' @param time.col The column containing the original times that need to be converted.
##' @param LocalTZ The time zone that you want to convert to. This must an R-recognized string for a given time zone. This function utilizes the with_tz function from the lubridate package. See the help for that package for details.
##'
##' @return Your original data frame with the new date and time columns appended to the end as LocalDate and LocalTime
##'
##' @seealso \code{\link[lubridate]{with_tz}}
##'
##' @keywords manip
##'
##' @concept collars
##'
##' @importFrom lubridate ymd hms ymd_hms year month day hour minute second with_tz
##' @export
##'
##' @example \dontrun{
##' # No example provided
##' }
timeConvert <- function(ds, date.col, time.col, LocalTZ){
  #ds <- bob
  #date.col <- "RTC.date"
  #time.col <- "RTC.time"
  #LocalTZ <- "America/Chicago"

  date1 <- lubridate::ymd(ds[,date.col])
  time1 <- lubridate::hms(ds[,time.col])

  datetime1 <- paste(lubridate::year(date1), lubridate::month(date1), lubridate::day(date1), lubridate::hour(time1), lubridate::minute(time1), lubridate::second(time1), sep = " ")
  datetime2 <- lubridate::ymd_hms(datetime1)

  tzconv <- lubridate::with_tz(datetime2, tzone = LocalTZ)

  date2 <- as.Date(tzconv)
  time2 <- paste(formatC(lubridate::hour(tzconv), width = 2, flag = "0"),
                 formatC(lubridate::minute(tzconv), width = 2, flag = "0"),
                 formatC(lubridate::second(tzconv), width = 2, flag = "0"), sep = ":")

  ds$LocalDate <- date2
  ds$LocalTime <- time2

  return(ds)
  rm(date1, date2, time1, time2, datetime1, datetime2, tzconv)
  #rm(ds, date.col, time.col, LocalTZ)
}
