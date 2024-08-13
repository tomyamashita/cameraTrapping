# Other Functions
## This script contains functions that are not related to working with camera data.
## This script contains the following functions:
  ## timeConvert
  ## prH0

################################################################################

## Working with GPS collar data
### Converting time zones to local time (Added 2022-08-25) ####
##' @description This function converts date-time information from a GPS collar from UTC time to your local time zone. This should be used when GPS collar data is not already converted and you need to keep separate "date" and "time" columns in your data.
##'
##' @title Convert Time Zones from GPS collar data
##'
##' @param ds The original GPS collar data.
##' @param dateCol The column containing the original dates that need to be converted.
##' @param timeCol The column containing the original times that need to be converted.
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
##' @examples \dontrun{
##' # No example provided
##' }
timeConvert <- function(ds, dateCol, timeCol, LocalTZ){
  #ds <- bob
  #dateCol <- "RTC.date"
  #timeCol <- "RTC.time"
  #LocalTZ <- "America/Chicago"

  date1 <- lubridate::ymd(ds[,dateCol])
  time1 <- lubridate::hms(ds[,timeCol])

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
  #rm(ds, dateCol, timeCol, LocalTZ)
}

## Statistical manipulatons
### Calculate the probability of a true null hypothesis (or alternatively the probability of a true alternative hypothesis)
##' @description This function calculates the probability of a true null hypothesis
##'
##' @title Calculate true null hypothesis
##'
##' @param p numeric. The p value from your analysis
##' @param prior numeric. The prior probability of a true null hypothesis.
##' 0.5 indicates an uninformative prior.
##' @param null logical. Do you want to test whether the null hypothesis is true or the alternative?
##'
##' @return value. The requested probability given the p value and prior.
##'
##' @keywords arith
##' @keywords utilities
##' @keywords htest
##'
##' @concept p value
##' @concept probability
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
prH0 <- function(p, prior = 0.5, null = TRUE){
  #p <- 0.0829
  #p <- 0.05
  #prior <- 0.50
  #null <- TRUE

  # Convert p value into a bayes factor
  ## Bayes factor for null hypothesis
  B <- -exp(1)*p*log(p)
  ## Bayes factor for alternative hypothesis
  BFB <- 1/B

  # Calculate the final oddds that the null/alternative hypothesis is true
  if(is.logical(null)){
    if(isTRUE(null)){
      message("Returning probability of a true null hypothesis")
      FO <- B*(prior/(1-prior))
    }else{
      message("Returning probability of a true alternative hypothesis")
      FO <- BFB*(prior/(1-prior))
    }
  }else{
    stop("null must be logical")
  }

  # Calculate the probability that the null/alternative hypothesis is true
  PR <- FO/(1+FO)

  return(PR)
  rm(B, BFB, FO, PR)
  #rm(p, prior)
}
