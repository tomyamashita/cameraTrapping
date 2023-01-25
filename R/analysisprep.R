# Camera Data Analysis Preparation functions ####
## This script contains functions for prepping and running camera specific analyses
## This script includes the following functions:
  ## actFun
  ## calculateEvents
  ## occFun (Deprecated)
  ## summarizeEvents

################################################################################

### Animal Diel Activity (Added 2022-08-25) ####
##' @description A function for calculating diel activity for selected species. The function can optionally also split your data based on a user-defined column.
##'
##' @title Animal Diel Activity
##'
##' @param x data.frame. This should be the output from running the APFun_env function.
##' @param split Logical. Whether you want to split your data by a column in the data frame. This defaults to FALSE.
##' @param splitcol (Optional). Defaults to NULL. Which column to use to split the data. Only required if split=T.
##' @param species String. Which species do you want to run diel activity on? Use form c("Species 1", "Species 2", "etc.").
##' @param bw Numeric or NULL. Do you want to specify a bandwidth for the kernel density estimation of the activity distribution? The default is NULL where the function with calculate bandwidth internally.
##' @param rep Numeric. The number of bootstraps that should be used to estimate the confidence intervals of the diel activity distribution.
##'
##' @return data:
##' These are the data tables for each species (and split) used to produce the activity distribution.
##' @return activity:
##' These are the estimations of diel activity for each species.
##'
##' @references Rowcliffe, J. M., R. Kays, B. Kranstauber, C. Carbone, and P. A. Jansen. 2014. Quantifying levels of animal activity using camera trap data. Methods in Ecology and Evolution 5:1170-1179. 10.1111/2041-210x.12278
##'
##' Frey, S., J. T. Fisher, A. C. Burton, and J. P. Volpe. 2017. Investigating animal activity patterns and temporal niche partitioning using camera-trap data: challenges and opportunities. Remote Sensing in Ecology and Conservation 3:123-132. 10.1002/rse2.60
##'
##' Ramesh, T., R. Kalle, K. Sankar, and Q. Qureshi. 2012. Spatio-temporal partitioning among large carnivores in relation to major prey species in Western Ghats. Journal of Zoology 287:269-275. 10.1111/j.1469-7998.2012.00908.x
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso activity package: \url{https://cran.r-project.org/web/packages/activity/index.html}, \code{\link[activity]{activity}}
##'
##' \code{\link{calculateEvents}}
##'
##' @keywords manip
##' @keywords univar
##'
##' @concept camera trapping
##' @concept diel activity
##'
##' @importFrom activity fitact
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
actFun <- function(x, split=F, splitcol=NULL, species, bw = NULL, rep = 999){
  #x <- AP2_1min$FM1847
  #split <- F
  #splitcol <- "timeperiod"
  #species <- c("bobcat", "coyote")
  #bw = NULL
  #rep = 99

  if(split==T){
    AP <- split(x, as.factor(x[,splitcol]))
  }else{
    AP <- list(all = x)
  }

  #require(activity)

  specs_all <- lapply(AP, function(x){subset(x, x$Species %in% species)})
  specs <- lapply(specs_all, function(x){split(x, as.factor(x[,"Species"]))})

  act <- lapply(specs, function(X){lapply(1:length(X), function(s){
    y <- X[[s]]
    if(is.null(splitcol)==T){
      print(paste("Started ", names(X)[s], " at ", Sys.time(), sep = ""))
    }else{
      print(paste("Started ", names(X)[s], " for group ", unique(y[,splitcol]), " at ", Sys.time(), sep = ""))
    }
    if(is.null(bw)==T){
      if(nrow(y)>=100){
        activity::fitact(y$time_radians, sample = "model", reps = rep, bw = NULL, adj = 1.5)
      }else{
        activity::fitact(y$time_radians, sample = "data", reps = rep, bw = NULL, adj = 1.5)
      }
    }else{
      if(nrow(y)>=100){
        activity::fitact(y$time_radians, sample = "model", reps = rep, bw = bw, adj = 1.5)
      }else{
        activity::fitact(y$time_radians, sample = "data", reps = rep, bw = bw, adj = 1.5)
      }
    }
  })})
  act2 <- lapply(1:length(act), function(y){names(act[[y]]) <- names(specs[[y]]); return(act[[y]])})
  names(act2) <- names(specs)

  return(list(data = specs, activity = act2))

  rm(AP, specs_all, specs, act, act2)
  #rm(x, split, splitcol, species, bw, rep)
}

### From a dataorganize file, create a usable output (Added 2022-08-25, Modified 2022-08-31) ####
##' @description Function for calculating the number of events from raw camera trap images and adding camera/site-specific information to the output.
##'
##' @title Calculate number of events from a file organized like a dataOrganize file.
##'
##' @param do data frame. A DataOrganize file produced by the DataOrganize program or the dataOrganize function in this package.
##' @param envdata Environmental variables data frame. This file must have header called "Camera" containing the list of cameras.
##' @param sort.col string. Column that you want to sort pictures by to create independent events. This should be one of c("Camera", "Site", "Station") depending on your wording for sites. This defaults to "Camera".
##' @param exclude string. species to exclude from the output data frame. Use c() to specify species. This defaults to excluding ghosts. If you want keep all items use c("").
##' @param start_date string. Start date for the AllPictures file. If you want all data, set this to an arbitrarily early date.
##' @param end_date string. End date for pictures. This defaults to the current date.
##' @param interval integer. Time in seconds between pictures for an independent event. See Details for some normal interval times. This is only necessary for the main function. It defaults to no interval.
##' @param all.pics Logical. Whether or not you want to return all the pictures or just independent events. If all.pics = TRUE, all pictures will be returned with an index number associated with independent events. This defaults to FALSE.
##'
##' @details Suggested interval times: 61 = 1 minute, 1801 = 30 minutes. 3601 = 1 hour
##'
##' One second should be added to the total to ensure the full interval is included.
##'
##' When interval is set to NULL, the function will use an interval of 1 second and therefore does not summarize any events.
##'
##' @return A data frame of camera data. Output varies depending on whether or not all.pics is true or false.
##' @return all.pics = FALSE:
##' Returns a data frame of only independent events at the specified interval.
##' @return all.pics = TRUE:
##' Returns a data frame of all pictures from the input file. A index number is added associated with independent events.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{doTimelapse}}, \code{\link{doFolder}}
##'
##' \code{\link{actFun}}, \code{\link{summarizeEvents}}
##'
##' @keywords datagen
##'
##' @concept camera trapping
##' @concept DataOrganize
##'
##' @importFrom dplyr arrange summarise group_by
##' @importFrom pbapply pbsapply
##' @importFrom zoo na.locf
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
calculateEvents <- function(do, envdata, sort.col="Camera", exclude=c("ghost"), start_date, end_date=Sys.Date(), interval=NULL, all.pics=FALSE){
  # This function takes a DataOrganize file and changes it into a form that is usable for analyses in R
  # Inputs:
  ## do = file produced by Dataorganize program or dataOrganize function
  ## envdata = environmental variables file. This file must have header called "Camera" containing the list of cameras
  ## sort.col = Column which you want to sort your pictures by. It should most likely either be site or camera. Defaults to camera
  ## exclude = The species you want to subset out so that you don't get information on them. Defaults to ghost
  ## start_date = start date for the AllPictures file
  ## end_date = end date for pictures. Defaults to current date
  ## interval = time in seconds between pictures for an independent event. Defaults to none
  ## all.pics = Logical on whether you want all the pictures or just the independent events
  # Outputs:
  ## A data frame containing independent events in date-time format + camera/site specific variables

  # Defining an interval if interval left as default
  if(is.null(interval)==TRUE){
    interval <- 1
  }

  # Formatting the columns in the AllPictures file
  if(ncol(do)==9){
    colnames(do) <- c("Camera","Species", "N.Individuals", "Year","Month","Day","Hour","Minute","Second")
  }else if(ncol(do)==10){
    colnames(do) <- c("Camera","Species", "N.Individuals", "Year","Month","Day","Hour","Minute","Second","Serial")
  }else{
    stop("Your DataOrganize file does not have the correct number of columns. Check your input file.")
  }

  if(isTRUE(is.na(exclude))){
    x1 <- do
  }else{
    x1=subset(do, !(Species %in% exclude))
  }
  x1$Month_Day <- paste(formatC(x1[,"Year"], width = 4, format = "d", flag = "0"),
                        formatC(x1[,"Month"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Day"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Hour"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Minute"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Second"], width = 2, format = "d", flag = "0"), sep = "")
  x1$DateTimeOriginal <- as.POSIXct(strptime(x1$Month_Day,'%Y%m%d%H%M%S'))
  x1$Date <- as.POSIXct(strptime(x1$Month_Day,'%Y%m%d'))
  x1$hms <- format(x1$DateTimeOriginal,format="%H:%M:%S")
  x1$delta.time.secs[1] <- 0

  # Adding in environmental data and time of day data
  if(!any(colnames(envdata)=="Camera")){
    stop("Your EnvData file must contain a column called 'Camera'.")
  }

  x2 <- merge.data.frame(x1, envdata, by = "Camera")
  if(nrow(x2)==0){
    stop("Something went wrong merging the camera data to the envdata. Check your column and camera names in your envdata file.")
  }
  x2$time_numeric <- (as.numeric(x2$Hour)*3600 + as.numeric(x2$Minute)*60 + as.numeric(x2$Second))/(86400)
  x2$time_radians <-2*pi*x2$time_numeric

  # Subsetting the data by date
  x2a <- subset(x2,Date >= start_date)
  x2b <- subset(x2a,Date <= as.character(end_date))
  if(nrow(x2b)==0){
    stop("The specified date range is outside the range of the data")
  }
  x3 <- x2b

  # Calculating the time difference between pictures
  x3["Sort"] <- x3[sort.col]
  x4=dplyr::arrange(x3, Sort, Species, Month_Day)
  x4$delta.time.secs <- pbapply::pbsapply(1:(nrow(x4)), FUN = function(i){
    ifelse(i-1==0, interval,
           ifelse(x4$Species[i]==x4$Species[i-1],
                  difftime(x4$DateTimeOriginal[i], x4$DateTimeOriginal[i-1], units = "secs"), interval))
    #rm(i)
  })
  x4$Ident <- seq(1,nrow(x4))

  # Identifying and subsetting out independent events
  x5 <- x4[!(x4$delta.time.secs < interval),]
  x5$Ident2 <- seq(1, nrow(x5))
  x5a <- merge.data.frame(x5, x4, by="Ident", all.y=TRUE, sort.y=TRUE)
  x5b <- data.frame(x5a[,c(1,(ncol(x5)+1):ncol(x5a))], Ident2 = zoo::na.locf(x5a$Ident2, fromLast=FALSE))
  x5c <- dplyr::summarise(dplyr::group_by(x5b, Ident2), Individuals = max(`N.Individuals.y`))

  # Sorting the columns and removing unnecessary columns
  if(all.pics==TRUE){
    x5b2 <- x5b[,1:ncol(x5)]
    x5b3 <- x5b2[,c(3:ncol(x5b2)-1,1,ncol(x5b2))]
    colnames(x5b3) <- colnames(x5)
    x6 <- merge.data.frame(x5b3, x5c, by="Ident2", all.x=TRUE, sort.x=TRUE)
    x7 <- x6[,c(1,2,seq(2,length(envdata))+14,3,length(envdata)+length(x1)+5,11,12,13,14,length(envdata)+length(x1)+1,length(envdata)+length(x1)+2)]
    rm(x5b2, xb53)
  }else{
    x6 <- merge.data.frame(x5, x5c, by="Ident2", all.x=TRUE, sort.x=TRUE)
    x7 <- x6[,c(2,seq(2,length(envdata))+14,3,length(envdata)+length(x1)+5,11,12,13,14,length(envdata)+length(x1)+1,length(envdata)+length(x1)+2)]
  }
  return(x7)
  rm(x1, x2, x2a, x2b, x3, x4, x5, x5a, x5b, x5c, x6, x7)
  #rm(do, envdata, sort.col, exclude, start_date, end_date, interval, all.pics)
}

### Setting up for occupancy modelling from an APFun_env output (Added 2022-08-25) ####
##' @description This function modifies data for use in occupancy modelling using Unmarked or a separate program, I think.
##'
##' @title (Deprecated) Occupancy Analysis Data Setup
##'
##' @param x A data frame produced by the APFun_env function available in this package.
##' @param ct A CT Table following the format from the package camtrapR.
##' @param unit The temporal unit for dividing up your data. This must be in units "days".
##' @param subset A character vector indicating which species to prepare data for.
##' @param stationCol String. A character string for the column used to identify sites.
##' @param sessionCol Character. A character string for the column used to identify sessions. This is currently untested. Use with caution.
##' @param ct_probs Logical. Does the CT table have problems?
##' @param count Logical. Should presence/absence (FALSE) or counts (TRUE) be outputted?
##'
##' @details Please note that I personally have never tested the output of this function with an occupancy model although I am told that it works. Please use this with caution and test it properly before using it for any real research.
##'
##' This function is now deprecated due to the existence of \code{\link{summarizeEvents}}. That function does everything this one does in a better and more effecient way.
##'
##' @return A LIST object by species chosen in the subset input with a table of sites (rows) and dates (columns).
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{calculateEvents}}
##'
##' \code{\link{summarizeEvents}}
##'
##' @keywords datagen
##' @keywords manip
##'
##' @concept camera trapping
##' @concept occupancy analysis
##'
##' @importFrom camtrapR cameraOperation
##' @importFrom stats aggregate reshape
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
occFun <- function(x, ct, unit, subset, stationCol, sessionCol=NULL, ct_probs=T, count=F){
  #x <- AP2_swift
  #ct <- CT
  #unit <- "1 days"
  #subset <- c("rio_grande_turkey")
  #count <- F
  #ct_probs <- T
  #stationCol <- "Site"
  #sessionCol <- NULL

  #require(camtrapR)
  #require(lubridate)

  if(isFALSE(grepl("day", unit))){
    stop("Use days")
  }

  unit1 <- ceiling(as.numeric(sub("days", "", unit, ignore.case = T))/2)

  # Dealing with active and inactive camera trap nights
  if(is.logical(ct_probs)){
    if(isTRUE(ct_probs)){
      if(is.null(sessionCol)==T){
        camop <- camtrapR::cameraOperation(ct, stationCol = stationCol, cameraCol = "Camera",
                                           byCamera = F, allCamsOn = F, camerasIndependent = F,
                                           setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T)
      }else{
        warning("This function is untested when specifying sessionCol other than NULL. The sortCol may not get called properly.")
        camop <- camtrapR::cameraOperation(ct, stationCol = stationCol, cameraCol = "Camera", sessionCol = sessionCol,
                                           byCamera = F, allCamsOn = F, camerasIndependent = F,
                                           setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T)
      }
    }else if(isFALSE(ct_probs)){
      warning("When there are no problems in the ct table, the ct table is only used to gather the site list and range of dates.")
      if(is.null(sessionCol)==T){
        camop <- camtrapR::cameraOperation(ct, stationCol = stationCol, cameraCol = "Camera",
                                           byCamera = F, allCamsOn = F, camerasIndependent = F,
                                           setupCol = "Setup_date", retrievalCol = "Retrieval_date")
      }else{
        warning("This function is untested when specifying sessionCol other than NULL. The sortCol may not get called properly.")
        camop <- camtrapR::cameraOperation(ct, stationCol = stationCol, cameraCol = "Camera", sessionCol = sessionCol,
                                           byCamera = F, allCamsOn = F, camerasIndependent = F,
                                           setupCol = "Setup_date", retrievalCol = "Retrieval_date")
      }
    }
  }else{
    stop("ct_probs should be logical (T,F)")
  }

  camop[camop==0] <- -5
  camop[camop==1] <- 0

  camopt <- t(camop)

  dates <- data.frame(Date = as.Date(colnames(camop)))
  dates$Date2 <- rep(seq.Date(min(dates$Date), max(dates$Date), by = unit), each = as.numeric(sub("days", "", unit, ignore.case = T)))[1:nrow(dates)]

  camdate <- data.frame(sortCol = rep(rownames(camop), each = ncol(camop)), Date = rep(dates[,1], times = nrow(camop)), Date2 = rep(dates[,2], times = nrow(camop)))
  camdate$camdate <- with(camdate, paste(sortCol, Date, sep = "_"))

  if(ncol(camop) * nrow(camop) != nrow(camdate)){
    stop("# Cameras * # dates does not equal total rows. Check the function. This error shouldn't be possible.")
  }

  active <- stats::aggregate(camopt, by = list(Date2 = dates$Date2), sum)
  rownames(active) <- active[,1]
  activet <- t(active[,2:ncol(active)])
  activet[activet<=(-5*unit1)] <- NA
  activet[activet>(-5*unit1)] <- 0

  print(paste("There should be ", dim(activet)[1], " rows and ", dim(activet)[2], " columns in each output.", sep = ""))

  # Now for the camera data
  x$sortCol <- x[,stationCol]
  x$Date <- as.Date(x$Date)
  x$camdate <- with(x, paste(sortCol, Date, sep = "_"))
  x1 <- split(x, factor(x$Species))
  x2 <- x1[subset]

  x3 <- lapply(x2, function(a){
    a1 <- stats::aggregate(Individuals~camdate, data = a, sum)
    a2 <- merge.data.frame(camdate, a1, by = "camdate", all.x = T)
    a2$Individuals[is.na(a2$Individuals)] <- 0

    a3 <- stats::aggregate(Individuals~sortCol+Date2, data = a2, sum)

    a4 <- stats::reshape(a3, direction = "wide", timevar = "Date2", idvar = "sortCol")
    row.names(a4) <- a4[,1]
    a5 <- as.matrix(a4[,-1])
    colnames(a5) <- as.character(unique(a3$Date2))

    if(dim(a5)[1]!=dim(activet)[1] | dim(a5)[2]!=dim(activet)[2]){
      warning("Your camera data and ct table have different numbers of sites and dates")
    }

    if(is.logical(count)==T){
      if(isFALSE(count)){
        a5 <- ifelse(a5>0,1,0)
      }
    }else{
      stop("The count variable must be a logical (T,F)")
    }

    length(a5[a5>0])

    if(ct_probs==T){
      comb <- a5 + activet
      print(paste(length(comb[comb>0 & !is.na(comb)]), " camera trap nights had a detection of a ", unique(a$Species), sep = ""))
      if(length(comb[comb>0 & !is.na(comb)]) < length(a5[a5>0])){
        warning(paste("For ", unique(a$Species), ", inactive camera trap nights removed some detections. This is what this function is supposed to do.", sep = ""))
      }else if(length(comb[comb>0 & !is.na(comb)]) > length(a5[a5>0])){
        warning(paste("For ", unique(a$Species), ", Something strange happened. Detections were added when accounting for inactive camera trap nights."))
      }
      return(comb)
    }else{
      print(paste(length(a5[a5>0]), " camera trap nights had a detection of a ", unique(a$Species), sep = ""))
      return(a5)
    }
    rm(a1,a2,a3,a4,a5,comb)
    rm(a)
  })

  if(is.logical(count)==T){
    if(isFALSE(count)){
      print("Presence/absence data is outputted")
    }else{
      print("Abundance data is outputted")
    }
  }else{
    stop("The count variable must be a logical (T,F)")
  }

  return(x3)

  rm(unit1, camop, camopt, dates, camdate, active, activet, x1, x2, x3)
  #rm(x, ct, unit, subset, stationCol, sessionCol, ct_probs, count)
}

### Summarize number of events by a time period (Added 2023-01-20) ####
##' @description Summarize raw camera trap events by a user-specified time period for various analyses
##'
##' @title Summarize camera trap events, including setting up for regression analyses, occupancy models, n-mixture models
##'
##' @param x A data frame produced by the APFun_env function available in this package.
##' @param ct A CT Table following the format from the package camtrapR.
##' @param unit The temporal unit used to summarize the data. This should a unit >= 1 day. Weeks, months, or years can all be called using this argument.
##' See details for how this function handles summarizing units greater than days.
##' @param include String of characters. This should be the names of the species that you want to include in this analysis.
##' @param camOP Arguments passed to \code{\link[camtraR]{cameraOperation}}. The most important ones of these are
##' stationCol, setupCol, and retrievalCol. hasProblems is often used as well.
##' If multiple cameras per site, you must specify cameraCol, byCamera, allCamsOn, and camerasIndepedent.
##' See \code{\link[camtrapR]{cameraOperation}} for details.
##' @param out_form Should the data output in long format, wide format or both. Acceptable inputs are c("all", "l", "long", "w", "wide"). The maximum length of this input is 2. Do not include "all" if providing multiple arguments.
##' @param out_data Should the data output number of detections (DE), abundance (AB; number of individuals), or presence/absence (PA). Acceptable inputs are c("all", "DE", "detections", "AB", "abundance", "PA", "presence"). The maximum length of this input is 3. Do not include "all" if providing multiple arguments.
##' @param out_correction Should the data apply no correction for active camera trap nights (raw), remove all data when camera traps were inactive for more than have the time interval (rm; Unit), or calculate an adjusted number of detections/abundance for each time interval (pu; events/activenights * totalnights). Acceptable inputs are c("all", "none", "raw", "rm", "rmInactive", "pu", "perUnit"). The maximum length of this input is 3. Do not include "all" if providing multiple arguments.
##'
##' @details Using units other than days: If you use units other than days, the function will start the time interval on the first day of the period.
##' For example, if "1 month" is specified as the unit, the function will start each period on the 1st of the month.
##' If "1 week" is specified as the unit, the function will start each period on Sunday.
##' If "1 year" is specified as the unit, the function will start on the January 1.
##'
##' This function deprecates the \code{\link{occFun}} function. All capabilities from that function are implemented here in a simpler, faster, and more versatile way. Avoid using occFun in favor of this function in the future.
##'
##' A note about the outputs: This function will output up to 18 items (2 forms, 3 datas, and 3 corrections) and does a combination of each. Currently it is not possible to request specific outputs of multiple forms, datas, and corrections.
##'
##' @return Usually a 2-leveled list with species (chosen using the include argument) as the top level and requested outputs (chosen using out_form, out_data, and out_correction) below it.
##' However, this function will coerce any list that is length 1 to a data.frame, simplifying the return.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{calculateEvents}}
##'
##' \code{\link{occFun}}
##'
##' @keywords datagen
##' @keywords manip
##' @keywords univar
##'
##' @concept camera trapping
##' @concept occupancy analysis
##' @concept n-mixture model
##' @concept regression
##'
##' @importFrom camtrapR cameraOperation
##' @importFrom tidyr pivot_longer
##' @importFrom lubridate ymd floor_date
##' @importFrom dplyr summarise group_by n summarise_all
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
summarizeEvents <- function(x, ct, unit, include, camOP, out_form = "all", out_data = "all", out_correction = "all"){
  #x <- AP2
  #ct <- cttable
  #unit <- "1 month"
  #include <- c("bobcat", "coyote")
  #camOP <- list(stationCol = "Camera", setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T, cameraCol = "Camera", byCamera = F, allCamsOn = F, camerasIndependent = F)
  #out_form <- "all"
  #out_data <- "all"
  #out_correction <- "all"

  print(paste("This function started at ", Sys.time(), ". Running camtrapR...", sep = ""))

  # Calculating number of camera trap nights
  if(!is.list(camOP)){
    stop("camOP must be a list of arguments used in the camtrapR::cameraOperation function.")
  }

  if(!any(grep("day", unit, ignore.case = T), grep("week", unit, ignore.case = T), grep("month", unit, ignore.case = T), grep("year", unit, ignore.case = T))){
    stop("You chose an incorrect unit. The unit must be a time interval of at least 1 day.\n(e.g., '3 days', '1 week', '2 months', '10 years').")
  }

  camOP[["CTtable"]] <- ct

  camop <- do.call(camtrapR::cameraOperation, camOP)

  print(paste("camtrapR completed at ", Sys.time(), ". Finalizing outputs...", sep = ""))

  camopt <- t(camop)
  c1 <- data.frame(tidyr::pivot_longer(data.frame(Date = row.names(camopt), camopt), cols = 1:ncol(camopt)+1, names_to = "Site", values_to = "op"))
  c1$Date <- lubridate::ymd(c1$Date)
  c1$Site <- sub("[.]", "-", c1$Site)

  ## Uses user-specified unit to round dates back to desired unit. Months start on first of the month, weeks on Sundays
  c1$Interval <- lubridate::floor_date(c1$Date, unit = unit)

  ## Add a sorting column to the data
  c1$camdate <- with(c1, paste(Site, Date, sep = "_"))
  c1$intdate <- with(c1, paste(Site, Interval, sep = "_"))

  c2 <- suppressMessages(data.frame(dplyr::summarise(dplyr::group_by(c1, intdate), mind = min(Date), maxd = max(Date), actived = sum(op, na.rm = T))))
  c2$totald <- as.numeric(difftime(c2$maxd, c2$mind)+1)
  c2$activet <- ifelse(c2$actived < c2$totald/2, NA, 0)

  c3 <- merge.data.frame(c1, c2, by = "intdate", all.x = T)
  c4 <- c3[,c("Site", "Date", "Interval", "camdate", "totald", "actived", "activet")]


  # For the camera data now
  x1 <- x[,c(camOP$stationCol, "Date", "Species", "Individuals")]
  colnames(x1) <- c("Site", "Date", "Species", "Individuals")
  x1$Individuals <- as.numeric(x1$Individuals)
  x1$camdate <- with(x1, paste(Site, Date, sep = "_"))
  x2 <- suppressMessages(data.frame(dplyr::summarise(dplyr::group_by(x1, Site, Date, Species, camdate), Detections = dplyr::n(), Abundance = sum(Individuals))))
  x3 <- split(x2, f = x2$Species)
  x4 <- x3[include]

  ## Check if requested outputs are valid outputs
  subFun <- function(x, v, s){
    if(any(x == v)){
      x[which(x == v)] <- s
    }
    return(x)
  }

  if(length(out_form) == 1){
    if(out_form == "all"){
      out_form <- c("l", "w")
    }else{
      out_form <- subFun(out_form, "long", "l")
      out_form <- subFun(out_form, "wide", "w")
    }
  }else if(length(out_form) == 2){
    out_form <- subFun(out_form, "long", "l")
    out_form <- subFun(out_form, "wide", "w")
  }else{
    stop(paste("out_form has ", length(out_form), " items. It can only have between 1 and 2.", sep = ""))
  }
  if(length(out_form[-which(out_form %in% c("l", "w"))]) > 0){
    stop(paste("You have incorrect inputs in out_form. These are: c(", paste(out_form[-which(out_form %in% c("l", "w"))], collapse = ", "), ").", sep = ""))
  }
  if(length(out_data) == 1){
    if(out_data == "all"){
      out_data <- c("DE", "AB", "PA")
    }else{
      out_data <- subFun(out_data, "detections", "DE")
      out_data <- subFun(out_data, "abundance", "AB")
      out_data <- subFun(out_data, "presence", "PA")
    }
  }else if(any(length(out_data) == 2, length(out_data) == 3)){
    out_data <- subFun(out_data, "detections", "DE")
    out_data <- subFun(out_data, "abundance", "AB")
    out_data <- subFun(out_data, "presence", "PA")
  }else{
    stop(paste("out_data has ", length(out_data), " items. It can only have between 1 and 3.", sep = ""))
  }
  if(length(out_data[-which(out_data %in% c("DE", "AB", "PA"))]) > 0){
    stop(paste("You have incorrect inputs in out_data. These are: c(", paste(out_data[-which(out_data %in% c("DE", "AB", "PA"))], collapse = ", "), ").", sep = ""))
  }
  if(length(out_correction) == 1){
    if(out_correction == "all"){
      out_correction <- c("raw", "rm", "pu")
    }else{
      out_correction <- subFun(out_correction, "none", "raw")
      out_correction <- subFun(out_correction, "rmInactive", "rm")
      out_correction <- subFun(out_correction, "perUnit", "pu")
    }
  }else if(any(length(out_correction) == 2, length(out_correction) == 3)){
    out_correction <- subFun(out_correction, "none", "raw")
    out_correction <- subFun(out_correction, "rmInactive", "rm")
    out_correction <- subFun(out_correction, "perUnit", "pu")
  }else{
    stop(paste("out_correction has ", length(out_correction), " items. It can only have between 1 and 3.", sep = ""))
  }
  if(length(out_correction[-which(out_correction %in% c("raw", "rm", "pu"))]) > 0){
    stop(paste("You have incorrect inputs in out_correction. These are: c(", paste(out_correction[-which(out_correction %in% c("raw", "rm", "pu"))], collapse = ", "), ").", sep = ""))
  }

  if(all(length(out_form)==1, length(out_data)==1, length(out_correction)==1)){
    print(paste("Only one output will be created for each species. The output is ", out_form, "_", out_data, "_", out_correction, ". ", sep = ""))
  }

  ## Checks for data structure and number of species
  if(length(x4) == 0){
    stop("There are no valid species in the include list. Check your species names")
  }else{
    print(paste("There should be ", length(unique(c1$Site)), " unique sites and ", length(unique(c1$Interval)), " unique time periods in the outputs.", sep =""))
    print(paste("There are ", length(x4), " species being processed.", sep = ""))
  }

  x5 <- lapply(1:length(x4), function(i){
    a <- x4[[i]]
    a1 <- a[,c("camdate", "Detections", "Abundance")]
    a2 <- merge.data.frame(c4, a1, by = "camdate", all.x = T)
    a2$DE_raw <- ifelse(is.na(a2$Detections), 0, a2$Detections)
    a2$AB_raw <- ifelse(is.na(a2$Abundance), 0, a2$Abundance)
    a2$PA_raw <- ifelse(a2$DE_raw>0,1,0)
    a2$DE_rmInactive <- a2$DE_raw - a2$activet
    a2$AB_rmInactive <- a2$AB_raw - a2$activet
    a2$PA_rmInactive <- a2$PA_raw - a2$activet
    a2$DE_perUnit <- with(a2, ifelse(actived==0, NA, DE_raw/actived * totald))
    a2$AB_perUnit <- with(a2, ifelse(actived==0, NA, AB_raw/actived * totald))
    a2$PA_perUnit <- with(a2, ifelse(actived==0, NA, PA_raw/actived * totald))
    a3 <- suppressMessages(data.frame(dplyr::summarise_all(dplyr::group_by(a2[,c(2,4:7,10:ncol(a2))], Site, Interval, totald, actived, activet), sum)))
    a3$PA_raw <- with(a3, ifelse(PA_raw==0 | is.na(PA_raw),0,1))
    a3$PA_rmInactive <- with(a3, ifelse(PA_rmInactive==0 | is.na(PA_rmInactive),0,1))
    a3$PA_perUnit <- with(a3, ifelse(PA_perUnit==0 | is.na(PA_perUnit),0,1))

    ## Creating wide format data
    a4 <- lapply(6:ncol(a3), function(i){
      b1 <- a3[,c(1:2,i)]
      colnames(b1)[3] <- "value"
      b2 <- tidyr::pivot_wider(b1, names_from = Interval, values_from = value)
      b3 <- data.frame(b2[,-1])
      row.names(b3) <- b2$Site
      colnames(b3) <- colnames(b2)[-1]
      return(b3)
      rm(b1, b2, b3)
    })
    names(a4) <- colnames(a3)[6:ncol(a3)]

    ## Output all possible outputs
    out <- list("l_DE_raw" = a3[,c(1:5,6)],
                "l_DE_rm" = a3[,c(1:5,9)],
                "l_DE_pu" = a3[,c(1:5,12)],
                "l_AB_raw" = a3[,c(1:5,7)],
                "l_AB_rm" = a3[,c(1:5,10)],
                "l_AB_pu" = a3[,c(1:5,13)],
                "l_PA_raw" = a3[,c(1:5,8)],
                "l_PA_rm" = a3[,c(1:5,11)],
                "l_PA_pu" = a3[,c(1:5,14)],
                "w_DE_raw" = a4[["DE_raw"]],
                "w_DE_rm" = a4[["DE_rmInactive"]],
                "w_DE_pu" = a4[["DE_perUnit"]],
                "w_AB_raw" = a4[["AB_raw"]],
                "w_AB_rm" = a4[["AB_rmInactive"]],
                "w_AB_pu" = a4[["AB_perUnit"]],
                "w_PA_raw" = a4[["PA_raw"]],
                "w_PA_rm" = a4[["PA_rmInactive"]],
                "w_PA_pu" = a4[["PA_rmInactive"]]
    )

    choices <- unlist(lapply(out_form, function(f){lapply(out_data, function(d){lapply(out_correction, function(g){paste(f, d, g, sep = "_")})})}))

    out2 <- out[choices]

    if(length(out2) == 1){
      out3 <- out2[[1]]
    }else{
      out3 <- out2
    }

    return(out3)
    rm(a, a1, a2, a3, a4, out, out2, out3, choices)
    #rm(i)
  })
  names(x5) <- names(x4)

  if(length(x5) == 1){
    x6 <- x5[[1]]
  }else{
    x6 <- x5
  }
  return(x6)
  rm(camop, camopt, c1, c2, c3, c4, subFun, x1, x2, x3, x4, x5, x6)
  #rm(x, ct, include, unit, camOP, out_form, out_data, out_correction)
}
