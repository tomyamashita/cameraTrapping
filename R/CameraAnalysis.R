# Camera Data Analysis

### Animal Diel Activity (Added 2022-08-25)
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
##' @note As with all of my functions, this assumes a very particular formatting for your data. If the CT table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
##'
##' @seealso activity package: \url{https://cran.r-project.org/web/packages/activity/index.html}
##'
##' @keywords manip
##' @keywords univar
##'
##' @concept camera trapping
##' @concept diel activity
##'
##' @importFrom activity fitact
##' @export
##'
##' @example \dontrun{
##' # No example provided
##' }
actfun <- function(x, split=F, splitcol=NULL, species, bw = NULL, rep = 999){
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

### Setting up for occupancy modelling from an APFun_env output (Added 2022-08-25)
##' @description This function modifies data for use in occupancy modelling using Unmarked or a separate program, I think.
##'
##' @title Occupancy Analysis Data Setup
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
##' @return A LIST object by species chosen in the subset input with a table of sites (rows) and dates (columns).
##'
##' @seealso \code{\link{APFun_env}}
##'
##' @keywords datagen
##' @keywords manip
##'
##' @concept camera trapping
##' @concept occupancy analysis
##'
##' @importFrom camtrapR cameraOperation
##' @export
##'
##' @example \dontrun{
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

  active <- aggregate(camopt, by = list(Date2 = dates$Date2), sum)
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
    a1 <- aggregate(Individuals~camdate, data = a, sum)
    a2 <- merge.data.frame(camdate, a1, by = "camdate", all.x = T)
    a2$Individuals[is.na(a2$Individuals)] <- 0

    a3 <- aggregate(Individuals~sortCol+Date2, data = a2, sum)

    a4 <- reshape(a3, direction = "wide", timevar = "Date2", idvar = "sortCol")
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

