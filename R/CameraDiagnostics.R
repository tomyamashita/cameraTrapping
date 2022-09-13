# Camera Diagnostics Functions

## First and Last Pictures (Added 2022-08-30) ####
##' @description This function computes the first and last pictures from the output of the \code{\link{cameraRename3}} function for basic diagnostic purposes.
##'
##' @title Camera basic diagnostics
##'
##' @param x data.frame. A data frame outputted from the \code{\link{cameraRename3}} function
##'
##' @details This function has only been tested to work with the output of \code{\link{cameraRename3}}. Theoretically it could work with any input that has an outpath, UserLabel, and DateTimeOriginal column but I don't know what it will do
##'
##' @return A data frame containing the outpath for the images, the user label, number of pictures, first picture date, and last picture date
##'
##' @note As with all of my functions, this assumes a very particular formatting for your data.
##' This function is designed to help you create a CT table by identifying problems.
##' I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own convenience and make no promises that they will work for different situations.
##' As I come across errors, I attempt to further generalize the function but this is done as I go.
##'
##' @seealso \code{\link{cameraRename3}}
##'
##' @keywords manip
##'
##' @concept camera trapping
##' @concept diagnostics
##'
##' @importFrom dplyr summarize group_by n
##' @importFrom lubridate ymd year month day
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
cameraDiagnostics <- function(x){
  #x <- openxlsx::read.xlsx("new_20220801.xlsx", sheet = 1, detectDates = T)

  x$datetime <- lubridate::ymd_hms(x$DateTimeOriginal)
  x$Date <- lubridate::ymd(paste(lubridate::year(x$datetime), lubridate::month(x$datetime), lubridate::day(x$datetime)))
  out <- data.frame(dplyr::summarize(dplyr::group_by(x, outpath), Label = unique(UserLabel), num_pics = dplyr::n(), first_pic = min(Date), last_pic = max(Date)))

  print(paste("The total number of pictures is: ", sum(out$num_pics), sep = ""))
  print(paste("The following cameras did not make to the end: ", paste(out$Label[out$last_pic %in% sort(unique(out$last_pic))[-length(unique(out$last_pic))]], collapse = ", "), sep = ""))

  return(out)
  rm(out)
  #rm(x)
}

## Camera Trap Nights (Added 2022-08-25, Renamed 2022-09-13) ####
##' @description This function calculates the number of active camera trap nights and total camera trap nights using an inputted CT table, formatted based on camtrapR specifications.Required columns are setup date and retrieval date and theoretically, the table should have problems. I have never tested it on a dataset without any problems.
##'
##' @title Camera trapping effort (Trap nights)
##'
##' @param cttable data.frame. A data frame formatted as a CT table.
##' @param group String. A column in the CT Table indicating how camera trap nights should be calculated. Generally accepted are c("Camera", "Site", "Station").
##' @param sessions Logical. Does your data have multiple sessions (e.g., field seasons, etc.)? This defaults to FALSE.
##' @param sessioncol String. What column distinguishes sessions? This defaults to NULL. This is only needed if sessions is set to T.
##'
##' @details Make sure that your CT table is formatted properly. See the \code{\link[camtrapR]{camtrapR-package}} documentation for details. That is the only way this function works. Also, at some point camptrapR had removed its support for dates in "Date" or "POSIXct" format so dates had to be in character format. You can use my ctdates_fun function to fix this in a CT table. This may not be the case anymore and they may have fixed this issue.
##'
##' @return A data frame containing the items from the group column, session column (if included), active camera trap nights, and total camera trap nights.
##'
##' @note As with all of my functions, this assumes a very particular formatting for your data. If the CT table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
##'
##' @seealso \code{\link{ctdates_fun}} \code{\link[camtrapR]{cameraOperation}}
##'
##' @keywords manip
##'
##' @concept camera trapping
##'
##' @importFrom camtrapR cameraOperation
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
trapEffort <- function(cttable, group, sessions=F, sessioncol=NULL){
  #cttable <- CTtable_WCS
  #group <- "Site"
  #group <- "Station"
  #sessions <- T
  #sessioncol <- "timeperiod"

  #require(camtrapR)

  if(isTRUE(sessions)){
    if(is.null(sessioncol)){
      stop("You indicated that there are sessions in you ct table. You must specify a sessioncol.")
    }
    CamOp <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T,
                                         cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE,
                                         sessionCol = sessioncol))
    CamOp2 <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = FALSE,
                                          cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE,
                                          sessionCol = sessioncol))
  }else if(isFALSE(sessions)){
    CamOp <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T,
                                         cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE))
    CamOp2 <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = FALSE,
                                          cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE))
  }else{
    stop("Sessions must be logical. Choose c(T,F).")
  }

  # Active and Total Camera Trap Nights
  trapnights1 <- data.frame(activenights = apply(CamOp,2,function(x){sum(x,na.rm=T)}),
                            totalnights = apply(CamOp2,2,function(x){sum(x,na.rm=T)}))

  if(is.null(sessioncol)){
    trapnights2 <- data.frame(rownames(trapnights1), trapnights1)
    colnames(trapnights2) <- c(group, "activenights", "totalnights")
  }else{
    trapnights2 <- data.frame(do.call(rbind, strsplit(rownames(trapnights1), "__")), trapnights1)
    colnames(trapnights2) <- c(group, sessioncol, "activenights", "totalnights")
  }
  rownames(trapnights2) <- NULL

  return(trapnights2)
  rm(CamOp, CamOp2, trapnights1, trapnights2)
  #rm(cttable, group, sessions, sessioncol)
}

## Number of Photos (Added 2022-08-25) ####
##' @description This function calculates the total number of pictures, number of animals, ghosts, and humans from one or more timelapse or dataorganize files.
##'
##' @title Camera trapping effort (Images)
##'
##' @param timelapse List. A list object containing either timelapse files or dataorganize files. If you name your files, the names will be outputted in the result.
##' @param type String. What was the source of the input files. Choose one of c("timelapse", "dataorganize").
##'
##' @details If a timelapse file is given to the function, it will run the \code{\link{APFun_Timelapse}} function to convert it to a dataorganize file.
##'
##' @return A data frame containing total number of pictures, number of pictures of animals, ghosts, and humans, and the success rate for animal pictures in each file added as well as a row for the total number of pictures.
##'
##' @note As with all of my functions, this assumes a very particular formatting for your data. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
##'
##' @seealso \code{\link{dataorganize}}
##'
##' @keywords manip
##'
##' @concept camera trapping
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
imageEffort <- function(timelapse, type){
  #timelapse <- list('20220117' = read.csv("K:/Completed/new_20220117/timelapse_out_20220117.csv"), '20220214' = read.csv("K:/Completed/new_20220214/timelapse_out_20220214.csv"))
  #type <- "timelapse"

  if(is.data.frame(timelapse)){
    timelapse <- list(timelapse)
  }
  if(type=="timelapse"){
    AP <- lapply(timelapse, APFun_Timelapse)
  }else if(type=="dataorganize"){
    AP <- timelapse
  }else{
    stop("Your data must be a 'timelapse' or 'dataorganize' file.")
  }

  if(is.null(names(AP))){
    name <- F
    message("Your items do not have names. They will be outputted in the order they were inputted")
  }else{
    name <- T
  }

  ghosts <- lapply(AP, function(y){y[y[,2]=="ghost",]})
  human <- lapply(AP, function(y){y[y[,2]=="human",]})

  out <- do.call(rbind, lapply(1:length(timelapse), function(i){
    data.frame(total = nrow(timelapse[[i]]), animal = nrow(timelapse[[i]]) - nrow(ghosts[[i]]) - nrow(human[[i]]), human = nrow(human[[i]]), ghost = nrow(ghosts[[i]]), success = NA)
  }))
  rownames(out) <- names(AP)

  total <- apply(out, 2, sum)
  out2 <- rbind(out,total = apply(out, 2, sum))
  if(isFALSE(name)){
    rownames(out2) <- c(1:nrow(out), "total")
  }else{
    rownames(out2) <- c(names(AP), "total")
  }
  out2[,"success"] <- with(out2, animal/total)

  return(out2)
  rm(AP, name, ghosts, human, out, total, out2)
  #rm(timelapse, type)
}

