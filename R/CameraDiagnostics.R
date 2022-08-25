# Camera Diagnostics Functions

### Camera Trap Nights (Added 2022-08-25)
##' @description This function calculates the number of active camera trap nights and total camera trap nights using an inputted CT table, formatted based on camtrapR specifications.Required columns are setup date and retrieval date and theoretically, the table should have problems. I have never tested it on a dataset without any problems.
##'
##' @title Camera trapping effort (Trap nights)
##'
##' @param cttable data.frame. A data frame formatted as a CT table.
##' @param group A column in the CT Table indicating how camera trap nights should be calculated. Generally accepted are c("Camera", "Site", "Station").
##' @param sessions Logical. Does your data have multiple sessions (e.g., field seasons, etc.)? This defaults to FALSE.
##' @param sessioncol (Optional). What column distinguishes sessions? This should not be needed if sessions is set to F. If you have problems, set this to NULL.
##'
##' @details Make sure that your CT table is formatted properly. That is the only way this function works. Also, camptrapR for some reason removed its support for dates in "Date" or "POSIXct" format so dates must be in character format. You can use my ctdates_fun function to fix this in a CT table.
##'
##' @return A data frame containing the items from the group column, session column (if included), active camera trap nights, and total camera trap nights.
##'
##' @note As with all of my functions, this assumes a very particular formatting for your data. If the CT table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
##'
##' @seealso \code{\link{ctdates_fun}}
##'
##' @keywords manip
##'
##' @concept camera trapping
##'
##' @importFrom camtrapR cameraOperation
##' @export
##'
##' @example \dontrun{
##' # No example provided
##' }
trapeffort_fun <- function(cttable, group, sessions=F, sessioncol){
  #cttable <- CTtable_WCS
  #group <- "Site"
  #group <- "Station"
  #sessions <- T
  #sessioncol <- "timeperiod"

  #require(camtrapR)

  if(sessions==T){
    CamOp <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T,
                                         cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE,
                                         sessionCol = "timeperiod"))
    CamOp2 <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = FALSE,
                                          cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE,
                                          sessionCol = "timeperiod"))
  }else{
    CamOp <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T,
                                         cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE))
    CamOp2 <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = FALSE,
                                          cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE))
  }

  # Active Camera Trap Nights
  activenights <- apply(CamOp,2,function(x){sum(x,na.rm=T)})

  # Total Camera Trapping Nights
  totalnights <- apply(CamOp2,2,function(x){sum(x,na.rm=T)})

  # Cleaning and merging the camera trap nights
  if(sessions==T){
    name <- unique(cttable[,c(group,sessioncol)])

  }else{
    name <- unique(cttable[,c(group)])
  }

  trapnights <- data.frame(name, activenights, totalnights)
  rownames(trapnights) <- NULL

  return(trapnights)
  rm(CamOp, CamOp2, activenights, totalnights,name, trapnights)
  #rm(cttable, group)
}

### Number of Photos (Added 2022-08-25)
##' @description This function calculates the total number of pictures, number of animals, ghosts, and humans from one or more timelapse or dataorganize files.
##'
##' @title Camera trapping effort (Images)
##'
##' @param x List. A list object containing either timelapse files or dataorganize files. If you name your files, the names will be outputted in the result.
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
##' @importFrom cameraTrapping APFun_Timelapse
##' @export
##'
##' @example \dontrun{
##' # No example provided
##' }
imageeffort_fun <- function(x, type){
  #x <- list('20220117' = read.csv("K:/Completed/new_20220117/timelapse_out_20220117.csv"), '20220214' = read.csv("K:/Completed/new_20220214/timelapse_out_20220214.csv"))
  #type <- "timelapse"

  if(is.data.frame(x)){
    x <- list(x)
  }
  if(type=="timelapse"){
    AP <- lapply(x, cameraTrapping::APFun_Timelapse)
  }else if(type=="dataorganize"){
    AP <- x
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

  out <- do.call(rbind, lapply(1:length(x), function(i){
    data.frame(total = nrow(x[[i]]), animal = nrow(x[[i]]) - nrow(ghosts[[i]]) - nrow(human[[i]]), human = nrow(human[[i]]), ghost = nrow(ghosts[[i]]), success = NA)
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
  #rm(x, type)
}

