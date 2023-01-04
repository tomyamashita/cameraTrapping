# Quality Control Functions ####
## This script contains quality control and diagnostic functions for camera data.
## This script includes the following functions:
  ## cameraDiagnostics
  ## ctDates
  ## imageEffort
  ## mergeFiles
  ## timelapseQC
  ## trapEffort
  ## unsortImages

################################################################################

### First and Last Pictures (Added 2022-08-30) ####
##' @description This function computes the first and last pictures from the output of the \code{\link{cameraRename3}} function for basic diagnostic purposes.
##'
##' @title Camera basic diagnostics
##'
##' @param x data.frame. A data frame outputted from the \code{\link{cameraRename3}} function
##'
##' @details This function has only been tested to work with the output of \code{\link{cameraRename3}}.
##' Theoretically it could work with any input that has an outpath, UserLabel, and DateTimeOriginal column but I don't know what it will do.
##' Update: The above fields seem to be required to ensure that the function does not throw an error. If no UserLabel exists, this function will leave the Label as NA
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
  if(any(colnames(x)=="UserLabel")){
    out <- data.frame(dplyr::summarize(dplyr::group_by(x, outpath), Label = unique(UserLabel), num_pics = dplyr::n(), first_pic = min(Date), last_pic = max(Date)))
  }else{
    out <- data.frame(dplyr::summarize(dplyr::group_by(x, outpath), Label = NA, num_pics = dplyr::n(), first_pic = min(Date), last_pic = max(Date)))
  }

  print(paste("The total number of pictures is: ", sum(out$num_pics), sep = ""))
  print(paste("The following cameras did not make to the end: ", paste(out$Label[out$last_pic %in% sort(unique(out$last_pic))[-length(unique(out$last_pic))]], collapse = ", "), sep = ""))

  return(out)
  rm(out)
  #rm(x)
}

### Converting date-time information in a CT Table to character format (Added 2022-08-25) ####
##' @description This function will convert properly formatted date objects to characters. This is so camtrapR can read the character date. For some reason, the package does not like date-formatted dates.
##'
##' @title CT Table date conversion
##'
##' @param cttable data frame representing a CT Table.
##' @param start.col Defaults to 6. This is the column number in the ct table where dates first appear. It is assumed that all columns after this contain dates.
##'
##' @return The same as the input except with character dates instead of date dates.
##'
##' @note As with all of my functions, this assumes a very particular formatting for your data. If the CT table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
##'
##' @seealso \code{\link{trapeffort_fun}}
##'
##' @keywords manip
##'
##' @concept camera trapping
##' @concept CT tables
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
ctDates <- function(cttable, start.col=6){
  #cttable = CT_FM1847
  #start.col = 6

  for(i in start.col:ncol(cttable)){
    cttable[,i] <- as.character(cttable[,i])
  }
  return(cttable)
  rm(cttable, start.col)
}

### Number of Photos (Added 2022-08-25) ####
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

### Merge Files with the same extension (Added 2022-01-04) ####
##' @description This function will find all files in a directory with a specified extension and merge them into a single file. It is useful for merge dataorganize or timelapse files for quicker access
##'
##' @title Merge Files
##'
##' @param in.dir string. The directory containing the files you want to merge
##' @param pattern string. The file extension of the files you want to merge. Currently this can only be c(".txt", ".csv", ".xlsx"). The period must be included in the extension name. Note that the ".xlsx" file extension uses the \code{\link[openxlsx]{openxlsx}} package for reading and writing.
##' @param save. logical. Should the output be saved to the current working directory? This defaults to FALSE.
##'
##' @details This function assumes that you are only merging files that have the same data structure. It does not do any checking of this so please make sure that your data structure is consistent among files before using this function.
##' This makes it especially useful for merging DataOrganize files or Timelapse outputs.
##'
##' Currently, this function only allows for the above file extensions. Other extensions could be included in the future. Please let me know if you want a different file extension to be available.
##'
##' @return data frame containing the merged files
##'
##' @seealso \code{\link{dataOrganize}}, \code{\link{APFun_Timelapse}}
##'
##' \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}
##'
##' @keywords files
##' @keywords manip
##' @keywords datagen
##'
##' @concept camera trapping
##' @concept timelapse
##' @concept DataOrganize
##'
##' @importFrom openxlsx read.xlsx
##' @importFrom openxlsx write.xlsx
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
mergeFiles <- function(in.dir, pattern, save = F){
  #in.dir <- getwd()
  #pattern <- ".txt"
  #save <- F

  files <- list.files(path = in.dir, pattern = pattern, full.names = T)

  if(pattern == ".txt"){
    x1 <- lapply(files, read.table)
  }else if(pattern == ".csv"){
    x1 <- lapply(files, read.csv)
  }else if(pattern == ".xlsx"){
    x1 <- lapply(files, openxlsx::read.xlsx)
  }else{
    stop("You chose an incompatible file extension. Make sure you include the period. Choose one of c('.txt', '.csv', '.xlsx')")
  }

  x2 <- do.call(rbind, x1)

  if(isTRUE(save)){
    if(pattern == ".txt"){
      write.table(x2, file = "mergedfile.txt", row.names = F)
    }else if(pattern == ".csv"){
      write.csv(x2, file = "mergedfile.csv", row.names = F)
    }else if(pattern == ".xlsx"){
      openxlsx::write.xlsx(x2, file = "mergedfile.xlsx")
    }
  }

  return(x2)
  rm(files, x1, x2)
  #rm(in.dir, pattern, save)
}

### Quality control for timelapse-sorted images (Added 2022-08-25, Modified 2022-09-13) ####
##' @description This function outputs the unique species from a timelapse file, a list of rows that were not sorted by species, and a list of rows that were not sorted by individual. You should use this to check whether a set of images was completely sorted.
##'
##' @title Quality control for timelapse
##'
##' @param timelapse A data frame created from a csv file exported from Timelapse. Note, this function is set up to use my timelapse template.
##' @param exclude String. Which species should not be checked for number of individuals? The default is NULL which will only exclude c("ghost", "human", "", "rodent", "bird").
##' @param detailed_res Logical. Should a detailed output of errors be produced? If set to FALSE, only simple diagnostic information will be displayed (full information will be in the output file).
##'
##' @details This function will check if there are video files (.MP4) then check the number of unique species, whether any image was not sorted by species by checking if the species1 column contains any "" values, and whether any image was not sorted by individuals by checking if a column sorted into a species was not sorted by individual. Ghosts, humans, "", birds, rodents, lizards, and amphibians are not sorted by individual under my current protocol, and are therefore not included in this check. Video files are also not checked by individual.
##'
##' Once the timelapse file has passed this check, it can be used in the standard workflow for images.
##'
##' @return LIST containing unique species and data frames of missing species files and missing individuals files.
##' @return Unique Species:
##' String. The list of the unique species identified from the file.
##' @return Missing Species:
##' Data frame. Any entries that were not sorted by species is included in this file.
##' @return Missing Ind:
##' Data frame. Any entries that should have been but were not sorted by individual are included here.
##'
##' @seealso \code{\link{APFun_Timelapse}}
##'
##' @keywords debugging
##' @keywords manip
##'
##' @concept camera trapping
##' @concept timelapse
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
timelapseQC <- function(timelapse, exclude=NULL, detailed_res=F){
  # timelapse <- read.csv("timelapse_out_HY_20220708.csv)
  #exclude <- c("ghost", "human", "rodent", "bird", "cattle", "")
  #detailed_res <- F

  # Check for video files
  if(isTRUE(any(grepl(".MP4", timelapse$File, ignore.case = T)))){
    message("Video files detected. Number of individuals only analyzed for images")
    video <- timelapse[grep(".MP4", timelapse$File, ignore.case = T),]
    images <- timelapse[-grep(".MP4", timelapse$File, ignore.case = T),]
  }else{
    print("No video files detected.")
    images <- timelapse
  }

  # Check species names
  unique_spec <- list(
    unique1 = unique(timelapse$Species1),
    unique2 = if(!any(is.na(unique(timelapse$Species2)))){unique(timelapse$Species2)[unique(timelapse$Species2)!=""]},
    unique3 = if(!any(is.na(unique(timelapse$Species3)))){unique(timelapse$Species3)[unique(timelapse$Species3)!=""]},
    unique4 = if(!any(is.na(unique(timelapse$SpeciesOther)))){unique(timelapse$SpeciesOther)[unique(timelapse$SpeciesOther)!=""]}
  )
  unique_species <- sort(unique(do.call(c, unique_spec)))

  # Check missing species data in the species 1 column
  missing_spec <- timelapse[timelapse$Species1=="",]

  # Check missing individuals data in species data
  if(is.null(exclude)){
    exclude <- c("ghost", "human", "", "rodent", "bird")
  }else{
    exclude <- c(exclude, "")
  }
  no_ind <- list(
    no_ind1 = images[!(images$Species1 %in% exclude),][images[!(images$Species1 %in% exclude),]$Species1_Ind==0,],
    no_ind2 = if(!any(is.na(unique(images$Species2)))){images[!(images$Species2 %in% exclude),][images[!(images$Species2 %in% exclude),]$Species2_Ind==0,]},
    no_ind3 = if(!any(is.na(unique(images$Species3)))){images[!(images$Species3 %in% exclude),][images[!(images$Species3 %in% exclude),]$Species3_Ind==0,]},
    no_ind4 = if(!any(is.na(unique(images$SpeciesOther)))){images[!(images$SpeciesOther %in% exclude),][images[!(images$SpeciesOther %in% exclude),]$Other_Ind==0,]}
  )
  missing_ind <- do.call(rbind,no_ind)

  yes_ind <- list(
    no_ind1 = images[(images$Species1 %in% exclude),][images[(images$Species1 %in% exclude),]$Species1_Ind!=0,],
    no_ind2 = if(!any(is.na(unique(images$Species2)))){images[(images$Species2 %in% exclude),][images[(images$Species2 %in% exclude),]$Species2_Ind!=0,]},
    no_ind3 = if(!any(is.na(unique(images$Species3)))){images[(images$Species3 %in% exclude),][images[(images$Species3 %in% exclude),]$Species3_Ind!=0,]},
    no_ind4 = if(!any(is.na(unique(images$SpeciesOther)))){images[(images$SpeciesOther %in% exclude),][images[(images$SpeciesOther %in% exclude),]$Other_Ind!=0,]}
  )
  wrong_ind <- do.call(rbind, yes_ind)

  # Output the results
  if(isTRUE(detailed_res)){
    print(paste("There were ", length(unique_species), " unique species. These were: ", sep = ""))
    print(paste(unique_species, collapse = ", "))
    if(nrow(missing_spec)==0){
      print("All files were properly labelled by species")
    }else{
      message(paste(nrow(missing_spec), " file(s) were not labelled by species. These were: \n", paste(missing_spec$File, collapse = "\n"), sep = ""))
    }
    if(nrow(missing_ind)==0 & nrow(wrong_ind)==0){
      print("All files were properly labelled by individuals.")
    }else if(nrow(missing_ind)!=0 & nrow(wrong_ind)==0){
      message(paste(nrow(missing_ind), " file(s) were not labelled by individual. These were: \n", paste(missing_ind$RelativePath, missing_ind$File, collapse = "\n", sep = "\\"), "\nNo files were inproperly labelled by individual.", sep = ""))
    }else if(nrow(missing_ind)==0 & nrow(wrong_ind)!=0){
      message(paste("All files that should have been labelled by individual were. \n", nrow(wrong_ind), " files were inproperly labelled by individual. These were: \n", paste(wrong_ind$RelativePath, wrong_ind$File, collapse = "\n", sep = "\\"), sep = ""))
    }else{
      message(paste(nrow(missing_ind), " file(s) were not labelled by individual. These were: \n", paste(missing_ind$RelativePath, missing_ind$File, collapse = "\n", sep = "\\"), "\n", nrow(wrong_ind), " files were inproperly labelled by individual. These were: \n", paste(wrong_ind$RelativePath, wrong_ind$File, collapse = "\n", sep = "\\"), sep = ""))
    }
  }else if(isFALSE(detailed_res)){
    print(paste("There were ", length(unique_species), " unique species. See output for species list.", sep = ""))
    if(nrow(missing_spec)==0){
      print("All files were properly labelled by species")
    }else{
      message(paste(nrow(missing_spec), " file(s) were not labelled by species. See output for errors.", sep = ""))
    }
    if(nrow(missing_ind)==0 & nrow(wrong_ind)==0){
      print("All files were properly labelled by individuals.")
    }else if(nrow(missing_ind)!=0 & nrow(wrong_ind)==0){
      message(paste(nrow(missing_ind), " file(s) were not labelled by individual. See output for errors. \nNo files were inproperly labelled by individual.", sep = ""))
    }else if(nrow(missing_ind)==0 & nrow(wrong_ind)!=0){
      message(paste("All files that should have been labelled by individual were. \n", nrow(wrong_ind), " files were inproperly labelled by individual. See output for errors", sep = ""))
    }else{
      message(paste(nrow(missing_ind), " file(s) were not labelled by individual. See output for errors. \n", nrow(wrong_ind), " files were inproperly labelled by individual. See output for errors.", sep = ""))
    }
  }


  out <- list("Unique Species" = unique_species, "Missing Species" = missing_spec, "Missing Ind" = missing_ind, "Wrong Ind" = wrong_ind)
  return(out)
  rm(images, unique_spec, unique_species, missing_spec, exclude, no_ind, missing_ind, yes_ind, wrong_ind, out)
  #rm(timelapse, exclude, detailed_res)
}

### Camera Trap Nights (Added 2022-08-25, Renamed 2022-09-13) ####
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

### Unsort Images to Raw data structure (Added 2023-01-04) ####
##' @description This function is designed to take sorted images and return them to Raw data structure format. It is the reverse process to the movePictures function
##'
##' @title Move pictures from sorted to unsorted folders
##'
##' @param in.dir string. The directory containing the sorted camera folders. This can have lengths greater than 1
##' @param out.dir string. The directory where you want to unsort files to. This can have length 1 or equal to the in.dir
##' @param datecol string. The date collected of the batch/es of pictures that need to be unsorted. This must be the same length as the in.dir
##' @param create.dirs Logical. Should the function create the directories it needs?
##' @param type String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none')
##'
##' @details This function assumes that the sorted pictures are sorted in the same way as what is created by the movePictures function.
##' Therefore, it can be considered a reverse procedure to the movePictures function. This can be used to create new backups of raw data if any are lost or if the raw data is otherwise unavailable.
##'
##' This function has not gone through full testing and may have problems. Please report issues as they come up.
##'
##' @return list of the full file path to the in files and out files
##' @return in.files:
##' String. Full file paths to the in files
##' @return out.files:
##' String. Full file paths to the out files
##'
##' @seealso \code{\link{movePictures}}
##'
##' @keywords files
##' @keywords manip
##'
##' @concept camera trapping
##' @concept sorting
##'
##' @importFrom fs file_move file_copy
##' @importFrom pbapply pblapply
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
unsortImages <- function(in.dir, out.dir, date.col, type, create.dirs = T){
  #in.dir <- list.dirs(path = getwd(), recursive = F)[3:6]  # The location containing the camera folders
  #out.dir <- "I:/Hixon/images2"
  #datecol <- c("2021_08_13", "2021_07_05", "2021_05_06", "2021_06_12")
  #type <- "none"
  #create.dirs <- T

  print(paste("This function started at ", Sys.time(), sep = ""))

  # Initial error checking
  if(length(in.dir) != length(datecol)){
    stop("You must specify the same number of elements in in.dir as datecol")
  }

  if(length(out.dir) > 1){
    if(length(out.dir) != length(in.dir)){
      stop("You specified different number of elements in in.dir as out.dir. out.dir can be length 1 or the same length as in.dir")
    }
  }

  if(length(in.dir) == 1){
    if(!dir.exists(in.dir)){
      stop("Your in directory does not exist. Did you specify it correctly?")
    }
  }else if(length(in.dir) > 1){
    print("Multiple directories specified. Will run each directory iteratively.")
    if(any(!sapply(in.dir, dir.exists))){
      stop("At least one of your in directories do not exist. Did you specify them correctly?")
    }
  }else{
    stop("Somehow you broke this function.")
  }

  # Loading files and preparing for renaming
  ds1 <- pbapply::pblapply(1:length(in.dir), function(i){
    indir <- in.dir[i]
    dc <- datecol[i]

    files <- list.files(path = indir, pattern = ".jpg", ignore.case = T, full.names = T, recursive = T)

    if(length(files) == 0){
      stop("You included a folder that contains no images. Check this")
    }

    x1 <- data.frame(do.call(rbind, strsplit(files, "/")), oldname = files)
    colnames(x1)[c(ncol(x1)-4, ncol(x1)-1)] <- c("Camera", "Image")
    x1$dc <- dc
    if(length(out.dir) == 1){
      x1$newname <- with(x1, paste(out.dir, Camera, dc, Image, sep = "/"))
    }else{
      x1$newname <- with(x1, paste(out.dir[i], Camera, dc, Image, sep = "/"))
    }

    return(x1)
    #rm(indir, dc, files, x1)
    #rm(i)
  })
  names(ds1) <- do.call(rbind, strsplit(in.dir, "/"))[,ncol(do.call(rbind, strsplit(in.dir, "/")))]

  # Error checking
  print(paste("Image processing completed at ", Sys.time(), ". Checking for errors...", sep = ""))

  e <- lapply(ds1, function(x){which(grepl(":", x$Image))})
  if(any(sapply(e, length)>0)){
    message("Some files are not fully sorted. You need to fix these before continuing. Incorrect files are outputted. Function stopped")
    e1 <- lapply(1:length(ds1), function(i){ds1[[i]][e[i],]})
    names(e1) <- names(ds1)
    return(e1)
    #rm(e1)
  }

  # Preparing for and renaming files
  print(paste("Error check complete. Cleaning up and renaming..."))

  ## Create a rename object
  ds2 <- do.call(rbind, ds1)
  rename <- lapply(ds1, function(x){data.frame(in.files = x$oldname, out.files = x$newname)})

  ## Create Directories
  if(isTRUE(create.dirs)){
    print("Creating directories")
    dirs <- with(ds2, list(unique(file.path(out.dir, Camera)),
                           unique(file.path(out.dir, Camera, dc))))

    dirsTemp <- lapply(dirs, function(x1){
      lapply(x1, function(y){
        ifelse(!dir.exists(y), dir.create(y), print("Directory exists"))
      })
    })
    #rm(dirs, dirsTemp)
  }

  if(type=="move"){
    print("File transfer in progress. Images are moved from in.dir to out.dir")
    out <- fs::file_move(path = ds2[["in.files"]], new_path = ds2[["out.files"]])
  }else if(type=="copy"){
    print("File transfer in progress. Images are copied from in.dir to out.dir")
    out <- fs::file_copy(path = ds2[["in.files"]], new_path = ds2[["out.files"]])
  }else if(type == "none"){
    print("No file transfer specified")
  }else{
    message("You chose an invalid type. No file transfer will occur. Choose one of c('move', 'copy', 'none') to avoid this warning")
  }

  return(rename)
  rm(ds1, ds2, e, rename, out)
  rm(in.dir, out.dir, datecol, type, create.dirs)
}
