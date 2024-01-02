# Quality Control Functions ####
## This script contains quality control and diagnostic functions for camera data.
## This script includes the following functions:
  ## cameraDiagnostics
  ## copyFiles
  ## ctDates
  ## imageEffort
  ## mergeFiles
  ## subsetImages
  ## timelapseQC
  ## trapEffort
  ## unsortImages

################################################################################

### First and Last Pictures (Added 2022-08-30, Modified 2023-07-06) ####
##' @description This function computes the first and last pictures from the output of the \code{\link{cameraRename3}} function for basic diagnostic purposes.
##'
##' @title Camera basic diagnostics
##'
##' @param x data.frame. A data frame outputted from the \code{\link{cameraRename3}} function.
##' @param cam_dir_level Integer. The level of the directory where the camera name is. This can be a number of levels from the top or bottom directory, specified using the 'from_bottom' argument.
##' @param from_bottom Logical. Should cam_dir_level be calculated from the lowest directory (TRUE) or highest directory (FALSE)
##'
##' @details This function has only been tested to work with the output of \code{\link{cameraRename3}}.
##' Theoretically it could work with any input that has an outpath, UserLabel, and DateTimeOriginal column but I don't know what it will do.
##' Update: The above fields seem to be required to ensure that the function does not throw an error. If no UserLabel exists, this function will leave the Label as NA
##'
##' This function is designed to help you create a CT table by identifying problems.
##' I would recommend either adjusting your formatting or using this function as a template to build your own.
##'
##' @return A data frame containing the outpath for the images, the user label, number of pictures, first picture date, and last picture date
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{cameraRename3}}
##'
##' @keywords manip
##'
##' @concept camera trapping
##' @concept diagnostics
##'
##' @importFrom dplyr reframe group_by n
##' @importFrom lubridate ymd year month day
##' @importFrom fs path_split path_ext
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
cameraDiagnostics <- function(x, cam_dir_level, from_bottom){
  #x <- openxlsx::read.xlsx("new_20220801.xlsx", sheet = 1, detectDates = T)
  #cam_dir_level <- 2
  #from_bottom <- TRUE

  x$datetime <- lubridate::ymd_hms(x$DateTimeOriginal)
  x$Date <- lubridate::ymd(paste(lubridate::year(x$datetime), lubridate::month(x$datetime), lubridate::day(x$datetime)))

  dirs <- do.call(rbind, fs::path_split(x$outpath))

  if(isTRUE(from_bottom)){
    cam_dir_level <- ncol(dirs)+1-cam_dir_level
  }

  x$camname <- dirs[,cam_dir_level]
  x$ext <- fs::path_ext(x$new.name)

  if(any(colnames(x) == "UserLabel")){
    out <- dplyr::reframe(dplyr::group_by(x, outpath, ext), Camera = unique(camname), Label = unique(UserLabel), num_pics = dplyr::n(), first_pic = min(Date, na.rm = T), last_pic = max(Date, na.rm = T))
  }else{
    out <- dplyr::reframe(dplyr::group_by(x, outpath, ext), Camera = unique(camname), Label = NA, num_pics = dplyr::n(), first_pic = min(Date, na.rm = T), last_pic = max(Date, na.rm = T))
  }

  print(paste("The total number of photos is: ", sum(out$num_pics), sep = ""))
  print(paste("The following cameras did not make to the end: ", paste(out$Label[out$last_pic %in% sort(unique(out$last_pic))[-length(unique(out$last_pic))]], collapse = ", "), sep = ""))

  return(out)
  rm(out)
  #rm(x)
}


## Direct copy of files to another location (Added 2023-11-03) ####
##' @description This function copies or moves files from one location to another
##'
##' @title Copy files from one location to another
##'
##' @param in.dir String. The directory containing the files you want to transfer. This can be anything but all folders and files below the level specified will be copied
##' @param out.dir String. The directory where you want to copy the files to.
##' @param create.dirs Logical. Should the function create the directories it needs?
##' @param type String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none').
##'
##' @details This function can be used to copy or move files from one location to another.
##' It is useful for creating backups and duplicates of file structure and files on multiple drives or in multiple locations.
##'
##' @return data.frame containing the file paths of the input and output files.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{movePictuers}}
##'
##' @keywords files
##' @keywords manip
##'
##' @concept camera trapping
##' @concept data backup
##'
##' @importFrom fs dir_ls dir_create file_move file_copy
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
copyFiles <- function(in.dir, out.dir, create.dirs, type){
  #in.dir <- "G:/new_20231218/images"
  #out.dir <- "D:/Raw/Raw_1847_PostCon"

  fs1 <- fs::dir_ls(path = in.dir, recurse = TRUE, type = "file")
  fs2 <- sub(in.dir, out.dir, fs1)

  fs3 <- data.frame(in.files = fs1, out.files = fs2, row.names = NULL)
  fs3$outpath <- sapply(fs3$out.files, function(x){sub(paste("/",basename(x), sep = ""), "", x)}, USE.NAMES = F)

  ## Create directories
  if(isTRUE(create.dirs)){
    print("Creating Directories")
    dirs <- unique(fs3$outpath)
    fs::dir_create(dirs)
    rm(dirs)
  }

  if(type == "move"){
    print(paste("File transfer in progress. Images are moved from in.dir to out.dir. ", nrow(fs3), " files are being moved.", sep = ""))
    test <- fs::file_move(path = fs3$in.files, new_path = fs3$out.files)
  }else if(type == "copy"){
    print(paste("File transfer in progress. Images are copied from in.dir to out.dir. ", nrow(fs3), " files are being copied.", sep = ""))
    test <- fs::file_copy(path = fs3$in.files, new_path = fs3$out.files)
  }else if(type == "none"){
    print("No file transfer specified")
  }else{
    message("You chose an invalid type. No file transfer will occur. Choose one of c('move', 'copy', 'none') to avoid this warning")
  }
  return(fs3)
  rm(fs1, fs2, fs3)
  #rm(in.dir, out.dir, type)
}

### Converting date-time information in a CT Table to character format (Added 2022-08-25) ####
##' @description This function will convert properly formatted date objects to characters. This is so camtrapR can read the character date. For some reason, the package does not like date-formatted dates.
##'
##' @title CT Table date conversion
##'
##' @param ct data frame representing a CT Table.
##' @param start.col Defaults to 6. This is the column number in the ct table where dates first appear. It is assumed that all columns after this contain dates.
##'
##' @details This function may not be necessary anymore as camtrapR seems to have fixed their issue where you couldn't use date-formatted dates in its input.
##'
##' A CT table formatted for \code{\link[camtrapR]{camtrapR}} is required. This CT table is used in other functions that utilize the camtrapR package and must be formatted properly.
##'
##' @return The same as the input except with character dates instead of date dates.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{trapEffort}}
##'
##' \code{\link{summarizeEvents}}
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
ctDates <- function(ct, start.col=6){
  #ct = CT_FM1847
  #start.col = 6

  for(i in start.col:ncol(ct)){
    ct[,i] <- as.character(ct[,i])
  }
  return(ct)
  #rm(ct, start.col)
}

### Number of Photos (Added 2022-08-25) ####
##' @description This function calculates the total number of pictures, number of animals, ghosts, and humans from one or more timelapse or dataorganize files.
##'
##' @title Camera trapping image effort
##'
##' @param timelapse List. A list object containing a list of timelapse files. If you name your files, the names will be outputted in the result.
##' @param do List. A list object containing a list of DataOrganize files. If you name your files, the names will be outputted in the result.
##'
##' @details If a timelapse file is given to the function, it will run an internal version of the \code{\link{doTimelapse}} function to convert it to a DataOrganize file.
##'
##' This function can accept either a timelapse file or a DataOrganize file, not both. Be sure to only specify one.
##'
##' @return A data frame containing total number of pictures, number of pictures of animals, ghosts, and humans, and the success rate for animal pictures in each file added as well as a row for the total number of pictures.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{doFolder}}, \code{\link{doTimelapse}}
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
imageEffort <- function(timelapse = NULL, do = NULL){
  #timelapse <- timelapse_test; do <- do_test
  #timelapse <- NULL; do <- NULL
  #timelapse <- timelapse_test; do <- NULL
  #timelapse <- NULL; do <- do_test

  if(all(is.null(timelapse), is.null(do))){
    stop("Both the timelapse and do arguments cannot be left NULL. A timelapse file is specified using the 'timelapse' argument and a dataorganize file is specified using the 'do' argument. Do not specify both")
  }else if(all(!is.null(timelapse), !is.null(do))){
    stop("Both a timelapse file and a dataorganize file were provided. Please only provide one.")
  }else if(!is.null(timelapse) & is.null(do)){
    print("Using a timelapse file. Loading images...")

    if(is.data.frame(timelapse)){
      timelapse <- list(timelapse)
    }

    AP <- lapply(timelapse, function(x){
      images1 <- x[,c("File", "RelativePath", "Species1", "Species1_Ind")]
      colnames(images1) <- c("File", "Path", "Species", "Individuals")

      if(!all(is.na(x$Species2))){
        images2 <- x[x$Species2!="",c("File", "RelativePath", "Species2", "Species2_Ind")]
        colnames(images2) <- c("File", "Path", "Species", "Individuals")
      }else{
        images2 <- NULL
      }
      if(!all(is.na(x$Species3))){
        images3 <- x[x$Species3!="",c("File", "RelativePath", "Species3", "Species3_Ind")]
        colnames(images3) <- c("File", "Path", "Species", "Individuals")
      }else{
        images3 <- NULL
      }
      if(!all(is.na(x$SpeciesOther))){
        images4 <- x[x$SpeciesOther!="",c("File", "RelativePath", "SpeciesOther", "Other_Ind")]
        colnames(images4) <- c("File", "Path", "Species", "Individuals")
      }else{
        images4 <- NULL
      }
      x1 <- rbind(images1,images2,images3,images4)
      x2 <- data.frame(do.call(rbind, strsplit(x1$Path, split = "\\\\")), do.call(rbind, strsplit(sub(".JPG", "", x1$File, ignore.case = T), split = " ")), x1[,3:4])
      x3 <- x2[,c(2,11,4:9,12)]
      colnames(x3) <- paste("V", seq(1:ncol(x3)), sep = "")
      x4 <- data.frame(x3[,1:2], apply(x3[,3:ncol(x3)], 2, as.integer))
      return(x4)
      rm(images1, images2, images3, images4, x1, x2, x3, x4)
    })
  }else if(is.null(timelapse) & !is.null(do)){
    print("Using a DataOrganize file. Loading images...")

    if(is.data.frame(do)){
      do <- list(do)
    }
    AP <- do
  }
  print("Image loading completed. Summarizing capture rates...")

  out1 <- lapply(AP, function(x){
    ghost <- nrow(x[x[,2]=="ghost",])
    human <- nrow(x[x[,2]=="human",])
    animal <- nrow(x) - ghost - human
    c(total = nrow(x), animal = animal, human = human, ghost = ghost)
  })
  if(is.null(names(out1))){
    message("Your items do not have names. They will be outputted in the order they were inputted.")
    names(out1) <- paste("item", seq(1,length(out1),1), sep = "")
  }
  out1[["total"]] <- apply(do.call(rbind, out1), 2, sum)
  out2 <- data.frame(do.call(rbind, out1))
  out2$success <- with(out2, animal/total)

  return(out2)
  rm(AP, out1, out2)
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
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{doFolder}}, \code{\link{doTimelapse}}
##'
##' \code{\link[openxlsx]{openxlsx}}
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

### Subsetting sets of images from directories ####
##' @description Subset images from a larger image directory
##'
##' @title Subset Images
##'
##' @param in.dir String. The directory containing the images folders. This must the folder containing either the raw or sorted camera folders.
##' @param out.dir String. The directory where you want to move or copy your subsetted images.
##' @param ext String. Defaults to c(".jpg", ".mp4"). What file extensions should the function look for for subsetting?
##' @param datatype String. Does the in.dir contain raw or sorted images. This can only be c("raw", "sorted").
##' @param from String. Defaults to NULL. The start date for images you want to subset. This is not required. See details below.
##' @param to String. Defaults to NULL The end date for images you want to subset. This is not required. See details below.
##' @param date.col String. Defaults to NULL Which date collected folders should be included in the subset. This is not required. See details below. This cannot be specified when datatype = "sorted".
##' @param species.col String. Defaults to NULL Which species folders should be included in the subset. This is not required. See details below. This cannot be specified when datatype = "raw".
##' @param create.dirs. Logical. Defaults to FALSE. Should the function create the directories it needs to do a file transfer?
##' @param type. String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none').
##' @param alt.method. String. Options could include c("alt1", "alt2"). All other options use the default.
##' Sometimes there is an issue loading files using the \code{\link[fs]{dir_ls}} function so this provides a couple alternatives that may address this issue.
##' alt1 loads directories using the fs package then loads each directory individually using the fs package. This may help identify errors.
##' alt1 loads files using \code{\link[base]{list.files}}. This is significantly slower than using the fs package but seems to work more consistently than \code{link[fs]{dir_ls}}
##'
##' @details When this function does its subset, you must specify at least one of from, to, date.col, or species.col.
##' date.col can only be used with raw data and species.col can only be used with sorted data.
##' When more than one of these is included, the subset will be created from all parameters provided.
##' The function will tell you what it is using for the subset.
##'
##' @return data.frame containing the old file path and new file path
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{unsortImages}}, \code{\link{movePictures}}
##'
##' @keywords files
##' @keywords manip
##'
##' @concept camera trapping
##' @concept subsetting images
##'
##' @importFrom fs dir_ls path_split path_join path_ext_remove path_ext dir_create file_move file_copy
##' @importFrom lubridate ymd_hms
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
subsetImages <- function(in.dir, out.dir, ext = c(".jpg", ".mp4"), datatype, from = NULL, to = NULL, date.col = NULL, species.col = NULL, create.dirs = F, type = "none", alt.load = NA){
  #in.dir <- "H:/Raw/Raw_1847_PostCon"
  #in.dir <- c("H:/Raw/Raw_77_PostCon", "H:/Raw/Raw_1847_PostCon")
  #in.dir <- "H:/Raw"
  #out.dir <- "M:/lostdata"
  #ext <- c(".jpg", ".mp4")
  #datatype = "raw"
  #from <- NULL; to <- NULL; species.col <- NULL
  #from <- "2022-05-09 00:00:00"
  #to <- "2022-06-01 00:00:00"
  #date.col <- c("20230106", "20230201", "20230228", "20230328", "20230427")
  #species.col <- c("bobcat", "coyote")
  #create.dirs <- T
  #type <- "copy"
  #alt.load <- NA

  print(paste("This function started at ", Sys.time(), sep = ""))
  if(any(!grepl("[.]", ext))){
    message("Some of your file extensions did not include the '.'. This is being added. Add a '.' to each ext to avoid this message")
    ext[which(!grepl("[.]", ext))] <- paste(".", ext[which(!grepl("[.]", ext))], sep = "")
  }

  if(any(!grepl("\\*", ext))){
    message("Adding a '*' to file extensions.")
    ext[which(!grepl("\\*", ext))] <- paste("*", ext[which(!grepl("\\*", ext))], sep = "")
  }

  # Confirm that the options are specified correctly
  nullfrom <- is.null(from); nullto <- is.null(to); nulldate <- is.null(date.col); nullspec <- is.null(species.col)

  if(!datatype %in% c("raw", "sorted")){stop("You must specify a proper datatype. The options are c('raw','sorted').")}
  if(all(nullfrom, nullto, nulldate, nullspec)){stop("You did not specify any valid subset inputs.")}
  if(datatype == "raw" & isFALSE(nullspec)){stop("You cannot use a set of species when using datatype='raw'.")}
  if(datatype == "sorted" & isFALSE(nulldate)){stop("You cannot use a set of date collected folders when using datatype='sorted'.")}

  # Load files
  print("Loading files...")

  ## For some reason, there is a weird issue loading files. An alternate method may help either identify the problem or fix it
  if(alt.load == "alt1"){
    dirs <- fs::dir_ls(path = in.dir, type = "directory", recurse = TRUE)
    x1 <- lapply(ext, function(ex){lapply(dirs, function(x){fs::dir_ls(path = x, type = "file", recurse = TRUE, glob = ex, ignore.case = TRUE)})})
  }else if(alt.load == "alt2"){
    x1 <- do.call(c, lapply(ext, function(ex){list.files(path = in.dir, pattern = ex, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)}))
  }else{
    x1 <- unlist(lapply(ext, function(ex){fs::dir_ls(path = in.dir, type = "file", recurse = TRUE, glob = ex, ignore.case = T)}))
  }

  if(length(x1) == 0){
    stop("You chose an invalid directory or file extension. No files returned.")
  }

  # Split file paths
  x2 <- data.frame(do.call(rbind, fs::path_split(x1)))

  if(datatype == "raw"){
    if(ncol(x2) > 3){
      colnames(x2)[1:(ncol(x2)-3)] <- paste("old", seq(1:(ncol(x2)-3)), sep = "")
      colnames(x2)[c((ncol(x2)-2):ncol(x2))] <- c("site", "date.col", "file")
    }else if(ncol(x2) == 3){
      colnames(x2) <- c("site", "date.col", "file")
    }else{
      stop("This function won't work if the lowest 2 directories are not site and date collected")
    }
  }else if(datatype == "sorted"){
    if(ncol(x2) > 4){
      colnames(x2)[1:(ncol(x2)-4)] <- paste("old", seq(1:(ncol(x2)-4)), sep = "")
      colnames(x2)[(ncol(x2)-3):ncol(x2)] <- c("site", "species", "individuals", "file")
    }else if(ncol(x2) == 4){
      colnames(x2) <- c("site", "species", "individuals", "file")
    }else{
      stop("This function won't work if the lowest 3 directories are not site, species, and individuals")
    }
  }

  # Split file names to get dates
  x3 <- data.frame(do.call(rbind, strsplit(fs::path_ext_remove(x2[,ncol(x2)]), split = " ")), ext = fs::path_ext(x2[,ncol(x2)]))

  if(ncol(x3) == 8){
    colnames(x3) <- c("year", "month", "day", "hour", "minute", "second", "serial", "ext")
  }else if(ncol(x3) == 7){
    colnames(x3) <- c("year", "month", "day", "hour", "minute", "second", "ext")
  }

  # Recombine everything together
  x4 <- data.frame(in.dir, out.dir, x2[,!grepl(pattern = "old", colnames(x2))], x3,
                   path = apply(x2[,-c(grep(pattern = "old", colnames(x2)), ncol(x2))], 1, fs::path_join),
                   datetime = with(x3, lubridate::ymd_hms(paste(year, month, day, hour, minute, second, sep = " "))))

  # Subset the data by date, date collected, and/or species
  print("Subsetting data...")
  if(nullfrom){from1 <- "2010-01-01"}else{from1 <= from}
  if(nullto){to1 <- Sys.Date()}else{to1 <- to}

  if(datatype == "raw"){
    if(nulldate){
      message(paste("This function is using all date collected folders between ", from1, " and ", to1, ".", sep = ""))
      date.col1 <- unique(x4$date.col)
    }else{
      message(paste("This function is using ", paste(date.col, collapse = ", "), " date collected folders between ", from1, " and ", to1, ".", sep = ""))
      date.col1 <- date.col
    }
    x5 <- x4[x4$date.col %in% date.col1 & x4$datetime >= from1 & x4$datetime <= to1,]
  }else if(datatype == "sorted"){
    if(nullspec){
      message(paste("This function is using all species folders between ", from1, " and ", to1, ".", sep = ""))
      spec.col1 <- unique(x4$species)
    }else{
      message(paste("This function is using ", paste(species.col, collapse = ", "), " species folders between ", from1, " and ", to1, ".", sep = ""))
      spec.col1 <- species.col
    }
    x5 <- x4[x$species %in% spec.col1 & x4$datetime >= from1 & x4$datetime <= to1,]
  }

  x5$oldpath <- with(x5, file.path(in.dir, path, file))
  x5$newpath <- with(x5, file.path(out.dir, path, file))

  # Create directories
  if(isTRUE(create.dirs)){
    print("Creating Directories...")
    dirs <- with(x5, unique(file.path(out.dir, path)))
    fs::dir_create(dirs)
    rm(dirs)
  }

  # Transfer files if requested
  if(type == "move"){
    print("File transfer in progress. Images are moved from in.dir to out.dir")
    fs::file_move(path = x5$oldpath, new_path = x5$newpath)
  }else if(type == "copy"){
    print("File transfer in progress. Images are copied from in.dir to out.dir")
    fs::file_copy(path = x5$oldpath, new_path = x5$newpath)
  }else if(type == "none"){
    print("No file transfer specified")
  }else{
    message("You chose an invalid type. No file transfer will occur. Choose one of c('move', 'copy', 'none') to avoid this warning")
  }

  out <- x5[,c("oldpath", "newpath")]

  return(out)
  rm(nullfrom, nullto, nulldate, nullspec, x1, x2, x3, x4, x5, out, date.col1, from1, to1, spec.col1)
  #rm(in.dir, out.dir, ext, datatype, from, to, date.col, species.col, create.dirs, type)
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
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{doTimelapse}}
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
  no_spec <- timelapse[timelapse$Species1=="",]
  missing_spec <- no_spec[no_spec$SpeciesOther == "",]

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
  rm(images, unique_spec, unique_species, no_spec, missing_spec, exclude, no_ind, missing_ind, yes_ind, wrong_ind, out)
  #rm(timelapse, exclude, detailed_res)
}

### Camera Trap Nights (Added 2022-08-25, Renamed 2022-09-13) ####
##' @description This function calculates the number of active camera trap nights and total camera trap nights using an inputted CT table, formatted based on camtrapR specifications.Required columns are setup date and retrieval date and theoretically, the table should have problems. I have never tested it on a dataset without any problems.
##'
##' @title Camera trapping trap night effort
##'
##' @param ct data.frame. A data frame formatted as a CT table for the  \code{\link[camtrapR]{camtrapR-package}}
##' @param camOP list. Arguments passed to \code{\link[camtraR]{cameraOperation}}. The most important ones of these are
##' stationCol, setupCol, and retrievalCol. hasProblems is often used as well.
##' If multiple cameras per site, you must specify cameraCol, byCamera, allCamsOn, and camerasIndepedent.
##' See \code{\link[camtrapR]{cameraOperation}} for details.
##'
##' @details Make sure that your CT table is formatted properly. See the \code{\link[camtrapR]{camtrapR-package}} documentation for details. That is the only way this function works. Also, at some point camptrapR had removed its support for dates in "Date" or "POSIXct" format so dates had to be in character format. You can use my ctdates_fun function to fix this in a CT table. This may not be the case anymore and they may have fixed this issue.
##'
##' A CT table formatted for \code{\link[camtrapR]{camtrapR}} is required. This CT table is used in other functions that utilize the camtrapR package and must be formatted properly.
##'
##' @return A data frame containing the name of the Station (and possibly the session number and camera ID, depending on the inputs of camOP),
##' active camera trap nights (activenights), and total camera trap nights (totalnights).
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{ctDates}} \code{\link[camtrapR]{cameraOperation}}
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
trapEffort <- function(ct, camOP){
  #ct <- cttable
  #camOP <- list(stationCol = "Camera", setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T, cameraCol = "Camera", byCamera = F, allCamsOn = F, camerasIndependent = F)

  if(!is.list(camOP)){
    stop("camOP must be a list of arguments used in the camtrapR::cameraOperation function.")
  }

  camOP[["CTtable"]] <- ct

  co1 <- do.call(camtrapR::cameraOperation, camOP)

  co2 <- data.frame(Site = row.names(co1),
                    activenights = apply(co1, 1, function(x){sum(x, na.rm = T)}),
                    totalnights = apply(co1, 1, function(x){length(which(!is.na(x)))}))
  rownames(co2) <- NULL

  return(co2)
  rm(co1, co2)
  #rm(ct, camOP)
}

### Unsort Images to Raw data structure (Added 2023-01-04) ####
##' @description This function is designed to take sorted images and return them to Raw data structure format. It is the reverse process to the movePictures function
##'
##' @title Move pictures from sorted to unsorted folders
##'
##' @param in.dir string. The directory containing the sorted camera folders. This can have lengths greater than 1
##' @param out.dir string. The directory where you want to unsort files to. This can have length 1 or equal to the in.dir
##' @param date.col string. The date collected of the batch/es of pictures that need to be unsorted. This must be the same length as the in.dir
##' @param create.dirs Logical. Should the function create the directories it needs?
##' @param type String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none')
##'
##' @details This function assumes that the sorted pictures are sorted in the same way as what is created by the movePictures function.
##' Therefore, it can be considered a reverse procedure to the movePictures function. This can be used to create new backups of raw data if any are lost or if the raw data is otherwise unavailable.
##'
##' This function will only copy the first instance of an image and will ignore duplicates. This is because duplicate images can arise in sorted images when more than one species is detected in a single image.
##'
##' This function has not gone through full testing and may have problems. Please report issues as they come up.
##'
##' @return list of data frames of each each in.dir directory. Each data frame contains the following information:
##' @return in.files:
##' String. Full file paths to the in files
##' @return out.files:
##' String. Full file paths to the out files
##' @return duplicate:
##' Logical. Is the file a duplicate file
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
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
  #date.col <- c("2021_08_13", "2021_07_05", "2021_05_06", "2021_06_12")
  #type <- "none"
  #create.dirs <- T

  print(paste("This function started at ", Sys.time(), sep = ""))

  # Initial error checking
  if(length(in.dir) != length(date.col)){
    stop("You must specify the same number of elements in in.dir as date.col")
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
    dc <- date.col[i]

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

    x1$dup <- duplicated(x1$newname)

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
  rename <- lapply(ds1, function(x){data.frame(in.files = x$oldname, out.files = x$newname, duplicate = x$dup)})

  ## Remove duplicate images
  ds3 <- ds2[ds2$dup == FALSE,]

  if(length(ds2) > length(ds3)){
    message("There were duplicate files in the sorted.  Duplicate files will not be unsorted.")
  }


  ## Create Directories
  if(isTRUE(create.dirs)){
    print("Creating directories")
    dirs <- with(ds3, list(unique(file.path(out.dir, Camera)),
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
    out <- fs::file_move(path = ds2$oldname, new_path = ds2$newname)
  }else if(type=="copy"){
    print("File transfer in progress. Images are copied from in.dir to out.dir")
    out <- fs::file_copy(path = ds2$oldname, new_path = ds2$newname)
  }else if(type == "none"){
    print("No file transfer specified")
  }else{
    message("You chose an invalid type. No file transfer will occur. Choose one of c('move', 'copy', 'none') to avoid this warning")
  }

  return(rename)
  #rm(ds1, ds2, ds3, e, rename, out)
  #rm(in.dir, out.dir, date.col, type, create.dirs)
}
