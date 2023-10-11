# Post-processing functions ####
## This script contains functions for post-sorting processing of camera data
## This script includes the following functions:
  ## bestPics
  ## doFolder
  ## doTimelapse
  ## movePictures
  ## removeGhosts

################################################################################

### Extract best pictures from the timelapse file (Added 2022-08-25) ####
##' @description This function uses a timelapse csv to extract photos tagged as a best picture and copies them to a specified location.
##'
##' @title Extract Best Pictures from Timelapse
##'
##' @param timelapse A data frame from a timelapse file
##' @param in.dir String. The directory containing the pictures. This can either be an unsorted directory or a sorted directory. Keep in mind, these assume my very specific directory format.
##' @param out.dir String. The directory where you want the best pictures to be stored. This can be anything.
##' @param copy Logical. Should the images be copied into the new directory?
##' @param sorted Logical. Is the in.dir an unsorted folder or a sorted folder?
##'
##' @return A data frame containing the file path info of the best pics.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
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
##' @concept timelapse
##'
##' @importFrom fs file_exists file_move file_copy
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
bestPics <- function(timelapse, in.dir, out.dir, copy = T, sorted = F){
  #timelapse <- read.csv("timelapse_out_20220608.csv")
  #in.dir <- "K:/new_20220608"
  #out.dir <- "K:/new_20220608/best_pics"
  #copy <- T
  #sorted <- F

  if(!fs::dir_exists(out.dir)){
    stop("The out directory does not exist. Did you remember to create it?")
  }

  x1 <- timelapse[timelapse$Best_Pic=="true",]
  print(paste(nrow(x1), " pictures will be exported to the best_pics folder.", sep = ""))
  x1$RelativePath <- gsub("\\\\", "/", x1$RelativePath)
  x2 <- data.frame(do.call(rbind, strsplit(x1$RelativePath, split = "/")), x1[,c("File", "Species1", "Species1_Ind")])
  colnames(x2) <- c("Folder", "Camera", "Date", "File", "Species", "Individuals")
  x2$Individuals <- formatC(x2$Individuals, flag = "0", width = 2)
  x2$inUnsorted <- file.path(in.dir, x2$Folder, x2$Camera, x2$Date, x2$File)
  x2$inSorted <- file.path(in.dir, "sorted", x2$Camera, x2$Species, x2$Individuals, x2$File)
  x2$outpath <- file.path(out.dir, x2$File)

  if(isTRUE(copy)){
    print("Images will be copied to the specified out.dir.")
    if(isFALSE(sorted)){
      if(!fs::file_exists(x2$inUnsorted[1])){
        stop(paste("Something is wrong with your input directory. This is what is outputted: \n", x2$inUnsorted[1], sep = ""))
      }
      fs::file_copy(x2$inUnsorted, x2$outpath)
    }else{
      if(!fs::file_exists(x2$inSorted[1])){
        stop(paste("Something is wrong with your input directory. This is what is outputted: \n", x2$inSorted[1], sep = ""))
      }
      fs::file_copy(x2$inSorted, x2$outpath)
    }
  }

  return(x2)
  rm(x1, x2)
  #rm(timelapse, in.dir, out.dir, copy, sorted)
}

### Create a dataorganize like output from sorted images (Added 2022-08-25, Modified 2023-07-06) ####
##' @description This is an R version of the DataOrganize program developed by Jim Sanderson and Grant Harris. While untested, it should provide a little more flexibility in naming of folders than the original DataOrganize program. It also can do basic diagnostics so you can check camera and species names.
##'
##' @title DataOrganize
##'
##' @param in.dir data.frame. The directory containing the camera folders.
##' @param ext String. Defaults to c(".jpg", ".mp4"). What file extensions should the function look for to run DataOrganize on?
##' @param do_format String. Defaults to "serial". Should dataOrganize include the camera's serial number if it has one? Choose one of c("serial", "original"). See details below.
##' @param save Logical. Should the file be saved to the working directory? The default is FALSE. If TRUE, this will create a file called dataorganize.txt containing the output. Be careful, as this can overwrite other files with this name.
##' @param diagnostics Logical. Should diagnostic information be outputted to the console? This is set to TRUE by default.
##'
##' @details This function's original intention was to replicate the results of the DataOrganize program and its file format.
##' Due to a modification with the \code{\link{movePictures}} to accommodate a DataOrganize file as an input, this function was modified to allow the inclusion of the camera serial number in the output.
##' While, this should not have any downstream effects, the addition of the serial number column could impact future processes.
##' This is something that I am checking and will confirm a smooth transition to this method.
##'
##' In addition, this function can now accommodate image names that are in "yyyy mm dd hh mm ss" (the standard in Renamer and SpecialRenamer) or "yyyy mm dd hh mm ss serial" (the standard in this package).
##' This should help improve compatibility with images that were formatted using Renamer or SpecialRenamer.
##'
##' @return original:
##' A data frame formatted in the same way as the DataOrganize program:
##' site species individuals year month day hour minute second.
##'
##' @return serial:
##' A data frame including the same information as DataOrganize but with the addition of the image serial number:
##' site species individuals year month day hour minute second serial.
##'
##' @references Original DataOrganize program: \url{https://smallcats.org/resources/}
##'
##' @note This function used to be called dataOrganize
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{calculateEvents}}
##'
##' \code{\link{movePictures}}
##'
##' \code{\link{doTimelapse}}
##'
##' @keywords files
##' @keywords manip
##'
##' @concept camera trapping
##' @concept DataOrganize
##'
##' @importFrom dplyr summarise group_by n
##' @importFrom pbapply pblapply
##' @importFrom fs path_ext_remove path_ext dir_ls, path_split
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
doFolder <- function(in.dir, ext = c(".jpg", ".mp4"), do_format = "serial", save = F, diagnostics = T){
  #in.dir <- "I:/new_20221201/sorted"
  #ext <- c(".jpg", "mp4")
  #do_format <- "serial"
  #save <- FALSE
  #diagnostics <- TRUE

  if(any(grepl("[.]", ext))){
    message("File extensions cannot include a '.'. This is being removed. Remove the '.' to each ext to avoid this message")
    ext[which(grepl("[.]", ext))] <- gsub("\\.", "", ext)
  }

  Ext <- c(toupper(ext), tolower(ext))

  fs1 <- fs::dir_ls(path = in.dir, recurse = T, type = "file")
  fs2 <- data.frame(do.call(rbind, fs::path_split(fs::path_ext_remove(fs1[fs::path_ext(fs1) %in% Ext]))))
  fs3 <- data.frame(fs2[,(ncol(fs2)-3):(ncol(fs2)-1)], do.call(rbind, strsplit(fs2[,ncol(fs2)], " ")))

  if(ncol(fs3)==10){
    if(!exists("do_format")){
      fs4 <- fs3[,-ncol(fs3)]
      colnames(fs4) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
      message("You forgot to specify do_format. The function will output in 'original' format. \nTo avoid this message, please choose one of c('serial', 'original').")
    }else if(do_format == "serial"){
      fs4 <- fs3
      colnames(fs4) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second", "serial")
    }else if(do_format == "original"){
      fs4 <- fs3[,-ncol(fs3)]
      colnames(fs4) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
    }else{
      fs4 <- fs3[,-ncol(fs3)]
      colnames(fs4) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
      message("You specified an incorrect do_format. The function will output in 'original' format. \nTo avoid this message, please choose one of c('serial', 'original').")
    }
  }else if(ncol(fs3)==9){
    fs4 <- fs3
    colnames(fs4) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
  }else{
    message("Your file and folder structure in your input directory does not have an expected number of columns. If you are attempting to use this function with a different structure, the function may not work properly.")
    fs4 <- fs3
  }

  if(diagnostics == T){
    diagn <- list("Sites and Species" = data.frame(dplyr::summarise(dplyr::group_by(fs4, site), species = length(unique(species)), images = dplyr::n())),
                  "Unique Species" = sort(unique(fs4$species)),
                  "Unique Number of Individuals" = sort(unique(fs4$individuals)))
    print(diagn)
    rm(diagn)
  }

  if(isTRUE(save)){
    print("Writing a text file to the in.dir.")
    write.table(fs4, file = file.path(in.dir, "dataorganize.txt"), row.names = F, col.names = T)
  }

  return(fs4)

  rm(fs1, fs2, fs3, fs4)
  #rm(in.dir, ext, do_format, save, diagnostics)
}

### Convert a Timelapse file to a dataorganize output (Added 2022-08-25, Updated 2023-02-02) ####
##' @description This function converts a Timelapse csv file to the dataorganize file output format. This was done this way because many of my core functions for processing camera trap data depend on the existence of a data frame created by APFun_env. Converting a timelapse file to this format is just the easiest way to maintain consistency.
##'
##' @title Convert a Timelapse csv to a format for use with APFun_env
##'
##' @param timelapse data.frame. A data frame representing a Timelapse csv file formatted using my timelapse template.
##' @param do_format String. Defaults to "serial". Should dataOrganize include the camera's serial number if it has one? Choose one of c("serial", "original"). See details below.
##'
##' @details The timelapse file must contain the following column names: c("Species1", "Species1_Ind", "Species2", "Species2_Ind", "Species3", "Species3_Ind", "SpeciesOther", "Other_Ind").
##'
##' File extensions are automatically removed so it is no longer necessary to specify file extensions in this function.
##'
##' This function's original intention was to replicate the results of the DataOrganize program and its file format.
##' Due to a modification with the \code{\link{movePictures}} to accommodate a DataOrganize file as an input, this function was modified to allow the inclusion of the camera serial number in the output.
##' While, this should not have any downstream effects, the addition of the serial number column could impact future processes.
##' This is something that I am checking and will confirm a smooth transition to this method.
##'
##' In addition, this function can now accommodate image names that are in "yyyy mm dd hh mm ss" (the standard in Renamer and SpecialRenamer) or "yyyy mm dd hh mm ss serial" (the standard in this package).
##' This should help improve compatibility with images that were formatted using Renamer or SpecialRenamer.
##'
##' @return original:
##' A data frame formatted in the same way as the DataOrganize program:
##' site species individuals year month day hour minute second.
##'
##' @return serial:
##' A data frame including the same information as DataOrganize but with the addition of the image serial number:
##' site species individuals year month day hour minute second serial.
##'
##' @note This function is designed to make timelapse files compatible with DataOrganize files which are used for most of the analyses in this package.
##'
##' This function used to be called APFun_Timelapse
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{calculateEvents}}
##'
##' \code{\link{doFolder}}
##'
##' @keywords datagen
##'
##' @concept camera trapping
##' @concept timelapse
##' @concept DataOrganize
##'
##' @importFrom fs path_ext_remove path_split
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
doTimelapse <- function(timelapse, do_format = "serial"){
  #timelapse <- read.csv("timelapse_out_20221028.csv")

  images1 <- timelapse[,c("File", "RelativePath", "Species1", "Species1_Ind")]
  colnames(images1) <- c("File", "Path", "Species", "Individuals")

  if(!all(is.na(timelapse$Species2))){
    images2 <- timelapse[timelapse$Species2!="",c("File", "RelativePath", "Species2", "Species2_Ind")]
    colnames(images2) <- c("File", "Path", "Species", "Individuals")
  }else{
    images2 <- NULL
  }
  if(!all(is.na(timelapse$Species3))){
    images3 <- timelapse[timelapse$Species3!="",c("File", "RelativePath", "Species3", "Species3_Ind")]
    colnames(images3) <- c("File", "Path", "Species", "Individuals")
  }else{
    images3 <- NULL
  }
  if(!all(is.na(timelapse$SpeciesOther))){
    images4 <- timelapse[timelapse$SpeciesOther!="",c("File", "RelativePath", "SpeciesOther", "Other_Ind")]
    colnames(images4) <- c("File", "Path", "Species", "Individuals")
  }else{
    images4 <- NULL
  }
  x1 <- rbind(images1,images2,images3,images4)

  x1$filename <- fs::path_ext_remove(x1$File)
  x2 <- data.frame(do.call(rbind, fs::path_split(x1$Path))[,2], x1[,3:4], do.call(rbind, strsplit(x1$filename, " ")))

  if(ncol(x2)==10){
    if(!exists("do_format")){
      x3 <- x2[,-ncol(x2)]
      colnames(x3) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
      message("You forgot to specify do_format. The function will output in 'original' format. \nTo avoid this message, please choose one of c('serial', 'original').")
    }else if(do_format == "serial"){
      x3 <- x2
      colnames(x3) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second", "serial")
    }else if(do_format == "original"){
      x3 <- x2[,-ncol(x2)]
      colnames(x3) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
    }else{
      x3 <- x2[,-ncol(x2)]
      colnames(x3) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
      message("You specified an incorrect do_format. The function will output in 'original' format. \nTo avoid this message, please choose one of c('serial', 'original').")
    }
  }else if(ncol(x2)==9){
    x3 <- x2
    colnames(x3) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
  }else{
    message("Your file and folder structure in your input directory does not have an expected number of columns. If you are attempting to use this function with a different structure, the function may not work properly.")
    x3 <- x2
  }

  x3$individuals <- formatC(x3$individuals, width = 2, flag = "0")

  return(x3)
  rm(images1, images2, images3, images4, x1, x2, x3)
  #rm(timelapse)
}

## Move all pictures into sorted folders (Added 2022-08-25, Modified 2022-09-13) ####
##' @description This function uses a Timelapse csv file to move or copy images from an unsorted folder to sorted folders based on species and number of individuals (in the same format as required for \code{link{dataorganize}}), although see details.
##'
##' @title Move pictures from unsorted to sorted folders
##'
##' @param timelapse data.frame. A data frame of a timelapse file. Note, this should follow the timelapse template that I typically use. You cannot specify both a timelapse file and a Dataorganize file using this function
##' @param do data.frame. A data frame of a DataOrganze file. This can be either from the DataOrganize program or from the \code{\link{doFolder}} function in this package. You cannot specify both a timelapse and DataOrganize file using this function.
##' @param in.dir String. The directory containing the root folder for the timelapse file. For example, if all your images were a folder called "images" which sits in an external drive, labelled "F:", then you would specify the in.dir as "F:".
##' @param out.dir String. The directory where you want to store the sorted images
##' @param create.dirs Logical. Should the function create the directories it needs?
##' @param type String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none'). If 'move' is selected but there are duplicate files, the function will use 'copy' instead of 'move' to allow transfer of all files.
##' @param exclude String. Which species should not be sorted? The default is NULL which sorts all species. This can take multiple inputs. Use c("Species1", "Species2", "etc") to specify unique species.
##' @param ... Additional arguments used when specifying a DataOrganize file. Only img_format and do_format are used. All other inputs are ignored. Both img_format and do_format can only be c("serial", "original") and are used to indicate whether a serial number has been included in the image names and/or DataOrganize file.
##'
##' @details When this function creates its folder structure, it uses the Individuals column in the Timelapse output.
##' For some "species" (e.g., ghost, human, bird, rodent), we do not sort these by individual, therefore the Individuals column is a 0.
##' These species get assigned a folder of 00 for their number of individuals. I do not know how this will affect quality control and the workflow down the line.
##' Generally, it should not be an issue but could result in NA values in the Individuals column of the \code{\link{calculateEvents}} or errors in the \code{\link{doFolder}} functions in this package.
##' Once this has been tested, I will update this.
##'
##' This function can "sort" pictures using either a timelapse csv or a dataorganize text file.
##' The inclusion of a dataorganize file was to better accommodate data sorted manually (i.e., not using Timelapse) and its primary use is when sorted images need to be replaced (e.g., the images got corrupted or lost somehow).
##'
##' When a dataorganize file is used in this function, the file must have the following column names: c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second", "serial").
##' The "serial" name is not required and is only used if do_format is specified as "serial".
##' These columns names are already produced by the \code{\link{doFolder}} and \code{\link{doTimelapse}} functions.
##'
##' @return list of the full file path to the in files and out files
##' @return in.files:
##' String. Full file paths to the in files
##' @return out.files:
##' String. Full file paths to the out files
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{dataOrganize}}
##'
##' \code{\link{calculateEvents}}
##'
##' @keywords files
##' @keywords manip
##'
##' @concept camera trapping
##' @concept timelapse
##'
##' @importFrom fs file_move file_copy dir_create
##' @importFrom pbapply pblapply
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
movePictures <- function(timelapse=NULL, do=NULL, in.dir, out.dir, create.dirs, type = "none", exclude = NULL, ...){
  #timelapse <- read.csv("timelapse_out_20221201.csv")
  #do <- test
  #in.dir <- "I:/new_20221201/images"
  #out.dir <- "I:/new_20221201/sorted"
  #create.dirs <- F
  #type <- "none"
  #exclude <- NULL

  print(paste("This function started at ", Sys.time(), sep = ""))

  if(!dir.exists(in.dir)){
    stop("Your in.dir does not exist. Did you specify the correct path")
  }
  if(!dir.exists(out.dir)){
    stop("Your out.dir does not exist. Did you forget to create it?")
  }

  if(!is.null(timelapse) & !is.null(do)){
    stop("Both a timelapse file and a dataorganize file were provided. Please only provide one.")
  }else if(!is.null(timelapse) & is.null(do)){
    print("Using a timelapse file. Loading images...")
    if(grepl("images", in.dir, ignore.case = T)){
      message("Your in.dir path includes the images folder. File transfer may not work properly if timelapse also references this folder")
    }

    timelapse$RelativePath <- gsub("\\\\", "/", timelapse$RelativePath)

    images1 <- timelapse[,c("File", "RelativePath", "Species1", "Species1_Ind")]
    colnames(images1) <- c("File", "Path", "Species", "Individuals")

    if(!all(is.na(timelapse$Species2))){
      images2 <- timelapse[timelapse$Species2 != "" & !is.na(timelapse$Species2), c("File", "RelativePath", "Species2", "Species2_Ind")]
      colnames(images2) <- c("File", "Path", "Species", "Individuals")
    }else{
      images2 <- NULL
    }
    if(!all(is.na(timelapse$Species3))){
      images3 <- timelapse[timelapse$Species3 != "" & !is.na(timelapse$Species3), c("File", "RelativePath", "Species3", "Species3_Ind")]
      colnames(images3) <- c("File", "Path", "Species", "Individuals")
    }else{
      images3 <- NULL
    }
    if(!all(is.na(timelapse$SpeciesOther))){
      images4 <- timelapse[timelapse$SpeciesOther != "" & !is.na(timelapse$SpeciesOther), c("File", "RelativePath", "SpeciesOther", "Other_Ind")]
      colnames(images4) <- c("File", "Path", "Species", "Individuals")
    }else{
      images4 <- NULL
    }
    x1 <- rbind(images1,images2,images3,images4)
    x2 <- data.frame(do.call(rbind, strsplit(x1$Path, split = "/")), x1[,c("File", "Species", "Individuals")])
    colnames(x2)[-c(ncol(x2)-2,ncol(x2)-1,ncol(x2))] <- c("Folder", "Camera", "Date")
    x2$Individuals <- formatC(x2$Individuals, flag = "0", width = 2)
    x3 <- x2[!(x2$Species %in% exclude), ]  # Remove species from list of those to be sorted

    x3in <- with(x3, file.path(Folder, Camera, Date, File))
    x3out <- with(x3, file.path(Camera, Species, Individuals, File))

    #rm(images1, images2, images3, images4)
  }else if(is.null(timelapse) & !is.null(do)){
    #do_ins <- list(img_format = "serial", do_format = "serial")

    print("Using a DataOrganize file. Loading images...")
    do_ins <- list(...)

    if(!any(any(names(do_ins)=="img_format"), any(names(do_ins)=="do_format"))){
      stop("To use a DataOrganize file for movePictures, you must specify c(img_format, do_format).")
    }
    img_format <- do_ins$img_format
    do_format <- do_ins$do_format

    if(!(grepl("images", in.dir, ignore.case = T))){
      in.dir <- file.path(in.dir, "images")
      message("Your in.dir path does not include the images folder. File transfer from a dataOrganize file will likely not work properly. 'images' is being appended to the in.dir")
    }

    if(img_format == "serial" & do_format == "original"){
      message("You specified that your images have serial numbers but your DataOrganize file does not. This function may not work properly")
    }else if(img_format == "original" & do_format == "serial"){
      message("You specified that your images do not have serial numbers but your DataOrganize does. This function may not work properly")
    }

    if(img_format == "serial"){
      files1 <- pbapply::pblapply(c(".jpg", ".mp4"), function(x){
        a1 <- list.files(in.dir, pattern = x, full.names = T, recursive = T, ignore.case = T)
        a2 <- data.frame(oldname = a1, do.call(rbind, strsplit(a1, "/")))
        colnames(a2)[(ncol(a2)-3):ncol(a2)] <- c("Folder", "Camera", "Date", "File")
        a3 <- data.frame(a2[,c("Folder", "Camera", "Date", "File")], do.call(rbind, strsplit(sub(x, "", a2$File), " ")))
        colnames(a3)[(ncol(a3)-6):ncol(a3)] <- c("year", "month", "day", "hour", "minute", "second", "serial")
        return(a3)
        rm(a1, a2, a3)
      })
    }else if(img_format == "original"){
      files1 <- pbapply::pblapply(c(".jpg", ".mp4"), function(x){
        a1 <- list.files(in.dir, pattern = x, full.names = T, recursive = T, ignore.case = T)
        a2 <- data.frame(do.call(rbind, strsplit(a1, "/")))
        colnames(a2)[(ncol(a2)-3):ncol(a2)] <- c("Folder", "Camera", "Date", "File")
        a3 <- data.frame(a2[,c("Folder", "Camera", "Date", "File")], do.call(rbind, strsplit(sub(x, "", a2$File), " ")))
        colnames(a3)[(ncol(a3)-6):ncol(a3)] <- c("year", "month", "day", "hour", "minute", "second")
        return(a3)
        rm(a1, a2, a3)
      })
    }else{
      stop("You did not specify a valid img_format. Choose one of c('serial', 'original').")
    }
    files2 <- do.call(rbind, files1)

    x1 <- do

    if(do_format == "serial"){
      files2$ID <- with(files2, paste(Camera, year, month, day, hour, minute, second, serial, sep = "_"))
      x1$ID <- with(x1, paste(site, year, month, day, hour, minute, second, serial, sep = "_"))
    }else if(do_format == "original"){
      files2$ID <- with(files2, paste(Camera, year, month, day, hour, minute, second, sep=""))
      x1$ID <- with(x1, paste(site, year, month, day, hour, minute, second, ".jpg", sep = ""))
    }else{
      stop("You specified an incorrect do_format. Choose one of c('serial', 'original').")
    }

    x2 <- merge.data.frame(files2, x1, by = "ID")
    x3 <- x2[!(x2$species %in% exclude), ]

    x3in <- with(x3, file.path(Camera, Date, File))
    x3out <- with(x3, file.path(site, species, individuals, File))

    #rm(do_ins, img_format, do_format, files1, files2)
  }else{
    stop("Both the timelapse and do arguments cannot be left NULL. A timelapse file is specified using the 'timelapse' argument and a dataorganize file is specified using the 'do' argument. Do not specify both")
  }

  if(length(x3in) != length(x3out)){
    stop("You have different numbers of files in the 'in' and 'out' directories. You broke my function...")
  }

  rename <- list(in.files = file.path(in.dir, x3in),
                 out.files = file.path(out.dir, x3out))
  print(paste("The in files will look like: ", rename[["in.files"]][[1]], sep = ""))
  print(paste("The out files will look like: ", rename[["out.files"]][[1]], sep = ""))

  if(isTRUE(create.dirs)){
    print("Creating Directories")
    dirs <- with(x3, unique(file.path(out.dir, Camera, Species, Individuals)))
    fs::dir_create(dirs)
    rm(dirs)
  }

  if(length(unique(x3in)) != length(unique(x3out))){
    message("You have a different number of in and out files, likely because more than one species was detected in a single picture. \nIf type = 'move', this will be switched to 'copy'.")
    if(type == "move"){type <- "copy"}
  }

  if(type == "move"){
    print("File transfer in progress. Images are moved from in.dir to out.dir")
    test <- fs::file_move(path = rename[["in.files"]], new_path = rename[["out.files"]])
  }else if(type == "copy"){
    print("File transfer in progress. Images are copied from in.dir to out.dir")
    test <- fs::file_copy(path = rename[["in.files"]], new_path = rename[["out.files"]])
  }else if(type == "none"){
    print("No file transfer specified")
  }else{
    message("You chose an invalid type. No file transfer will occur. Choose one of c('move', 'copy', 'none') to avoid this warning")
  }

  print(paste("This function completed at ", Sys.time(), sep = ""))
  return(rename)
  rm(x1, x2, x3, x3in, x3out, dirs, dirsTemp, rename)
  #rm(timelapse, do, in.dir, out.dir, create.dirs, type, exclude)
}

### Move ghosts identified by the Microsoft Megadetector AI to a sorted ghosts folder (Added 2022-08-25) ####
##' @description Use this function to use the output json file from the Microsoft Megadetector AI to identify images as false captures. It uses a detection threshold derived from the the json file to identify possible ghosts and move them to a user specified directory. The directory has the same structure as the input folder structure.
##'
##' @title Remove ghosts based on AI
##'
##' @param jsonfile The filepath to the json file containing the detections from MegaDetector.
##' @param in.dir The directory where the original images are stored. This should be the same directory specified when running the Megadetector AI.
##' @param out.dir The directory where the ghost images should be stored. See details below.
##' @param create.dirs Logical. Should R attempt to create sub-directories for the file transfer in the out.dir directory?
##' @param conf.threshold The confidence threshold that should be used to identify ghost images.
##' @param move Logical. Defaults to TRUE. Should the images be moved as part of the function. If FALSE, images will not be moved. You can use the output file later to move images using the file.rename function.
##'
##' @details It may be beneficial in some cases to not move the images immediately so you can check how many images are actually labelled as ghost. If you want to do this, specify move=F. This is highly recommended as you can confirm that the input file paths are correct.
##'
##' R will not create directories for you unless you specify create.dirs=T. If you choose to create the file paths yourself and you already have a directory of sorted pictures, you can use Command Prompt in Windows to create the needed folder structure. If you do not have a directory of sorted pictures, you will need to create this manually (or use create.dirs=T):
##'
##'   1. Open command prompt in Windows.
##'
##'   2. Type: xcopy [filepath to the in.dir] [filepath to the out.dir] /t /e
##'
##'   Example: xcopy e:/new_20211015/images e:/new_20211015/ghosts /t /e [Use a "\" instead of a "/" for the file paths]
##'
##' @return LIST of the input filepaths for each ghost image and the output filepath of each ghost image. If move is specified as FALSE, then this file can be used to move ghost images.
##' @return in.files:
##' The full file paths to the input ghost images.
##' @return out.files:
##' The full file paths to the output ghost images.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{movePictures}}, \code{\link{doFolder}}
##'
##' @keywords files
##' @keywords manip
##'
##' @concept camera trapping
##' @concept MegaDetector
##'
##' @importFrom jsonlite fromJSON
##' @importFrom fs file_move
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
removeGhosts <- function(jsonfile, in.dir, out.dir, create.dirs, conf.threshold, move = T){
  #jsonfile <- "G:/AI_detector/detections_filtered_20220117.json"
  #conf.threshold <- 0.8
  #in.dir <- "G:/test/new_20220117"
  #out.dir <- "G:/test/sorted_20220117"
  #create.dirs <- T

  print(paste("This function started at ", Sys.time(), sep = ""))

  json <- jsonlite::fromJSON(jsonfile)

  images <- json$images
  ghosts <- images[images$max_detection_conf<conf.threshold,]

  ghosts.temp <- do.call(rbind, strsplit(ghosts$file, "\\\\"))
  ghosts.out <- ghosts.temp[,ncol(ghosts.temp)]

  nrow(ghosts)
  print(paste("There were ", nrow(ghosts), " ghosts out of ", nrow(images), " total images. The proportion is ", round(nrow(ghosts)/nrow(images), digits = 4)*100, "%.", sep = ""))

  out.files <- data.frame(do.call(rbind, strsplit(gsub("\\\\", "/", ghosts$file), split = "/")))
  len <- apply(out.files, 2, function(x){length(unique(x))})
  out.files$path <- paste(out.files[,which(len>1)[1]], "ghost/00", out.files[,ncol(out.files)], sep = "/")

  rename <- list(in.files = paste(in.dir, ghosts$file, sep = "/"),
                 out.files = paste(out.dir, out.files$path, sep = "/"))
  print(paste("The absolute path for the 'in' files is: ", rename[["in.files"]][1], sep = ""))
  print(paste("The absolute path for the 'out' files is: ", rename[["out.files"]][1], sep = ""))

  if(isTRUE(create.dirs)){
    print("Creating directories")
    dirs <- with(out.files, list(unique(paste(out.dir, out.files[,which(len>1)[1]], sep = "/")),
                                 unique(paste(out.dir, out.files[,which(len>1)[1]], "ghost", sep = "/")),
                                 unique(paste(out.dir, out.files[,which(len>1)[1]], "ghost", "01", sep = "/"))))
    dirsTemp <- lapply(dirs, function(x){
      lapply(x, function(y){
        ifelse(!dir.exists(y), dir.create(y), print("Folder exists"))
      })
    })
  }

  if(isTRUE(move)){
    print(paste("File renaming in progress", sep = ""))
    fs::file_move(path = rename[["in.files"]], new_path = rename[["out.files"]])
  }

  print(paste("This function finished at ", Sys.time(), sep = ""))
  return(rename)
}
