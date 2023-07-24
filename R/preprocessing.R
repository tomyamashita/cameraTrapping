# Pre-processing functions ####
## This script contains functions for pre-processing camera data
## This script includes the following functions:
  ## cameraRename2 (Deprecated)
  ## cameraRename3
  ## findCorruptImages

################################################################################

## Image renaming function (Added 2022-8-25) ####
##' @description The cameraRename2 version seeks to avoid using the camtrapR package to rename images and does so by directly reading the exif data using the exiftoolr package, then renaming based on the datetimeoriginal column of the exif data. This version has the advantage of being able to spit out any exif data the user wants (including information on the trigger number, whether images were external or internal trigger, camera name, etc.). This function is also more generalizable than the original cameraRename function and allows for performing specialRenamer like adjustment on images.
##'
##' Before running this function for the first time on a new machine see the details below.
##'
##' This function has been deprecated by the \code{\link{cameraRename3}} function.
##'
##' @title (Deprecated) Rename Camera Trap Images
##'
##' @param in.dir String. The directory where the pictures are located. I typically specify a folder containing the camera folders although theoretically any folder could work. The function can look for either ".jpg" or ".mp4" files.
##' @param out.dir multiple. The directory where the pictures will be renamed to. If left unspecified, this will default to the in.dir. Use this if you want to change the location of the images. You can specify this as NULL or "in.dir" to keep out.dir the same as in.dir or you can specify a vector of length 1 or length "number of pictures" to output your images to a new location.
##' @param file.type String. The type of file you want to rename. For images, this is likely to be ".jpg". For videos, this is likely to be ".MP4". This can also take c("image", "video"). For images, this will default the file type to ".jpg", for videos, it will default to ".mp4". If a different file type is needed, manually specify the type. You must include the "." in the file type. This function can theoretically take any image format, although I do not know how exiftool reads file types other than ".jpg" and ".mp4". The function will ignore case when looking for file types.
##' @param trigger.info String or NULL. Should additional information besides the date-time be included in the output? This defaults to NULL where no additional information is included. Because camera-specific information is variable between camera models, you must specify the camera model if you want additional information. Currently, only c("Hyperfire2", "Ultrafire_Video") are supported. Note that Ultrafire pictures use the same metadata tags as the Hyperfire2. Additional camera models could be added. See details for more information on this.
##' @param rename Logical or String. Should the images be renamed? The options are c(TRUE, FALSE, "replace", "copy", "none"). TRUE and "replace" will replace/move images with their new file names in the location specified by out.dir. "copy" will create a copy of the images in the out.dir. This has not been tested with an out.dir that is the same as in.dir. FALSE or "none" does not rename images. The default is FALSE. Typically, you should specify an option that moves/copies images but if you wanted to test the output before performing the rename, you can set to FALSE. See examples for how to rename pictures if set to FALSE or "none".
##' @param adjust String or NULL. Do the image date-times need adjustment? This defaults to NULL, indicating no adjustment needed. You only need to specify this if your image date-times need to be adjusted. This could arise due to daylight savings time or misentered date-time on the camera. This can be thought of as an R version of SpecialRenamer. You can either specify a difftime object or a character vector of the original and new date-times.
##' @param fix.names Logical. The default is FALSE. Do you need to rename all file names or just update those that don't have serial numbers and replace those that were not originally renamed? Only specify this to TRUE if you are using this function to update/fix images that were originally renamed by Renamer/SpecialRenamer and are in the old format. Only use this option if you know what you are doing. See details below.
##'
##' @details Important: You need to load the exiftoolr library and run the function, \code{\link[exiftoolr]{install_exiftool}}, before running this function.
##' This is because the package does not have a default version of exiftool available before you run the function.
##' See the help documentation in exiftoolr for more details about this and the reasoning behind it.
##' This function will check if your system has exiftool installed and if it doesn't, it will warn you and install exiftool using default settings.
##'
##' If you are interested in additional metadata information that is not provided, please run \code{\link[exiftoolr]{exif_read}} on a few images and choose the columns you are interested in.
##' Send me the exact column names, and your camera model and I can add an option to trigger.info.
##' Please keep in mind that the more metadata tags you choose, the longer the function takes to run.
##'
##' If you need to fix names (or add a serial number to existing names) rather than replace names, then you should specify fix.names to TRUE. By specifying this, it is telling the tool to check if the images are in the form YYYY MM DD HH MM SS.jpg (or, more likely some consistent form with spaces but I don't know for sure). When it does this, if an image does not fit this form, it replaces the name with the new date-time information derived from the metadata. The reason for this is that sometimes camera date-times are wrong so we needed to make an adjustment, similar to what is done with the adjust call except using SpecialRenamer. Because I did not want to go back through the images and find those that were orignally fixed and fix them again, this should leave those photos alone and only adjust inproperly named images.
##'
##' @return A data frame containing the input directory, output directory, the old image name, new image name, and any additional metadata information asked for using trigger.info.
##'
##' @references exiftool: \url{https://exiftool.org/}
##'
##' exiftoolr package: \code{\link[exiftoolr]{exif_read}}, \url{https://github.com/JoshOBrien/exiftoolr}
##'
##' Renamer and SpecialRenamer: \url{https://smallcats.org/resources/}
##'
##' @note This function has the potential for and was built to be expanded on.
##' If you want to add something related to what information you want from the function or want to be able to turn on/off certain aspects of the function, let me know and I can hopefully modify it.
##' Thanks, Duston, for being the guinea pig for some of these adjustments.
##'
##' @section {Warning}:
##' In previous versions of this function, there was an issue with file.rename not working properly for large numbers of files.
##' This was due to issues with the paste function not properly concatenating columns.
##' This is theoretically resolved in the current version of the function.
##'
##' I switched my file directory functions to those in the fs package so issues with renaming should be resolved now.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{cameraRename3}}
##'
##' \code{\link[exiftoolr]{install_exiftool}}
##'
##' \code{\link[fs]{file_copy}}, \code{\link[fs]{file_move}}
##'
##' @importFrom exiftoolr exif_version install_exiftool exif_read
##' @importFrom fs file_move file_copy
##' @importFrom lubridate ymd_hms year month day hour minute second
##' @importFrom methods is
##'
##' @keywords manip
##' @keywords files
##'
##' @concept camera trapping
##' @concept rename images
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
cameraRename2 <- function(in.dir, out.dir=NULL, file.type, trigger.info=NULL, rename=FALSE, adjust = NULL, fix.names = FALSE){
  #in.dir <- "J:/test/new_20220117/images_ConLate"  # More than likely, this must be a folder containing camera folders
  #out.dir <- "J:/test/new_20220501"
  #file.type <- ".jpg"  # What type of file do you want to rename
  #trigger.info <- "Hyperfire2"  # Should the output include info about trigger method, and photo numbers? Currently only available for Hyperfire 2 cameras
  #rename <- F  # Should the original image files be renamed? Note: this affects the raw file names and cannot be easily undone.
  #adjust <- NULL
  #adjust <- c("2021-01-01 00:00:00", "2021-01-01 01:00:00")  # Do the image date-times need adjustment? Specify the original date-time and the new date-time. This is used to calculate a difftime object for adjustment purposes.
  #fix.names <- F

  print(paste("This function started at ", Sys.time(), ". Loading images...", sep = ""))
  # Define which metadata tags you want to include in your output
  if(isTRUE(is.null(trigger.info))){
    Tag <- c("DateTimeOriginal")
  }else if(trigger.info=="Hyperfire2"){
    Tag <- c("DateTimeOriginal", "TriggerMode", "Sequence", "EventNumber", "AmbientTemperature", "UserLabel")
  }else if(trigger.info=="Ultrafire_Video"){
    Tag <- c("CreateDate")
  }else{
    Tag <- c("DateTimeOriginal")
    warning("Additional info is not supported for your camera model. Only date-time information will be provided. If you want additional information, choose one of c('Hyperfire2', 'Ultrafire_Video'). To figure out which metadata tags you want, run exiftoolr::exif_read('image file path').")
  }

  # Check if exiftool is installed within R
  out <- tryCatch(exiftoolr::exif_version())
  if(methods::is(out, "try-error")){
    warning("Exiftool was not installed on your version of R. The most up-to-date version of exiftool was installed using defaults...")
    exiftoolr::install_exiftool()
  }

  # Check if the "in" and "out" directories exist
  if(!dir.exists(in.dir)){
    stop("in.dir does not exist. Check your directory name")
  }
  ## If the out directory is not specified, it is set as the same as the "in" directory.
  if(length(out.dir)<=1){
    if(is.null(out.dir)){
      out.dir <- in.dir
    }else if(out.dir == "in.dir"){
      out.dir <- in.dir
    }else if(!dir.exists(out.dir)){
      stop("out.dir does not exist. Do you need to create it? If you want out.dir==in.dir, specify either c(NULL,'in.dir').")
    }
  }else{
    outdir.exist <- dir.exists(unique(out.dir))
    print(paste(length(outdir.exist), " unique out.dir(s) were specified", sep = ""))
    if(!all(outdir.exist)){
      stop("Some of your out.dirs do not exist. Do you need to create them?")
    }
  }

  # Check file types and ensure correct file types are used
  if(file.type=="image"){
    file.type <- ".jpg"
  }else if(file.type=="video"){
    file.type <- ".mp4"
  }else{
    file.type <- file.type
  }

  # Find files and check directory structure
  images <- list.files(path = in.dir, pattern = paste(file.type, "$", sep = ""), ignore.case = T, full.names = T, recursive = T)
  if(length(images)==0){
    stop("You have no images in your directory. Did you specify your path correctly?")
  }
  print(paste("Images loaded at ", Sys.time(), ". Checking file structure of the lowest two directories...", sep = ""))
  imagetest <- data.frame(do.call(rbind, strsplit(images, split = "/")))
  print(paste(length(unique(imagetest[,ncol(imagetest)-1])), " unique folder(s) was detected at the lowest directory. Is this correct?", sep = ""))
  print(paste(length(unique(imagetest[,ncol(imagetest)-2])), " unique folder(s) was detected at the second lowest directory. Is this correct?", sep = ""))
  if(length(out.dir)!=1){
    if(length(out.dir)!=length(images)){
      stop("out.dir must be either length 1 or the same length as the number of images")
    }
  }

  # Run exiftool and extract metadata
  print(paste("File structure checked. Cancel the function if any of the above is unexpected. Running exiftool...", sep = ""))
  exif1 <- exiftoolr::exif_read(images, tags = Tag)
  # Change the column name for video files because DateTimeOriginal does not exist
  if(isTRUE(grepl(".mp4", file.type, ignore.case = T))){
    colnames(exif1) <- sub("CreateDate", "DateTimeOriginal", colnames(exif1))
  }
  exif1b <- exif1[order(exif1$DateTimeOriginal),]
  print(paste("Exiftool completed at ", Sys.time(), ". Specifying image paths...", sep = ""))

  # Specify the proper paths for renaming images
  indir <- data.frame(do.call(rbind, strsplit(in.dir, "/")))
  if(length(unique(sapply(strsplit(exif1b$SourceFile, "/"), length)))>1){
    stop("Check your folder structure. You do not have the same number of subdirectories for each image")
  }
  folders <- data.frame(do.call(rbind, strsplit(exif1b$SourceFile, "/")))

  if(ncol(indir) == ncol(folders)-1){  # For the case of your in directory leading directly to images
    print("You have selected a folder with no sub-directories. out.dir should lead directly to pictures.")
    relativepath <- folders[,seq(1,ncol(folders)-1)]
    inpath <- indir[,seq(1,ncol(indir))]
    if(length(out.dir)==1){
      outpath <- data.frame(do.call(rbind, strsplit(rep(out.dir, times = nrow(folders)), "/")))
    }else{
      outpath <- data.frame(do.call(rbind, strsplit(out.dir, split = "/")))
    }
  }else if(ncol(indir) < (ncol(folders) - 1)){  # For any other case
    print("You have selected a folder with sub-directories. The relative paths will be preserved in out.dir")
    relativepath <- folders[,seq(ncol(indir)+1,ncol(folders)-1)]
    inpath <- folders[,seq(1,ncol(folders)-1)]
    outpath <- data.frame(do.call(rbind, strsplit(out.dir, "/")), relativepath)
  }else{
    stop("Something happened. in.dir has more sub-directories than the images.")
  }
  if(is.null(dim(relativepath))){
    relativepath2 <- relativepath
  }else{
    relativepath2 <- apply(relativepath, 1, paste, collapse = "/")
  }
  if(is.null(dim(inpath))){
    inpath2 <- inpath
  }else{
    inpath2 <- apply(inpath, 1, paste, collapse = "/")
  }
  if(is.null(dim(outpath))){
    outpath2 <- outpath
  }else{
    outpath2 <- apply(outpath, 1, paste, collapse = "/")
  }
  print(paste("Image paths specified at ", Sys.time(), ". Cleaning up and renaming...", sep = ""))

  # Do any adjustments to date-times, if necessary
  if(is.null(adjust)){
    dt.diff <- 0
  }else{
    print("Adjustments to the date-time are being made")
    if(isTRUE(lubridate::is.difftime(adjust))){
      dt.diff <- adjust
    }else{
      if(length(adjust)!=2){
        stop("Two date-times must be provided in adjust. If only one is provided, it needed to be a difftime object.")
      }
      dt.diff <- difftime(adjust[2], adjust[1], units = "secs")
    }
  }
  datetime <- lubridate::ymd_hms(exif1b$DateTimeOriginal) + dt.diff

  # Compile all the file paths and other information
  exif2 <- data.frame(SourceFile = exif1b$SourceFile,
                      inpath = inpath2,
                      indir = in.dir,
                      outpath = outpath2,
                      outdir = out.dir,
                      #relativepath = relativepath2,
                      old.name = folders[,ncol(folders)],
                      year = formatC(lubridate::year(datetime), width = 4, flag = "0"),
                      month = formatC(lubridate::month(datetime), width = 2, flag = "0"),
                      day = formatC(lubridate::day(datetime), width = 2, flag = "0"),
                      hour = formatC(lubridate::hour(datetime), width = 2, flag = "0"),
                      minute = formatC(lubridate::minute(datetime), width = 2, flag = "0"),
                      second = formatC(lubridate::second(datetime), width = 2, flag = "0"))

  # Add serial numbers based on the lowest sub-directory
  exif3 <- do.call(rbind, lapply(split(exif2, f = factor(exif2$inpath)), function(x){x$serial <- formatC(seq(1:nrow(x)), width = 5, flag = "0"); return(x)}))
  rownames(exif3) <- NULL
  exif3$new.name <- with(exif3, paste(year, " ", month, " ", day, " ", hour, " ", minute, " ", second, " ", serial, file.type, sep = ""))

  # Only add serial numbers to names that are renamed
  if(isTRUE(fix.names)){
    print("Image names were only replaced if they were not in the form YYYY MM DD HH MM SS.jpg. Serial number added to all images.")
    exif3$complete <- stats::complete.cases(apply(do.call(rbind,strsplit(sub(".jpg", "", exif3$old.name, ignore.case = T)," ")),2,as.numeric))
    exif3$new.name[exif3$complete==T] <- paste(sub(".jpg", "", exif3$old.name[exif3$complete==T], ignore.case = T), " ", exif3$serial[exif3$complete==T], ".jpg", sep = "")
  }

  # Rename the images if desired (this cannot be undone)
  if(isTRUE(rename)){
    print(paste("Files will be renamed and replaced. If you wanted to keep originals, specify c('copy') instead. Renaming started at ", Sys.time(), sep = ""))
    fs::file_move(path = with(exif3, paste(inpath, old.name, sep = "/")), new_path = with(exif3, paste(outpath, new.name, sep = "/")))
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(rename=="replace"){
    print(paste("Files will be renamed and replaced. Renaming started at ", Sys.time(), sep = ""))
    fs::file_move(path = with(exif3, paste(inpath, old.name, sep = "/")), new_path = with(exif3, paste(outpath, new.name, sep = "/")))
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(rename=="copy"){
    print(paste("Files will be renamed and copied. Renaming started at ", Sys.time(), sep = ""))
    fs::file_copy(path = with(exif3, paste(inpath, old.name, sep = "/")), new_path = with(exif3, paste(outpath, new.name, sep = "/")))
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(isFALSE(rename)){
    print("No files were renamed")
  }else if(rename=="none"){
    print("No files were renamed")
  }else{
    warning("You did not specify a correct value for rename. Choose one of c(TRUE,FALSE,'replace','copy','none'). No files were renamed")
  }

  # Clean up the data and prepare to shut down the function
  exif4 <- exif3[,c("SourceFile", "inpath", "outpath", "old.name", "new.name")]
  exif5 <- merge.data.frame(exif4, exif1b, by = "SourceFile")

  # One last data integrity check
  if(nrow(exif5) != nrow(exif4)){
    warning("Something happened. Some pictures went missing or got added between loading the exif data and renaming images.")
  }

  print(paste("This function completed at ", Sys.time(), sep = ""))
  return(exif5[,2:ncol(exif5)])

  rm(Tag, images, exif1, exif1b, indir.length, relativepath, inpath, outpath, relativepath2, inpath2, outpath2, folders, dt.diff, datetime, exif2, exif3, exif4, exif5)
  #rm(in.dir, out.dir, file.type, trigger.info, rename, adjust, fix.names)
}

## Another major update to the renaming function (Added 2022-08-25, modified 2023-07-24) ####
##' @description This function is used to extract metadata information and rename camera trap images using date-times and an assigned serial number to each image.
##' This function has all the same capabilities as the cameraRename2 function but should handle large datasets and corrupt files better.
##' Unlike the cameraRename2 functions, this version keeps everything within each camera directory, running the same process on each camera.
##' This allows it to take advantage of parallel processing and other functions to improve speed for very large datasets.
##'
##' @title Rename Camera Trap Images
##'
##' @param in.dir String. The directory where the pictures are located.
##' I typically specify a folder containing the camera folders although any folder containing images somewhere in the directory tree could work.
##' The function can look for either ".jpg" or ".mp4" files. See file.type.
##' @param out.dir String or Vector. The directory where the pictures will be renamed to.
##' The default is NULL which will use the in.dir as the location for images.
##' Another way to specify the output location to be the input location is to set out.dir = "in.dir".
##' You can use this to change the output location for each camera directory.
##' You can specify this with a single string, in which case it will use the relative paths to the images to locate the directory location or you can specify a vector of directories.
##' The length of this vector must equal the number of camera directories in the in.dir. If rename="copy", you should specify a different out.dir than in.dir although the function does not check for this.
##' @param ext String. The type(s) of file you want to rename.
##' For images, this is likely to be ".jpg". For videos, this is likely to be ".mp4".
##' Starting in package version 0.0.0.27, ext is used instead of file.type. This allows for specification of multiple file extensions, speeding up the renaming process.
##' If a different file type is needed, this function will need to be updated to accommodate this. Please let me know and I will add it.
##' This function can theoretically take any image format, although I do not know how exiftool reads file types other than ".jpg" and ".mp4". The function will ignore case when looking for file types.
##' @param trigger.info String or NULL. Should additional information besides the date-time be included in the output?
##' This defaults to NULL where no additional information is included, only date-time information.
##' Because camera-specific information is variable between camera models, you must specify the camera model if you want additional information.
##' When specifying multiple file types, you can currently only specify one format for trigger.info. If Reconyx is used, it will output both image exif data and video exif data. No other camera models are supported for multiple file extensions at this time.
##' Currently, for image formats c("Reconyx", "PC900", "Hyperfire2","Ultrafire_image", "Browning", "Cuddyback") are supported and for video formats, c("Ultrafire_video") are supported.
##' Note that all Reconyx camera models tested (PC900, Hyperfire2, Ultrafire) use the same metadata.
##' Additional camera models could be added. See details for more information on this.
##' @param rename String. The default is none. Other options include c("replace", "copy").
##' This specifies how images should be renamed.
##' "none" tells R to not rename any pictures, "replace" replaces the name then moves the image with its new name to the out.dir, and "copy" creates a new copy of the image with its new name in the out.dir.
##' Copy has not been tested with an out.dir the same as the in.dir but it is likely that it will keep both images and you will end up with duplicates.
##' I suggest choosing a different out.dir if you are going to copy. Copy also takes significantly longer than replace.
##' @param return.type String. How should the data be outputted.
##' Options are c("list", "df"). The default is as a list where each camera directory is in its own item in the list.
##' If you output a "df", the list of each camera is converted into a single data.frame object containing all cameras.
##' @param adjust String or NULL. Do the image date-times need adjustment?
##' This defaults to NULL, indicating no adjustment needed.
##' You only need to specify this if your image date-times need to be adjusted.
##' This could arise due to daylight savings time or misentered date-time information on the camera.
##' This can be thought of as an R version of SpecialRenamer.
##' You can either specify a difftime object or a character vector of the original and new date-times.
##' If you specify a vector or original and new date-times, this must have length==2.
##' @param fix.names Logical. Should a serial number be appended to date-time names?
##' The default is FALSE. This was added to allow for updating image names to include a unique serial number that have already been run through a program like Renamer or SpecialRenamer where it may be uncertain if the DateTimeOriginal in the Exif data is accurate or not.
##' Only set this to TRUE if you know what you are doing. It is highly recommended that you set rename="none" for this then rename manually after running the function.
##' @param pp Logical. Should this function take advantage of parallel processing.
##' The default is FALSE. Because the function separates tasks by camera directory, it can use parallel processing to run multiple cameras at the same time.
##' This is currently set up to run in Windows OS so I do not know if this will work on a Mac or Linux system.
##' If you want this functionality on a Unix device and know how to set it up, let me know and I can incorporate it.
##' @param cores.left Numeric. How many cores should be left available when using parallel processing?
##' The default is NULL. This is only necessary when pp=TRUE.
##' If left at the default, the function will default to 2 cores remaining which is generally enough to continue using a PC while the function runs.
##' I would set this to be greater than 0, otherwise the function will use the entire processing power of your computer.
##' To see how many cores you have available for parallel processing, use: parallel::detectCores().
##'
##' @details Important: Exiftool must be available on the machine in its default location for the function to work.
##' This function will check for exiftool and install it into the default directory if it is not available before running.
##' Previous versions of this function required you to manually check this but this requirement has been removed.
##'
##' When running this function on video files, you need to make sure you specify a trigger.info that accommodates videos.
##' This function will check if your trigger.info is associated with a video format. If not, it will warn you about this.
##' Video files doe not have a DateTimeOriginal field in their metadata so it will fail to run if you do not properly specify the correct field.
##' Right now, the "Ultrafire_video" option only accesses the "CreateDate" field which is used for video files.
##'
##' If you are interested in additional metadata information that is not provided, please run \code{\link[exiftoolr]{exif_read}} on a few images and choose the columns you are interested in.
##' Send me the exact column names, and your camera model and I can add an option to trigger.info.
##' Please keep in mind that the more metadata tags you choose, the longer the function takes to run.
##'
##' If you need to fix names (or add a serial number to existing names) rather than replace names, then you should specify fix.names to TRUE.
##' By specifying this, it is telling the tool to check if the images are in the form YYYY MM DD HH MM SS.jpg (or, more likely some consistent form with spaces but I don't know for sure).
##' When it does this, if an image does not fit this form, it replaces the name with the new date-time information derived from the metadata.
##' The reason for this is that sometimes camera date-times are wrong so we needed to make an adjustment using SpecialRename, similar to what is done with the adjust call in this function.
##' Because I did not want to go back through the images and find those that were orignally fixed and fix them again, this should leave those photos alone and only adjust improperly named images.
##'
##' Starting in package version 0.0.0.27, the file.type parameter was replaced with ext to allow for specification of multiple file extensions.
##' As part of this, the c("image", "video") options have been removed. Please begin using c(".jpg", ".mp4") instead.
##'
##' @return This function outputs either a list or a data.frame, depending on whether return.type = c("list", "df")
##' @return list:
##' If a list, each camera directory will be kept in a separate item in the list. This allows for easy checking if there is a problem which would have thrown an error when running exiftool, namely from a corrupt file. After outputting a list, you can combine the items into a single data.frame using do.call(rbind, out) where out is the name of the list object outputted by this function.
##' @return df:
##' If a data.frame, all camera directories will be combined into a single output file.
##' If a data.frame cannot be outputted (usually because you have two different camera models that have different exif tags), a list will be outputted instead.
##'
##' @references exiftool: \url{https://exiftool.org/}
##'
##' exiftoolr package: \code{\link[exiftoolr]{exif_read}}, \url{https://github.com/JoshOBrien/exiftoolr}
##'
##' Renamer and SpecialRenamer: \url{https://smallcats.org/resources/}
##'
##' @note Differences between this function and \code{\link{cameraRename2}}:
##'
##' There were several major changes in this function that differentiate it from the \code{\link{cameraRename2}} function enough that it warranted its own function rather than an update.
##' The main change and this affected everything down the line was that the function now acts on each camera directory separately.
##' By doing it this way, it allows for speed improvements on large numbers of folders and images using parallel processing, allows for the addition of progress bars to help track where it is, and allows for a way to check for corrupt image files.
##' One of the main issues with the cameraRename2 function is that if one image is bad, the entire function will fail, something that I discovered when running it on 1,031,000 pictures in 47 folders.
##' Therefore we needed a way to ensure that if there is a bad image, the function will skip it and keep going. In this case, it skips the entire folder and keeps going, allowing it to not be caught and stopped by a bad file.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{cameraRename2}}
##'
##' \code{\link[exiftoolr]{install_exiftool}}
##'
##' \code{\link[fs]{file_copy}}, \code{\link[fs]{file_move}}
##'
##' @importFrom dplyr bind_rows
##' @importFrom exiftoolr exif_version install_exiftool exif_read
##' @importFrom fs file_move file_copy dir_ls path_split path_join path_ext path_ext_remove
##' @importFrom lubridate is.difftime ymd_hms year month day hour minute second
##' @importFrom parallel makeCluster clusterExport detectCores stopCluster
##' @importFrom pbapply pblapply
##' @importFrom stats complete.cases
##'
##' @keywords manip
##' @keywords files
##'
##' @concept camera trapping
##' @concept rename images
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided yet
##' }
##'
cameraRename3 <- function(in.dir, out.dir=NULL, file.type, trigger.info=NULL, rename="none", return.type = "list", adjust = NULL, fix.names = FALSE, pp = FALSE, cores.left = NULL){
  #in.dir <- "J:/new_20230720" # More than likely, this must be a folder containing camera folders
  #out.dir <- NULL             # Where should the files be outputted to?
  #ext <- c(".jpg", ".mp4")    # What type(s) of file do you want to rename
  #trigger.info <- "Reconyx"   # Should the output include info about trigger method, and photo numbers? Currently only available for Hyperfire 2 cameras
  #rename <- "none"            # Should the original image files be renamed? Note: this affects the raw file names and cannot be easily undone.
  #return.type <- "df"         # Should the function output a data frame or a list of each folder
  #adjust <- NULL              # Do the image date-times need adjustment?
  #adjust <- c("2021-01-01 00:00:00", "2021-01-01 01:00:00")  # Do the image date-times need adjustment? Specify the original date-time and the new date-time. This is used to calculate a difftime object for adjustment purposes.
  #fix.names <- F              # Do the image names only need a serial number?
  #pp <- F                     # Should you use parallel processing
  #pp <- T                     # Parallel processing?
  #cores.left <- 4             # How many cores should be reserved when running parallel processing

  print(paste("This function started at ", Sys.time(), ". Loading images...", sep = ""))

  # Check file types and define metadata tags for particular camera models
  if(length(ext) == 2){
    if(any(grepl(".jpg", ext, ignore.case = T)) & any(grepl(".mp4", ext, ignore.case = T))){
      message("You are running both image and video files. Selecting appropriate exif tags")
      if(isTRUE(is.null(trigger.info))){
        Tag1 <- list(image = c("DateTimeOriginal"), video = c("CreateDate"))
      }else if(trigger.info %in% c("Reconyx")){
        Tag1 <- list(image = c("DateTimeOriginal", "TriggerMode", "Sequence", "EventNumber", "AmbientTemperature", "UserLabel", "SerialNumber"), video = c("CreateDate"))
      }else if(trigger.info %in% c("Browning", "Cuddyback")){
        message("This function does not yet support video files from Browning or Cuddyback cameras. Only date-time information will be provided. \nPlease send me the tags you wish to include.")
        Tag1 <- list(image = c("DateTimeOriginal"), video = c("CreateDate"))
      }else{
        Tag1 <- list(image = c("DateTimeOriginal"), video = c("CreateDate"))
        message("Additional info is not supported for your camera model. Only date-time information will be provided. \nFor image file types, choose one of c('Reconyx', 'Hyperfire2', 'PC900', 'Ultrafire_image', 'Browning', 'Cuddyback') and for video file types, choose one of c('Reconyx', 'Ultrafire_Video').")
      }
    }else{
      stop("Providing multiple file extensions that are not '.jpg' and '.mp4' files is not supported at this time. \nContact me if you need this support.")
    }
  }else if(length(ext) == 1){
    if(ext %in% c(".jpg", ".JPG")){
      if(isTRUE(is.null(trigger.info))){
        Tag1 <- c("DateTimeOriginal")
      }else if(trigger.info %in% c("Reconyx", "Hyperfire2", "PC900", "Ultrafire_image")){
        Tag1 <- c("DateTimeOriginal", "TriggerMode", "Sequence", "EventNumber", "AmbientTemperature", "UserLabel", "SerialNumber")
      }else if(trigger.info %in% c("Browning", "Cuddyback")){
        Tag1 <- c("DateTimeOriginal", "UserComment")
      }else{
        Tag1 <- c("DateTimeOriginal")
        message("Additional info is not supported for your camera model. Only date-time information will be provided. \nFor image file types, choose one of c('Reconyx', 'Hyperfire2', 'PC900', 'Ultrafire_image', 'Browning', 'Cuddyback').")
      }
    }else if(ext %in% c(".mp4", ".MP4")){
      if(isTRUE(is.null(trigger.info))){
        Tag1 <- c("CreateDate")
      }else if(trigger.info %in% c("Reconyx", "Ultrafire_Video")){
        Tag1 <- c("CreateDate")
      }else{
        Tag1 <- c("CreateDate")
        message("Additional info is not supported for your camera model. Only date-time information will be provided. \nFor video file types, choose one of c('Ultrafire_Video').")
      }
    }else{
      stop("You must specify an appropriate file extension. Choose one of c('.jpg', '.JPG', '.mp4', '.MP4').")
    }
  }else{
    stop("You must input at least 1 file extension")
  }

  # Check if exiftool is installed within R
  tryCatch(paste("Using exiftool version ", exiftoolr::exif_version(), sep = ""),
           error = function(e){message("Exiftool is not installed on your version of R. Installing the most up-to-date version of exiftool using exiftoolr::install_exiftool() and default inputs."); exiftoolr::install_exiftool()})

  # Check if the "in" and "out" directories exist
  if(!dir.exists(in.dir)){
    stop("in.dir does not exist. Check your directory name")
  }
  ## If the out directory is not specified, it is set as the same as the "in" directory.
  if(length(out.dir)<=1){
    if(is.null(out.dir)){
      out.dir <- in.dir
    }else if(out.dir == "in.dir"){
      out.dir <- in.dir
    }else if(!dir.exists(out.dir)){
      stop("out.dir does not exist. Do you need to create it? If you want out.dir==in.dir, specify either c(NULL,'in.dir').")
    }
  }else{
    print(paste(length(dir.exists(unique(out.dir))), " unique out.dir(s) were specified", sep = ""))
    if(!all(dir.exists(unique(out.dir)))){
      stop("Some of your out.dirs do not exist. Do you need to create them?")
    }
  }

  if(any(!grepl("\\*", ext))){
    message("Adding a '*' to file extensions.")
    ext[which(!grepl("\\*", ext))] <- paste("*", ext[which(!grepl("\\*", ext))], sep = "")
  }
  ext1 <- sub("\\*.", "", ext)

  # Find files and check directory structure
  images1 <- lapply(ext, function(ex){fs::dir_ls(path = in.dir, type = "file", recurse = TRUE, glob = ex, ignore.case = T)})

  images2 <- lapply(1:length(images1), function(i){
    x <- images1[[i]]

    if(length(x)==0){
      stop("You have no images in your directory. Did you specify your path and file type correctly?")
    }

    imagelist1 <- tryCatch(data.frame(t(sapply(fs::path_split(x), function(y){names(y) <- paste("X", seq(1:length(y)), sep = ""); return(y)}))),
                           error = function(e){print(e); stop("You have images in sub-directories of different lengths. This function likely will not work properly.")})
    colnames(imagelist1)[ncol(imagelist1)] <- "image"
    imagelist1$dir <- apply(imagelist1, 1, function(p){fs::path_join(p[-length(p)])})
    imagelist1$path <- file.path(imagelist1$dir, imagelist1$image)

    imagelist2 <- split(imagelist1, f = imagelist1$dir)
    names(imagelist2) <- paste(ext1[[i]], names(imagelist2), sep = "_")

    return(imagelist2)
    rm(x, imagelist1, imagelist2)
  })

  Tag2 <- lapply(1:length(Tag1), function(i){replicate(length(images2[[i]]), Tag1[[i]], simplify = FALSE)})

  images3 <- do.call(c, images2)
  Tag3 <- do.call(c, Tag2)

  # Some directory diagnostics
  print(paste("Images loaded at ", Sys.time(), ". Checking file structure of the lowest two directories...", sep = ""))
  images4 <- do.call(rbind, images3)
  message(paste(length(unique(images4[,ncol(images4)-3])), " unique folder(s) was detected at the lowest directory. Is this correct?", sep = ""))
  message(paste(length(unique(images4[,ncol(images4)-4])), " unique folder(s) were detected at the second lowest directory. Is this correct?", sep = ""))

  # Checking the out directory
  if(length(out.dir)==1){
    out.dir.length <- "one"
    message("The length of out.dir is 1. out.dir will reference a relative path for all images")
  }else if(length(out.dir)==length(images_split2)){
    out.dir <- as.list(out.dir)
    out.dir.length <- "cams"
    message("The length of out.dir is the same as the number of unique directories. out.dir will be unique for each camera folder")
  }else{
    stop("The length of out.dir is not the same as the number of unique image directories")
  }

  # Parallel processing
  if(isTRUE(pp)){
    message("Parallel processing enabled")
    if(is.null(cores.left)){
      cores.left <- 2
    }else{
      cores.left <- tryCatch(as.numeric(cores.left),
                             error = function(e){message("There was an error coercing cores.left to a number. The default of 2 cores are not utilized"); return(2)},
                             warning = function(w){message("Could not coerce cores.left to a number. The default of 2 cores are not utilized"); return(2)})
    }
    cl1 <- parallel::makeCluster(parallel::detectCores()-cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("images3", "Tag3"), envir = environment())
  }else{
    cl1 <- NULL
  }

  exif1 <- pbapply::pblapply(1:length(images3), cl = cl1, function(i){
    # Run exiftoolr to get metadata information
    arg <- list(path = images3[[i]]$path, tags = Tag3[[i]])
    x <- tryCatch(do.call(exiftoolr::exif_read, args = arg),
                  error = function(e){message(paste("There is likely a corrupt file in: \n", names(images3)[i], "\nThe original error is: ", sep = "")); message(e); return(names(images_split2)[i])})

    # Change the column name for video files because DateTimeOriginal does not exist
    colnames(x) <- sub("CreateDate", "DateTimeOriginal", colnames(x))
    x1 <- x[order(x$DateTimeOriginal),]
    print(paste("Exiftool completed on ", names(images3)[i], ". This is ", i, " of ", length(images3), ".", sep = ""))
    return(x1)
    rm(arg, x, x1)
  })

  if(isTRUE(pp)){
    parallel::stopCluster(cl1)
  }
  names(exif1) <- names(images3)
  print(paste("Exiftool completed at ", Sys.time(), ". Specifying image paths...", sep = ""))

  # Specify the proper paths for renaming images
  indir <- data.frame(do.call(rbind, fs::path_split(in.dir)))
  folders <- lapply(exif1, function(x){data.frame(do.call(rbind, fs::path_split(x$SourceFile)))})

  if(ncol(indir)==unique(unlist(lapply(folders, function(x){ncol(x)-1})))){  # For the case of your in directory leading directly to images
    equal.dirs <- "Y"
    message("You have selected a folder with no sub-directories. out.dir should lead directly to pictures.")
  }else if(ncol(indir) < unique(unlist(lapply(folders, function(x){ncol(x)-1})))){  # For the case where your directory has sub-directories
    equal.dirs <- "N"
    message("You have selected a folder with sub-directories. The relative paths will be preserved in out.dir")
  }else{
    stop("Something happened. in.dir has more sub-directories than the images.")
  }

  paths1 <- lapply(1:length(folders), function(i){
    x <- folders[[i]]
    if(equal.dirs=="Y"){
      relativepath <- x[,seq(1,ncol(x)-1)]
      inpath <- indir[,seq(1,ncol(indir))]
      if(out.dir.length == "one"){
        outpath <- data.frame(do.call(rbind, fs::path_split(rep(out.dir, times = nrow(x)))))
      }else if(out.dir.length == "cams"){
        outpath <- data.frame(do.call(rbind, fs::path_split(rep(out.dir[[i]], times = nrow(x)))))
      }
    }else if(equal.dirs=="N"){
      relativepath <- x[,seq(ncol(indir)+1,ncol(x)-1)]
      inpath <- x[,seq(1,ncol(x)-1)]
      if(out.dir.length == "one"){
        outpath <- data.frame(do.call(rbind, fs::path_split(out.dir)), relativepath)
      }else if(out.dir.length == "cams"){
        outpath <- data.frame(do.call(rbind, fs::path_split(out.dir[[i]])), relativepath)
      }
    }

    if(is.null(dim(relativepath))){out1 <- relativepath}else{out1 <- apply(relativepath, 1, fs::path_join)}
    if(is.null(dim(inpath))){out2 <- inpath}else{out2 <- apply(inpath, 1, fs::path_join)}
    if(is.null(dim(outpath))){out3 <- outpath}else{out3 <- apply(outpath, 1, fs::path_join)}

    return(list(relativepath = out1, inpath = out2, outpath = out3))
    rm(x, relativepath, inpath, outpath, out1, out2, out3)
  })

  print(paste("Image paths specified at ", Sys.time(), ". Cleaning up and renaming...", sep = ""))

  # Do any adjustments to date-times, if necessary
  if(is.null(adjust)){
    dt.diff <- 0
  }else{
    print("Adjustments to the date-time are being made")
    if(isTRUE(lubridate::is.difftime(adjust))){
      dt.diff <- adjust
    }else{
      if(length(adjust)!=2){stop("Two date-times must be provided in adjust. If only one is provided, it needed to be a difftime object.")}
      dt.diff <- difftime(adjust[2], adjust[1], units = "secs")
    }
  }

  # Compile all the file paths, information for new file names, and combine with metadata
  if(length(exif1) != length(folders) | length(exif1) != length(paths1) | length(folders) != length(paths1)){
    message("Something went wrong with file path specification. Renaming may not work properly")
  }

  exif2 <- lapply(1:length(exif1), function(i){
    x <- exif1[[i]]
    f <- folders[[i]]
    p <- paths1[[i]]
    datetime <- lubridate::ymd_hms(x$DateTimeOriginal) + dt.diff
    x1 <- data.frame(SourceFile = x$SourceFile,
                     inpath = p$inpath,
                     outpath = p$outpath,
                     old.name = f[,ncol(f)],
                     ext = fs::path_ext(x$SourceFile),
                     year = formatC(lubridate::year(datetime), width = 4, flag = "0"),
                     month = formatC(lubridate::month(datetime), width = 2, flag = "0"),
                     day = formatC(lubridate::day(datetime), width = 2, flag = "0"),
                     hour = formatC(lubridate::hour(datetime), width = 2, flag = "0"),
                     minute = formatC(lubridate::minute(datetime), width = 2, flag = "0"),
                     second = formatC(lubridate::second(datetime), width = 2, flag = "0")
    )
    # Add serial numbers based on the lowest sub-directory
    x1$serial <- formatC(seq(1:nrow(x1)), width = 5, flag = "0")

    # Define the new name for each file. Manipulate this to get different file names
    x1$new.name <- with(x1, paste(year, " ", month, " ", day, " ", hour, " ", minute, " ", second, " ", serial, ".", ext, sep = ""))

    # Only add serial numbers to names that are renamed
    if(isTRUE(fix.names)){
      message("Image names were only replaced if they were not in the form YYYY MM DD HH MM SS.jpg. Serial number added to all images.")
      x1$complete <- stats::complete.cases(apply(do.call(rbind, strsplit(fs::path_ext_remove(x1$old.name), " ")), 2, as.numeric))
      x1$new.name[isTRUE(x1$complete)] <- with(x1, paste(fs::path_ext_remove(old.name[isTRUE(complete)]), " ", serial[isTRUE(complete)], ext, sep = ""))
    }

    # Cleanup the output
    x2 <- x1[,c("SourceFile", "inpath", "outpath", "old.name", "new.name")]
    x3 <- merge.data.frame(x2, x, by = "SourceFile")
    if(nrow(x3) != nrow(x2)){  # One last data integrity check
      warning("Something happened. Some pictures went missing or got added between loading the exif data and renaming images.")
    }
    return(x3[,2:ncol(x3)])
    rm(x, f, p, datetime, x1, x2, x3)
  })
  names(exif2) <- names(exif1)

  # Rename the images if desired (this cannot be undone)
  if(isTRUE(pp)){
    cl1 <- parallel::makeCluster(parallel::detectCores()-cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("exif2"), envir = environment())
  }

  if(rename=="replace"){
    print(paste("Files will be renamed and replaced. Renaming started at ", Sys.time(), sep = ""))
    pbapply::pblapply(1:length(exif2), cl = cl1, function(i){
      x <- exif2[[i]]
      if(all(is.na(x))){
        print(paste("Skipping ", names(exif2)[i], " due to missing exif data.", sep = ""))
      }else{
        with(x, fs::file_move(path = file.path(inpath, old.name), new_path = file.path(outpath, new.name)))
        print(paste("Completed renaming for ", names(exif2)[i], ".", sep = ""))
      }
      rm(x)
    })
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(rename=="copy"){
    print(paste("Files will be renamed and copied. Renaming started at ", Sys.time(), sep = ""))
    pbapply::pblapply(1:length(exif2), cl = cl1, function(i){
      x <- exif2[[i]]
      if(all(is.na(x))){
        print(paste("Skipping ", names(exif2)[i], " due to missing exif data.", sep = ""))
      }else{
        with(x, fs::file_copy(path = file.path(inpath, old.name), new_path = file.path(outpath, new.name)))
        print(paste("Completed renaming for ", names(exif2)[i], ".", sep = ""))
      }
      rm(x)
    })
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(rename=="none"){
    message("No files were renamed")
  }else{
    message("No files were renamed because you did not specify a correct value for rename. Renaming can be done using the output file. To avoid this warning, choose one of c('replace', 'copy', 'none').")
  }

  if(isTRUE(pp)){
    parallel::stopCluster(cl1)
  }

  # Clean up the data and prepare to shut down the function
  print(paste("This function completed at ", Sys.time(), sep = ""))
  if(return.type == "list"){
    exif3 <- exif2
  }else if(return.type == "df"){
    exif3 <- tryCatch(do.call(dplyr::bind_rows, exif2), error = function(e){exif2})
    if(!is.data.frame(exif3)){
      warning("You tried to recombine your camera folders into a single data frame but your data has incompatible columns. \nThis is likely because you have different camera models the in.dir and used a camera-specific trigger.info. \nA list will be returned.")
    }
  }else{
    message("You chose an invalid return type. A list is returned. Choose one of c('list', 'df').")
    exif3 <- exif2
  }
  return(exif3)
  rm(Tag1, Tag2, Tag3, ext1, images1, images2, images3, images4, exif1, exif2, exif3, cl1, paths1, folders, indir, out.dir.length, equal.dirs, dt.diff)
  #rm(in.dir, out.dir, ext, trigger.info, rename, return.type, adjust, fix.names, pp, cores.left)
}

## A function to find corrupt files (Added 2022-08-25, Modified 2023-03-22) ####
##' @description A useful, but slow function for locating the name and position of corrupt images within a camera folder.
##'
##' @title Locate the name of corrupt images
##'
##' @param in.dir String. The directory containing the images that need to be checked. This should lead to only one folder of images.
##' @param file.type String. What type of file should be searched for. This can be any file extension.
##' @param pp Logical. Should the function use parallel processing? This function is extremely slow so parallel processing should help speed things up a bit.
##' @param cores.left Numeric. How many cores should be left available when using parallel processing? The default is NULL. This is only necessary when pp=TRUE. If left at the default, the function will default to 2 cores remaining which is generally enough to continue using a PC while the function runs. I would set this to be greater than 0, otherwise the function will use the entire processing power of your computer. To see how many cores you have available for parallel processing, use: parallel::detectCores().
##'
##' @details NOTE: This function is extremely slow. It uses an apply function to run exiftool on each image separately rather than as a batch group, which greatly slows down the process. Parallel processing can help improve this speed but allowing it to check multiple images at once.
##'
##' @return A list of the corrupt files and the non-corrupt files on either side of it for easy locating within the file directory.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{cameraRename3}}
##'
##' @keywords files
##'
##' @concept rename images
##' @concept corrupt images
##'
##' @importFrom exiftoolr exif_read
##' @importFrom parallel makeCluster clusterExport detectCores stopCluster
##' @importFrom pbapply pblapply
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
##'
findCorruptImages <- function(in.dir, file.type, pp, cores.left = NULL){
  #in.dir <- "K:/new_HY_20220524/RH6-1/20220524"
  #file.type <- ".jpg"

  files <- list.files(in.dir, pattern = paste(file.type, "$", sep = ""), ignore.case = T, full.names = T, recursive = T)

  if(isTRUE(pp)){
    message("Parallel processing enabled")
    if(is.null(cores.left)){
      cores.left <- 2
    }else{
      cores.left <- tryCatch(as.numeric(cores.left),
                             error = function(e){message("There was an error coercing cores.left to a number. The default of 2 cores are not utilized"); return(2)},
                             warning = function(w){message("Could not coerce cores.left to a number. The default of 2 cores are not utilized"); return(2)})
    }
    cl1 <- parallel::makeCluster(parallel::detectCores()-cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("files"), envir = environment())
  }else{
    cl1 <- NULL
  }

  all <- pbapply::pblapply(files, cl = cl1, function(x){
    tryCatch(exiftoolr::exif_read(x, tags = c("Directory")), error = function(e){return(NA)})
    print(paste("Exiftool completed on ", x, ".", sep = ""))
  })

  if(isTRUE(pp)){
    parallel::stopCluster(cl1)
  }

  corrupt <- which(is.na(all))

  all.corrupt <- all[c(corrupt-1,corrupt, corrupt+1)]

  return(all.corrupt)

  rm(files, all, corrupt, all.corrupt)
  #rm(in.dir, file.type)
}
