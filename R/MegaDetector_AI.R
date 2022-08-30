# Microsoft MegaDetector AI functions

## Move ghosts identified by the Microsoft Megadetector AI to a sorted ghosts folder (Added 2022-08-25) ####
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

