# Timelapse Outputs

## Convert a Timelapse file to a dataorganize output (Added 2022-08-25) ####
##' @description This function converts a Timelapse csv file to the dataorganize file output format. This was done this way because many of my core functions for processing camera trap data depend on the existence of a data frame created by APFun_env. Converting a timelapse file to this format is just the easiest way to maintain consistency.
##'
##' @title Convert a Timelapse csv to a format for use with APFun_env
##'
##' @param timelapse data.frame. A data frame representing a properly formatted Timelapse csv file
##'
##' @return An R object formatted in the same style as a dataorganize text file.
##'
##' @note This function is designed to make timelapse files compatible with DataOrganize files which are used for most of the analyses in this package
##'
##' @seealso \code{\link{APFun_env}}
##'
##' @keywords datagen
##'
##' @concept camera trapping
##' @concept timelapse
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
APFun_Timelapse <- function(timelapse){
  #timelapse <- AP1_t

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
  x2 <- data.frame(do.call(rbind, strsplit(x1$Path, split = "\\\\")), do.call(rbind, strsplit(sub(".JPG", "", x1$File, ignore.case = T), split = " ")), x1[,3:4])
  x3 <- x2[,c(2,11,4:9,12)]
  colnames(x3) <- paste("V", seq(1:ncol(x3)), sep = "")
  x4 <- data.frame(x3[,1:2], apply(x3[,3:ncol(x3)], 2, as.integer))

  return(x4)
  rm(images1, images2, images3, images4, x1, x2, x3, x4)
  #rm(timelapse)
}

## Move all pictures into sorted folders (Added 2022-08-25) ####
##' @description This function uses a Timelapse csv file to move or copy images from an unsorted folder to sorted folders based on species and number of individuals (in the same format as required for \code{link{dataorganize}}), although see details.
##'
##' @title Move pictures from unsorted to sorted folders
##'
##' @param timelapse data.frame. A data frame of a timelapse file. Note, this should follow the timelapse template that I typically use
##' @param in.dir String. The directory containing the unsorted images
##' @param out.dir String. The directory where you want to store the sorted images
##' @param create.dirs Logical. Should the function create the directories it needs?
##' @param type String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none')
##'
##' @details When this function creates its folder structure, it uses the Individuals column in the Timelapse output. For some "species" (e.g., ghost, human, bird, rodent), we do not sort these by individual, therefore the Individuals column is a 0. These species get assigned a folder of 00 for their number of individuals. I do not know how this will affect quality control and the workflow down the line. Generally, it should not be an issue but could result in NA values in the Individuals column of the \code{\link{APFun_env}} or errors in the \code{\link{dataorganize}} functions in this package. Once this has been tested, I will update this.
##'
##' @return list of the full file path to the in files and out files
##' @return in.files:
##' String. Full file paths to the in files
##' @return out.files:
##' String. Full file paths to the out files
##'
##' @seealso \code{\link{dataorganize}}
##'
##' \code{\link{APFun_env}}
##'
##' @keywords files
##' @keywords manip
##'
##' @concept camera trapping
##' @concept timelapse
##'
##' @importFrom fs file_move file_copy
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
movePictures <- function(timelapse, in.dir, out.dir, create.dirs, type = "none"){
  #timelapse <- read.csv("K:/Completed/new_20210927/timelapse_out_20210927.csv")
  #in.dir <- "K:/Completed/new_20210927"
  #out.dir <- "K:/Completed/new_20210927/sorted"
  #create.dirs <- T
  #type <- "none"

  print(paste("This function started at ", Sys.time(), sep = ""))

  if(!dir.exists(in.dir)){
    stop("Your in.dir does not exist. Did you specify the correct path")
  }
  if(grepl("images", in.dir, ignore.case = T)){
    message("Your in.dir path includes the images folder. File transfer may not work properly if timelapse also references this folder")
  }
  if(!dir.exists(out.dir)){
    stop("Your out.dir does not exist. Did you forget to create it?")
  }

  timelapse$RelativePath <- gsub("\\\\", "/", timelapse$RelativePath)

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
  x2 <- data.frame(do.call(rbind, strsplit(x1$Path, split = "/")), x1[,c("File", "Species", "Individuals")])
  colnames(x2)[-c(ncol(x2)-2,ncol(x2)-1,ncol(x2))] <- c("Folder", "Camera", "Date")
  x2$Individuals <- formatC(x2$Individuals, flag = "0", width = 2)
  x3in <- with(x2, file.path(Folder, Camera, Date, File))
  x3out <- with(x2, file.path(Camera, Species, Individuals, File))

  if(length(x3in) != length(x3out)){
    stop("You have different numbers of files in the 'in' and 'out' directories. You broke my function...")
  }

  rename <- list(in.files = file.path(in.dir, x3in),
                 out.files = file.path(out.dir, x3out))
  print(paste("The in files will look like: ", rename[["in.files"]][[1]], sep = ""))
  print(paste("The out files will look like: ", rename[["out.files"]][[1]], sep = ""))

  if(isTRUE(create.dirs)){
    print("Creating Directories")
    dirs <- with(x2, list(unique(paste(out.dir, Camera, sep = "/")),
                          unique(paste(out.dir, Camera, Species, sep = "/")),
                          unique(paste(out.dir, Camera, Species, Individuals, sep = "/"))))
    dirsTemp <- lapply(dirs, function(x){
      lapply(x, function(y){
        ifelse(!dir.exists(y), dir.create(y), print("Folder exists"))
      })
    })
  }

  if(length(unique(x3in)) != length(unique(x3out))){
    message("You have a different number of in and out files, likely because more than one species was detected in a single picture. \nSuggest using 'copy' instead of 'move' for images.")
  }

  if(type=="move"){
    print("File transfer in progress. Images are moved from in.dir to out.dir")
    test <- fs::file_move(path = rename[["in.files"]], new_path = rename[["out.files"]])
  }else if(type=="copy"){
    print("File transfer in progress. Images are copied from in.dir to out.dir")
    test <- fs::file_copy(path = rename[["in.files"]], new_path = rename[["out.files"]])
  }else if(type=="none"){
    print("No file transfer specified")
  }else{
    message("You chose an invalid type. No file transfer will occur. Choose one of c('move', 'copy', 'none') to avoid this warning")
  }

  print(paste("This function completed at ", Sys.time(), sep = ""))
  return(rename)
  #rm(timelapse, images1, images2, images3, images4, x1, x2, x3in, x3out, dirs, dirsTemp, rename)
  #rm(timelapsefile, in.dir, out.dir, create.dirs, type)
}

## Extract best pictures from the timelapse file (Added 2022-08-25) ####
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

