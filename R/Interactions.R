# Wildlife Interactions functions ####
## This script contains functions for preparing for doing wildlife interactions
## This script contains the following functions:
  ## interactionsDataOrganize
  ## interactionsTimelapse

################################################################################

### From a Dataorganize file (Added 2022-08-25) ####
##' @description This function uses the old workflow based on DataOrganize and will create an object for use in computing Wildlife Interactions.
##'
##' @title Create an interactions file from a dataorganize file
##'
##' @param do An AllPictures file produced by DataOrganize.
##' @param envdata Environmental variables data frame. This file must have header called "Camera" containing the list of cameras.
##' @param exclude species to exclude from the output data frame. Use c() to specify species. If you want keep all items use c(""). Unlike APFun_env, this has no default. Use it to exclude any "species" from the output file.
##' @param start_date Start date for the AllPictures file.
##' @param end_date End date for pictures. This defaults to the current date.
##'
##' @details This function is nearly identical to the APFun_env except it is simpler because it does not have to calculate an interval for independent events and it outputs a slightly different object.
##'
##' For a detailed description of what these interactions are, see any of our or UTRGV's reports on the TxDOT camera monitoring projects
##'
##' @return A file containing the list of images and species for interactions.
##'
##' @references Tewes, M., J. Lombardi, Z. Wardle, and T. Yamashita. 2020. Ocelot and Jaguarundi Monitoring Project: Evaluating the Effectiveness of Wildlife Crossings, Cattle Guards, and Fencing on Road Facilities in Cameron County, Contract No. 57-9XXIA003, 0000018485. Feline Research Program, Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville.
##'
##' Kline, R., K. Ryer, A. Rivera, T. Yamashita, and T. Hopkins. 2020. Post-construction Monitoring Bi-annual Report for SH 100: May 2019 thru October 2019 (Contract No 57-9XXIA001). The University of Texas Rio Grande Valley.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{calculateEvents}}
##'
##' \code{\link{interactionsTimelapse}}
##'
##' @keywords manip
##'
##' @concept DataOrganize
##' @concept interactions
##'
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
interactionsDataOrganize <- function(do, envdata, exclude, start_date, end_date=Sys.Date()){
  # Formatting the columns in the AllPictures file
  colnames(do) <- c("Camera","Species","Year","Month","Day","Hour","Minute","Second","Individuals")
  x1=subset(do, !(Species %in% exclude))
  x1[,4] <- formatC(x1[,4], width = 2, format = "d", flag = "0")
  x1[,5] <- formatC(x1[,5], width = 2, format = "d", flag = "0")
  x1[,6] <- formatC(x1[,6], width = 2, format = "d", flag = "0")
  x1[,7] <- formatC(x1[,7], width = 2, format = "d", flag = "0")
  x1[,8] <- formatC(x1[,8], width = 2, format = "d", flag = "0")

  x1$Date_Time <- with(x1, paste(Year, Month, Day, Hour, Minute, Second))
  x1$DateTimeOriginal=as.POSIXct(strptime(x1$Date_Time,'%Y %m %d %H %M %S'))
  x1$Date=as.POSIXct(strptime(x1$Date_Time,'%Y %m %d'))

  x2 <- merge.data.frame(x1, envdata, by = "Camera")
  x2a=subset(x2,Date >= start_date)
  x2b=subset(x2a,Date <= as.character(end_date))
  x3 <- x2b

  x4 <- x3[,c("Type", "Site", "Camera", "Species", "Date_Time", "Individuals")]
  x4$Class <- NA
  x4$Direction <- NA

  return(x4)
  rm(x1,x2,x2a,x2b,x3,x4)
  #rm(x,y,exclude, start_date, end_date)
}

### From a timelapse file (Added 2022-08-25, Modified 2022-08-31) ####
##' @description This function copies images for interactions to a new folder and creates an interactions file.
##'
##' @title Create an Interactions File from a Timelapse Output
##'
##' @param timelapse A timelapse csv, formatted using our Timelapse template.
##' @param envdata Environmental variables data frame. This file must have header called "Camera" containing the list of cameras, "Site" containing the site name, "Side" indicating which side the camera is on, and "Type" indicating what type of structure the camera is at.
##' @param in.dir String. The directory where the original image data is. This should be the same folder containing the images folder.
##' @param out.dir String. Defaults to NULL. The directory where you want to store the Interactions images. The default specifies the in.dir. Options include c("in.dir") to more explicitly refer to the in.dir.
##' @param create.dirs Logical. Defaults to TRUE. Should new directories be checked for and created by R if necessary?
##' @param type String. Defaults to "none". Should image files be copied or not to the appropriate directories?
##' @param exclude species to exclude from the output data frame. Use c() to specify species. If you want keep all items use c(""). Unlike APFun_env, this has no default. Use it to exclude any "species" from the output file.
##'
##' @details NOTE: This function will add a (#) to duplicate file names, similar to the way Windows handles copying files with the same name into the same folder.  of handling duplicate file names so it will skip duplicates when copying.
##'
##' @return A list containing the data file and a file of the old and new paths and names.
##' @return Interactions:
##' The Interactions data file.
##' @return Files:
##' File used for copying files.
##'
##' @references Tewes, M., J. Lombardi, Z. Wardle, and T. Yamashita. 2020. Ocelot and Jaguarundi Monitoring Project: Evaluating the Effectiveness of Wildlife Crossings, Cattle Guards, and Fencing on Road Facilities in Cameron County, Contract No. 57-9XXIA003, 0000018485. Feline Research Program, Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville.
##'
##' Kline, R., K. Ryer, A. Rivera, T. Yamashita, and T. Hopkins. 2020. Post-construction Monitoring Bi-annual Report for SH 100: May 2019 thru October 2019 (Contract No 57-9XXIA001). The University of Texas Rio Grande Valley.
##'
##' @section {Standard Disclaimer}: As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
##' If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
##' These functions are built for very specific purposes and may not generalize well to whatever you need it for.
##' I build them for my own use and make no promises that they will work for different data formatting situations.
##' As I come across errors, I attempt to further generalize the functions but this is done as I go.
##'
##' @seealso \code{\link{interactionsDataOrganize}}
##'
##' @keywords manip
##'
##' @concept timelapse
##' @concept interactions
##'
##' @importFrom fs file_copy
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }

interactionsTimelapse <- function(timelapse, envdata, in.dir, out.dir = NULL, create.dirs = T, type = "copy", exclude = c("ghost")){
  #timelapse <- read.csv("timelapse_out_20221201.csv")
  #envdata <- openxlsx::read.xlsx(file.choose())
  #in.dir <- getwd()
  #out.dir <- NULL
  #create.dirs <- F
  #type <- "none"
  #exclude <- c("ghost", "human", "bird", "rodent", "unk_lizard", "spiny_lizard", "whiptail_lizard", "leopard_frog", "unk_amphibian")

  # Check if in.dir and out.dir exist
  ## in.dir
  if(!dir.exists(in.dir)){
    stop("in.dir does not exist. Check your directory name")
  }
  ## out.dir
  if(is.null(out.dir)){
    out.dir <- in.dir
  }else if(out.dir == "in.dir"){
    out.dir <- in.dir
  }else if(!dir.exists(out.dir)){
    stop("out.dir does not exist. Do you need to create it? If you want out.dir==in.dir, specify either c(NULL,'in.dir').")
  }

  # Step 1: Combine the Species1, Species2, Species3, and SpeciesOther columns (run APFun_timelapse basically)
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

  imagesout1 <- rbind(images1,images2,images3,images4)

  print(paste("There were ", nrow(imagesout1), " rows of data.", sep = ""))

  # Step 2: Exclude all species that we do not want to do interactions on
  imagesout2 <- subset(imagesout1, !(Species %in% exclude))

  print(paste(nrow(imagesout2), " rows contained an animal of interest.", sep = ""))

  # Step 3: Create new file paths for the interactions data
  imagesout3 <- data.frame(imagesout2, do.call(rbind,strsplit(imagesout2$Path, split = "\\\\")))
  colnames(imagesout3) <- c("File", "Path", "Species", "Individuals", "Folder", "Camera", "Date")
  imagesout4 <- merge.data.frame(imagesout3, envdata, by = "Camera", all.x = T)

  imagesout4$in.dir <- in.dir
  imagesout4$out.dir <- out.dir
  imagesout4$oldpath <- with(imagesout4, file.path(Folder, Camera, Date))
  imagesout4$newpath <- with(imagesout4, paste("Interactions_", Date, "/", Site, "/", Side, "/", Species, sep = ""))
  imagesout4$fileold <- imagesout4$File
  dups <- which(duplicated(file.path(imagesout4$newpath, imagesout4$File)))
  if(length(dups)>0){
    message(paste("There were ", length(dups), " files with the same name as other files in the folder. \nA (#) will be appended to the duplicated file names.", sep = ""))
    imagesout4$filenew <- ifelse(duplicated(file.path(imagesout4$newpath, imagesout4$File)), sapply(1:length(dups), function(i){imagesout4$File[dups[i]] <- paste(sub(".jpg", "", imagesout4$File[dups[i]], ignore.case = T), " (", i, ").jpg", sep = "")}), imagesout4$File)
  }else{
    imagesout4$filenew <- imagesout4$File
  }

  # Step 4: Create the interactions file
  intfile1 <- data.frame(imagesout4[,c("Type", "Site", "Camera", "Species")], Date_time = sub(".jpg", "", imagesout4$File, ignore.case = T), Individuals = imagesout4$Individuals, Class = NA, Direction = NA)

  # Step 5: Create new directories and subdirectories and copy pictures into them (optional)
  if(isTRUE(create.dirs)){
    print("Creating directories")
    dirs <- with(imagesout4, list(unique(file.path(out.dir, paste("Interactions_", Date, sep = ""))),
                                  unique(file.path(out.dir, paste("Interactions_", Date, sep = ""), Site)),
                                  unique(file.path(out.dir, paste("Interactions_", Date, sep = ""), Site, Side)),
                                  unique(file.path(out.dir, paste("Interactions_", Date, sep = ""), Site, Side, Species))))
    dirsTemp <- lapply(dirs, function(x){
      lapply(x, function(y){
        ifelse(!dir.exists(y), dir.create(y), print("Folder exists"))
      })
    })
  }

  if(type == "copy"){
    print("File transfer in progress. Images are copied from in.dir to out.dir")
    with(imagesout4, fs::file_copy(path = file.path(in.dir, oldpath, fileold), new_path = file.path(out.dir, newpath, filenew)))
  }else if(type == "none"){
    print("No file transfer specified")
  }else{
    message("You chose an invalid type. No file transfer will occur. Choose one of c('copy', 'none') to avoid this warning. Please note, the option 'move' does not exist for this function.")
  }

  return(list(Interactions = intfile1, Files = imagesout4))
  rm(images1, images2, images3, images4, imagesout1, imagesout2, imagesout3, imagesout4, dups, intfile1, dirs, dirsTemp)
  #rm(timelapse, envdata, in.dir, out.dir, create.dirs, type, exclude)
}

