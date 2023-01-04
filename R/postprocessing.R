# Post-processing
## This script contains functions for post-sorting processing of camera data,
  #including "auto"-sorting pictures, extracting best pics from a timelapse file,
  #removing ghosts, and running data organize.

################################################################################

### From a dataorganize file, create a usable output (Added 2022-08-25, Modified 2022-08-31) ####
##' @description Function for combining an AllPictures text file created by DataOrganize with environmental variables associated with individual camera traps. The No Interval version of the function uses an interval of 1 second and does not recalculate # number of individuals.
##'
##' @title All Pictures Function with Environmental Variables for a DataOrganize output
##'
##' @param do data frame. A DataOrganize file produced by the DataOrganize program or the dataOrganize function in this package.
##' @param envdata Environmental variables data frame. This file must have header called "Camera" containing the list of cameras.
##' @param sort.col string. Column that you want to sort pictures by to create independent events. This should be one of c("Camera", "Site", "Station") depending on your wording for sites. This defaults to "Camera".
##' @param exclude string. species to exclude from the output data frame. Use c() to specify species. This defaults to excluding ghosts. If you want keep all items use c("").
##' @param start_date string. Start date for the AllPictures file. If you want all data, set this to an arbitrarily early date.
##' @param end_date string. End date for pictures. This defaults to the current date.
##' @param interval integer. Time in seconds between pictures for an independent event. See Details for some normal interval times. This is only necessary for the main function. It defaults to no interval.
##' @param all.pics Logical. Whether or not you want to return all the pictures or just independent events. If all.pics = TRUE, all pictures will be returned with an index number associated with independent events. This defaults to FALSE.
##'
##' @details Suggested interval times: 61 = 1 minute, 1801 = 30 minutes. 3601 = 1 hour
##'
##' One second should be added to the total to ensure the full interval is included.
##'
##' @return A data frame of camera data. Output varies depending on whether or not all.pics is true or false.
##' @return all.pics = FALSE:
##' Returns a data frame of only independent events at the specified interval.
##' @return all.pics = TRUE:
##' Returns a data frame of all pictures from the input file. A index number is added associated with independent events.
##'
##' @note As with all of the functions in this package, this assumes a very particular formatting for your data. If the EnvData table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
##'
##' @seealso \code{\link{APFun_Timelapse}}
##'
##' \code{\link{dataorganize}}
##'
##' @keywords datagen
##'
##' @concept camera trapping
##' @concept dataorganize
##'
##' @importFrom dplyr arrange summarise group_by
##' @importFrom pbapply pbsapply
##' @importFrom zoo na.locf
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
APFun_env <- function(do, envdata, sort.col="Camera", exclude=c("ghost"), start_date, end_date=Sys.Date(), interval=NULL, all.pics=FALSE){
  # This function takes a DataOrganize file and changes it into a form that is usable for analyses in R
  # Inputs:
  ## do = file produced by Dataorganize program or dataOrganize function
  ## envdata = environmental variables file. This file must have header called "Camera" containing the list of cameras
  ## sort.col = Column which you want to sort your pictures by. It should most likely either be site or camera. Defaults to camera
  ## exclude = The species you want to subset out so that you don't get information on them. Defaults to ghost
  ## start_date = start date for the AllPictures file
  ## end_date = end date for pictures. Defaults to current date
  ## interval = time in seconds between pictures for an independent event. Defaults to none
  ## all.pics = Logical on whether you want all the pictures or just the independent events
  # Outputs:
  ## A data frame containing independent events in date-time format + camera/site specific variables

  # Defining an interval if interval left as default
  if(is.null(interval)==TRUE){
    interval <- 1
  }

  # Formatting the columns in the AllPictures file
  colnames(do) <- c("Camera","Species","Year","Month","Day","Hour","Minute","Second","N.Individuals")
  if(isTRUE(is.na(exclude))){
    x1 <- do
  }else{
    x1=subset(do, !(Species %in% exclude))
  }
  x1$Month_Day <- paste(formatC(x1[,"Year"], width = 4, format = "d", flag = "0"),
                        formatC(x1[,"Month"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Day"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Hour"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Minute"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Second"], width = 2, format = "d", flag = "0"), sep = "")
  x1$DateTimeOriginal <- as.POSIXct(strptime(x1$Month_Day,'%Y%m%d%H%M%S'))
  x1$Date <- as.POSIXct(strptime(x1$Month_Day,'%Y%m%d'))
  x1$hms <- format(x1$DateTimeOriginal,format="%H:%M:%S")
  x1$delta.time.secs[1] <- 0

  # Adding in environmental data and time of day data
  x2 <- merge.data.frame(x1, envdata, by = "Camera")
  if(nrow(x2)==0){
    stop("Something went wrong merging the camera data to the envdata. Check your column and camera names in your envdata file.")
  }
  x2$time_numeric <- (as.numeric(x2$Hour)*3600 + as.numeric(x2$Minute)*60 + as.numeric(x2$Second))/(86400)
  x2$time_radians <-2*pi*x2$time_numeric

  # Subsetting the data by date
  x2a <- subset(x2,Date >= start_date)
  x2b <- subset(x2a,Date <= as.character(end_date))
  if(nrow(x2b)==0){
    stop("The specified date range is outside the range of the data")
  }
  x3 <- x2b

  # Calculating the time difference between pictures
  x3["Sort"] <- x3[sort.col]
  x4=dplyr::arrange(x3, Sort, Species, Month_Day)
  x4$delta.time.secs <- pbapply::pbsapply(1:(nrow(x4)), FUN = function(i){
    ifelse(i-1==0, interval,
           ifelse(x4$Species[i]==x4$Species[i-1],
                  difftime(x4$DateTimeOriginal[i], x4$DateTimeOriginal[i-1], units = "secs"), interval))
    #rm(i)
  })
  x4$Ident <- seq(1,nrow(x4))

  # Identifying and subsetting out independent events
  x5 <- x4[!(x4$delta.time.secs < interval),]
  x5$Ident2 <- seq(1, nrow(x5))
  x5a <- merge.data.frame(x5, x4, by="Ident", all.y=TRUE, sort.y=TRUE)
  x5b <- data.frame(x5a[,c(1,(ncol(x5)+1):ncol(x5a))], Ident2 = zoo::na.locf(x5a$Ident2, fromLast=FALSE))
  x5c <- dplyr::summarise(dplyr::group_by(x5b, Ident2), Individuals = max(`N.Individuals.y`))

  # Sorting the columns and removing unnecessary columns
  if(all.pics==TRUE){
    x5b2 <- x5b[,1:ncol(x5)]
    x5b3 <- x5b2[,c(3:ncol(x5b2)-1,1,ncol(x5b2))]
    colnames(x5b3) <- colnames(x5)
    x6 <- merge.data.frame(x5b3, x5c, by="Ident2", all.x=TRUE, sort.x=TRUE)
    x7 <- x6[,c(1,2,seq(2,length(y))+14,3,length(y)+length(x1)+5,11,12,13,14,length(y)+length(x1)+1,length(y)+length(x1)+2)]
    rm(x5b2, xb53)
  }else{
    x6 <- merge.data.frame(x5, x5c, by="Ident2", all.x=TRUE, sort.x=TRUE)
    x7 <- x6[,c(2,seq(2,length(y))+14,3,length(y)+length(x1)+5,11,12,13,14,length(y)+length(x1)+1,length(y)+length(x1)+2)]
  }
  return(x7)
  rm(x1, x2, x2a, x2b, x3, x4, x5, x5a, x5b, x5c, x6, x7)
  #rm(x, y, sort.col, exclude, start_date, end_date, interval, all.pics)
}

### Convert a Timelapse file to a dataorganize output (Added 2022-08-25) ####
##' @description This function converts a Timelapse csv file to the dataorganize file output format. This was done this way because many of my core functions for processing camera trap data depend on the existence of a data frame created by APFun_env. Converting a timelapse file to this format is just the easiest way to maintain consistency.
##'
##' @title Convert a Timelapse csv to a format for use with APFun_env
##'
##' @param timelapse data.frame. A data frame representing a Timelapse csv file formatted using my timelapse template.
##'
##' @details The timelapse file must contain the following column names: c("Species1", "Species1_Ind", "Species2", "Species2_Ind", "Species3", "Species3_Ind", "SpeciesOther", "Other_Ind").
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

### Create a dataorganize like output from sorted images (Added 2022-08-25) ####
##' @description This is an R version of the DataOrganize program developed by Jim Sanderson and Grant Harris. While untested, it should provide a little more flexibility in naming of folders than the original DataOrganize program. It also can do basic diagnostics so you can check camera and species names.
##'
##' @title DataOrganize
##'
##' @param in.dir data.frame. The directory containing the camera folders.
##' @param save logical. Should the file be saved to the working directory? The default is FALSE. If TRUE, this will create a file called dataorganize.txt containing the output. Be careful, as this can overwrite other files with this name.
##' @param diagnostics logical. Should diagnostic information be outputted to the console? This is set to TRUE by default.
##'
##' @return A data frame organized in the same way as the DataOrganize program: Camera Species Year Month Day Hour Minute Second Num_of_Individuals.
##'
##' @references Original DataOrganize program: \url{https://smallcats.org/resources/}
##'
##' @seealso \code{\link{APFun_env}}
##'
##' @keywords files
##' @keywords manip
##'
##' @concept camera trapping
##' @concept DataOrganize
##'
##' @importFrom dplyr summarise group_by n
##' @export
##'
##' @examples \dontrun{
##' # No example provided
##' }
dataOrganize <- function(in.dir, save = F, diagnostics = T){
  #in.dir <- "J:/AI_Test_Microsoft/Test/AI_test"
  #diagnostics <- TRUE

  filelist <- list.files(in.dir, pattern = "jpg", ignore.case = T, full.names = T, recursive = T)
  files1 <- do.call(rbind, strsplit(filelist, "/"))
  files2 <- files1[,(ncol(files1)-3):ncol(files1)]
  images1 <- sub(".jpg", "", ignore.case = T, files2[,4])
  images2 <- do.call(rbind, strsplit(images1, " "))

  out <- data.frame(files2[,1:3], images2[,1:6])
  colnames(out) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
  if(diagnostics == T){
    diagn <- list("Sites and Species" = data.frame(dplyr::summarise(dplyr::group_by(out, site), species = length(unique(species)), pictures = dplyr::n())),
                  "Unique Species" = sort(unique(out$species)),
                  "Unique Number of Individuals" = sort(unique(out$individuals)))
    print(diagn)
    rm(diagn)
  }

  if(isTRUE(save)){
    write.table(out, file = "dataorganize.txt", row.names = F)
  }

  return(out)

  rm(filelist, files1, files2, images1, images2, out)
  #rm(in.dir, diagnostics)
}

## Move all pictures into sorted folders (Added 2022-08-25, Modified 2022-09-13) ####
##' @description This function uses a Timelapse csv file to move or copy images from an unsorted folder to sorted folders based on species and number of individuals (in the same format as required for \code{link{dataorganize}}), although see details.
##'
##' @title Move pictures from unsorted to sorted folders
##'
##' @param timelapse data.frame. A data frame of a timelapse file. Note, this should follow the timelapse template that I typically use
##' @param in.dir String. The directory containing the root folder for the timelapse file. For example, if all your images were a folder called "images" which sits in an external drive, labelled "F:", then you would specify the in.dir as "F:".
##' @param out.dir String. The directory where you want to store the sorted images
##' @param create.dirs Logical. Should the function create the directories it needs?
##' @param type String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none')
##' @param exclude String. Which species should not be sorted? The default is NULL which sorts all species. This can take multiple inputs. Use c("Species1", "Species2", "etc") to specify unique species
##'
##' @details When this function creates its folder structure, it uses the Individuals column in the Timelapse output.
##' For some "species" (e.g., ghost, human, bird, rodent), we do not sort these by individual, therefore the Individuals column is a 0.
##' These species get assigned a folder of 00 for their number of individuals. I do not know how this will affect quality control and the workflow down the line.
##' Generally, it should not be an issue but could result in NA values in the Individuals column of the \code{\link{APFun_env}} or errors in the \code{\link{dataorganize}} functions in this package.
##' Once this has been tested, I will update this.
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
movePictures <- function(timelapse, in.dir, out.dir, create.dirs, type = "none", exclude = NULL){
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
  x3 <- x2[!(x2$Species %in% exclude), ]  # Remove species from list of those to be sorted

  x3in <- with(x3, file.path(Folder, Camera, Date, File))
  x3out <- with(x3, file.path(Camera, Species, Individuals, File))

  if(length(x3in) != length(x3out)){
    stop("You have different numbers of files in the 'in' and 'out' directories. You broke my function...")
  }

  rename <- list(in.files = file.path(in.dir, x3in),
                 out.files = file.path(out.dir, x3out))
  print(paste("The in files will look like: ", rename[["in.files"]][[1]], sep = ""))
  print(paste("The out files will look like: ", rename[["out.files"]][[1]], sep = ""))

  if(isTRUE(create.dirs)){
    print("Creating Directories")
    dirs <- with(x3, list(unique(paste(out.dir, Camera, sep = "/")),
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
