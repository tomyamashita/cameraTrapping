# DataOrganize Files

## Create a dataorganize like output from sorted images (Added 2022-08-25) ####
##' @description This is an R version of the DataOrganize program developed by Jim Sanderson and Grant Harris. While untested, it should provide a little more flexibility in naming of folders than the original DataOrganize program. It also can do basic diagnostics so you can check camera and species names.
##'
##' @title DataOrganize
##'
##' @param inputdir The directory containing the camera folders.
##' @param diagnostics Logical. Should diagnostic information be outputted to the console? This is set to TRUE by default.
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
dataorganize <- function(inputdir, diagnostics = T){
  #inputdir <- "J:/AI_Test_Microsoft/Test/AI_test"
  #diagnostics <- TRUE

  filelist <- list.files(inputdir, pattern = "jpg", ignore.case = T, full.names = T, recursive = T)
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
  return(out)

  rm(filelist, files1, files2, images1, images2, out)
  #rm(inputdir, diagnostics)
}

## From a dataorganize file, create a usable output (Added 2022-08-25) ####
##' @description Function for combining an AllPictures text file created by DataOrganize with environmental variables associated with individual camera traps. The No Interval version of the function uses an interval of 1 second and does not recalculate # number of individuals.
##'
##' @title All Pictures Function with Environmental Variables for a DataOrganize output
##'
##' @param x AllPictures file produced by DataOrganize.
##' @param y Environmental variables data frame. This file must have header called "Camera" containing the list of cameras.
##' @param sort.col Column that you want to sort pictures by to create independent events. This should be one of c("Camera", "Site", "Station") depending on your wording for sites. This defaults to "Camera".
##' @param exclude species to exclude from the output data frame. Use c() to specify species. This defaults to excluding ghosts. If you want keep all items use c("").
##' @param start_date Start date for the AllPictures file. If you want all data, set this to an arbitrarily early date.
##' @param end_date End date for pictures. This defaults to the current date.
##' @param interval Time in seconds between pictures for an independent event. See Details for some normal interval times. This is only necessary for the main function. It defaults to no interval.
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
##' @note As with all of my functions, this assumes a very particular formatting for your data. If the EnvData table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
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
APFun_env <- function(x,y,sort.col="Camera",exclude=c("ghost"),start_date,end_date=Sys.Date(),interval=NULL,all.pics=FALSE){
  # This function takes the allpictures.txt file and changes it into a form that is usable for analyses in R
  # Inputs:
  ## x = allpictures file produced by Dataorganize
  ## y = environmental variables file. This file must have header called "Camera" containing the list of cameras
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
    interval=1
  }

  # Formatting the columns in the AllPictures file
  colnames(x)=c("Camera","Species","Year","Month","Day","Hour","Minute","Second","N.Individuals")
  if(isTRUE(is.na(exclude))){
    x1 <- x
  }else{
    x1=subset(x, !(Species %in% exclude))
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
  x2 <- merge.data.frame(x1,y,by = "Camera")
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
    #rm(x5b2, x5b3)
  }else{
    x6 <- merge.data.frame(x5, x5c, by="Ident2", all.x=TRUE, sort.x=TRUE)
    x7 <- x6[,c(2,seq(2,length(y))+14,3,length(y)+length(x1)+5,11,12,13,14,length(y)+length(x1)+1,length(y)+length(x1)+2)]
  }
  return(x7)
  #rm(x1, x2, x2a, x2b, x3, x4, x5, x5a, x5b, x5c, x6, x7)
  #rm(x, y, sort.col, exclude, start_date, end_date, interval, all.pics)
}

