# Quality Control Functions

### Converting date-time information in a CT Table to character format (Added 2022-08-25)
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
ctdates_fun <- function(cttable, start.col=6){
  #cttable = CT_FM1847
  #start.col = 6

  for(i in start.col:ncol(cttable)){
    cttable[,i] <- as.character(cttable[,i])
  }
  return(cttable)
  rm(cttable, start.col)
}

### Quality control for timelapse-sorted images (Added 2022-08-25)
##' @description This function outputs the unique species from a timelapse file, a list of rows that were not sorted by species, and a list of rows that were not sorted by individual. You should use this to check whether a set of images was completely sorted.
##'
##' @title Quality control for timelapse
##'
##' @param ds A data frame created from a csv file exported from Timelapse. Note, this function is set up to use my timelapse template.
##' @param exclude String. Which species should not be checked for number of individuals? The default is NULL which will only exclude c("ghost", "human", "", "rodent", "bird").
##'
##' @details This function will check if there are video files (.MP4) then check the number of unique species, whether any image was not sorted by species by checking if the species1 column contains any "" values, and whether any image was not sorted by individuals by checking if a column sorted into a species was not sorted by individual. Ghosts, humans, "", birds, rodents, lizards, and amphibians are not sorted by individual under the current protocol, and are therefore not included in this check. Video files are also not checked by individual.
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
timelapseQC <- function(ds, exclude=NULL){
  # ds <- read.csv("timelapse_out_20220117_BL.csv)

  # Check for video files
  if(isTRUE(any(grepl(".MP4", ds$File, ignore.case = T)))){
    message("Video files detected. Number of individuals only analyzed for images")
    video <- ds[grep(".MP4", ds$File, ignore.case = T),]
    images <- ds[-grep(".MP4", ds$File, ignore.case = T),]
  }else{
    print("No video files detected.")
    images <- ds
  }

  # Check species names
  unique_spec <- list(
    unique1 = unique(ds$Species1),
    unique2 = if(!any(is.na(unique(ds$Species2)))){unique(ds$Species2)[unique(ds$Species2)!=""]},
    unique3 = if(!any(is.na(unique(ds$Species3)))){unique(ds$Species3)[unique(ds$Species3)!=""]},
    unique4 = if(!any(is.na(unique(ds$SpeciesOther)))){unique(ds$SpeciesOther)[unique(ds$SpeciesOther)!=""]}
  )
  unique_species <- unique(do.call(c, unique_spec))

  # Check missing species data in the species 1 column
  missing_spec <- ds[ds$Species1=="",]

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

  # Output the results
  print(paste("Unique species: ", paste(unique_species, collapse = ", "), sep = ""))
  if(nrow(missing_spec)==0){
    print("All files were labelled by species")
  }else{
    message(paste(nrow(missing_spec), " file(s) were not labelled by species. These were: \n", paste(missing_spec$File, collapse = "\n"), sep = ""))
  }
  if(nrow(missing_ind)==0){
    print("All files were labelled by individual")
  }else{
    message(paste(nrow(missing_ind), " file(s) were not labelled by individual. These were: \n", paste(missing_ind$RelativePath, missing_ind$File, collapse = "\n", sep = "\\"), sep = ""))
  }

  out <- list("Unique Species" = unique_species, "Missing Species" = missing_spec, "Missing Ind" = missing_ind)
  return(out)
  rm(unique_spec, unique_species, missing_spec, exclude, no_ind, missing_ind, out)
  #rm(ds)
}
