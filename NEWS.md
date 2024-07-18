# cameraTrapping (development version)

## Version 0.0.0.1 (2022-08-25)

* Package created. 
* Added the GPLv3 License
* Added a `README.md` file to describe the package
* Added a `NAMESPACE` file
* Added a `NEWS.md` file to track changes to the package.
* Added package dependencies and imports
* Migrated many functions and their associated documentation from their original sources in my personal package.
 
## Version 0.0.0.2 (2022-08-25)

* Prepared data for use with Roxygen2. Ran roxygen2::roxygenise()

## Version 0.0.0.3 (2022-08-30)

* Added the cameraDiagnostics function and associated documentation
* Fixed an error with the description file
* Fixed an error in the documentation for the timeConvert function where the lubridate package was improperly specified
* Actually checked if the package could be built and fixed associated errors

## Version 0.0.0.4 (2022-08-31)

* Added an additional error code to the APFun_env function to ensure that camera names are included in the envdata file
* Modified several functions so that the input requirements are more consistent. "timelapse" is used any time a timelapse file is required as the input. "x" is used for other inputs related to camera data. "ds" is used for other inputs not related to camera data.

## Version 0.0.0.5 (2022-09-05)

* Added version numbers to imported packages. I used my current version numbers since I have not tested on any other versions of these packages.
* Fixed cameraDiagnostics function to compute a POSIX object from the poorly formatted exif date-times before calculating min and max dates. Also fixed issue when multiple cameras have the same failed end date.  

## Version 0.0.0.6 (2022-09-09)

* Modified the trapeffort_fun function to fix a number of errors, including with the ability to specify sessions and output camera names. 

## Version 0.0.0.7 (2022-09-13)

* Modified the timelapseQC function to also check for situations where Species that should not be labelled by individuals are
* Modified the movePictures function to allow users to exclude certain species from being "sorted". 
* Updated the documentation for the APFun_timelapse function to better describe the timelapse template requirements.
* Renamed several functions to have names more in line with other functions in this package.

## Version 0.0.0.8 (2022-10-05)

* Fixed the interactionsTimelapse function to properly create directories and accommodate duplicate file names

## Version 0.0.0.9 (2023-01-04)

* Made multiple fixes to cameraRename3 function for ease of use and greater ability to track progress
* Reorganized scripts to better keep track of functions
* Added save option to the dataOrganize function
* Clarified help text in movePictures functions
* Updated cameraDiagnostics to work when the UserLabel field does not exist
* Continued updating input names for consistency across functions
* Added the unsortImages function and associated documentation

## Version 0.0.0.10 (2023-01-04)

* Added the mergeFiles function and associated documentation
* Script updates to make it easier for me to find which file a function is in

## Version 0.0.0.11 (2023-01-09)

* Added duplicate removal from the unsortImages function so the unsorted version of the files will all be unique
* Fixed input name problem with APFun_env function

## Version 0.0.0.12 (2023-01-11, 2023-01-13, 2023-01-17)

* Added additional camera models for the cameraRename3 function
* Updated the interactionsTimelapse function to allow users to specify an out.dir. Input names were also updated for consistency across functions
* Made multiple fixes to dataOrganize to also search for video files and accommodate image names that do not include a serial number and added option to output serial number in the output.
* Added ability to use movePictures from a DataOrganize file. 
* Fixed problem in cameraRename3 where it didn't output exif data properly. 

## Version 0.0.0.13 (2023-01-20)

* Added the summarizeEvents function to better handle converting raw events into data usable in regression models. 
* Fixed APFun_env to be in line with the changes to the dataOrganize function. 
* Deprecated the occFun function in favor of summarizeEvents. 
* Updated documentation in the actFun function to properly link to the activity package.

## Version 0.0.0.14 (2023-01-23)

* Fixed documentation of cameraRename2 to properly deprecate the function in favor of cameraRename3
* Changed function names for APFun_env (calculateEvents), APFun_timelapse (doTimelapse), and dataOrganize (doFolder) to better represent what those functions do.
* Fixed and updated documentation for several functions related to changed function names and clarified documentation and function relatedness.
* Changed argument names on ctDates function for consistency across functions.
* Added a check for an inability to return a data.frame when outputting cameraRename3 so that the function will always output at least a list object.
* Added disclaimer section to all functions that assume my standard workflow for image processing. 

## Version 0.0.0.15 (2023-01-24)

* Updated the imageEffort function to have input names more in line with other functions in this package and to increase efficiency. 
* Updated the trapEffort function to have input names more in line with other functions in this package and to increase speed, versatility, and efficiency. Note that total trap nights is being calculated differently and is likely to output 1 extra trap night compared to previous versions of this function. This is due to the camtrapR::cameraOperation function calculating a 0.5 night on the first and last days of setup. 
* Updated the documentation of the movePictures function to properly link to new function names.
* Backend file name change for the quality control script from QualityControl to qualitycontrol to be more in line with other scripts in this package. 
* Backend file name change for the Interactions script from Interactions to interactions to be more in line with other scripts in this package. 

## Version 0.0.0.16 (2023-01-25, 2023-01-27)

* Allowed input of calculateEvents for interval to be a character-based time object (i.e., "30 min", "1 hour", etc.). 
* Updated documentation for some functions. 
* Fixed a bug in doFolder where it could not save the output. 
* Updated doTimelapse to have the same options for outputs as doFolder. 
* Updated movePictures documentation to better describe how a dataorganize file must be formatted for the function to work properly. 
* Cleaned up the code of calculateEvents and made it check for column names in the dataorganize file to make it function better. 

## Version 0.0.0.17 (2023-02-06, 2023-02-09, 2023-02-13)

* Fixed bug in doTimelapse function to handle situations where a file extension specified does not exist in the data.
* Fixed bug in summarizeEvents function where it could not find the species folder.
* Added the subsetImages function to subset sets of images from a raw or sorted camera folder. 
* Fixed bug in summarizeEvents where function would not be able to run if an item in the list of included species does not exist.

## Version 0.0.0.18 (2023-03-08, 2023-03-22)

* Updated documentation of interactions to properly cite and link to associated functions.
* Fixed bug in findCorruptImages function to properly call the correct object when running parallel processing.

## Version 0.0.0.19 (2023-04-17)

* Fixed a bug in summarizeEvents where total camera trap nights calculates as number of seconds, not number of days
* Fixed consistency bug in actFun to properly use "species" instead of "Species"

## Version 0.0.0.20 (2023-03-22, 2023-05-09)

* Added output message related to parallel processing for findCorruptImages function.
* Fixed the actFun function to call the correct spelling of "species" from calculateEvents.
* Added functionality in actFun to accommodate multiple grouping factors and output with species as the top level of the output list.
* Added the actPlot function to modify the output of the actFun function to make it easier to plot using ggplot2 or another plotting package.

## Version 0.0.0.22 (2023-06-01)

* Added progress line to rename section of cameraRename3 function.

## Version 0.0.0.23 (2023-06-23)

* Fixed problem with package install where the openxlsx package is set to required instead of suggested.

## Version 0.0.0.24 (2023-06-26)

* Fixed a bug in cameraRename3 where the function would fail if exiftool was not installed. 

## Version 0.0.0.25 (2023-06-30)

* Fixed a bug in imageEffort where the total capture rate was not being calculated correctly.
* Fixed a bug in timelapseQC where the function would not check if there is a missing species in SpeciesOther when there is a missing species in Species1.
* Fixed a bug in unsortImages where date.col was improperly specified and the function could not rename anything.

## Version 0.0.0.26 (2023-07-06)

* Forced use of copy instead of move when duplicates exist in movePictures.
* Updated doTimelapse and doFolder to more effectively find files and remove file extensions. doTimelapse no longer requires specification of file extensions. 
* Modified the cameraDiagnostics function to use dplyr::reframe() instead of dplyr::summarize() because cameras sometimes produce both photos and videos which prevents a one to one grouping of data. 
* Added functionality to cameraDiagnostics to add the camera name defined from the folder structure for comparisons of the camera label to actual camera name. 
* Modified actFun to allow for parallel processing of animal diel activity. 
* Fixed a bug in actPlot where the function would not work if no grouping variable was given in actFun. 

## Version 0.0.0.27 (2023-07-24)

* Fixed bug where namespace wasn't loading properly
* Major update to the cameraRename3 function including: 
  * Increased speed by utilizing more functions from the fs package
  * A bug fix preventing renaming from occurring
  * Added functionality to rename multiple file types at once
  * Other performance improvements and code simplification. 

## Version 0.0.0.28 (2023-08-07)

* Fixed bug in cameraDiagnostics to ensure that corrupt images which produce NA for their date time are skipped when computing first and last pics. 

## Version 0.0.0.29 (2023-08-08)

* Fixed bug in doFolder where diagnostics portion of function was attempting to use an object that no longer exists. 
* Updated the subsetImages function to run faster and have better functionality. 

## Version 0.0.0.30 (2023-08-10)

* Updated subsetImages to allow use of list.files or fs::dir_ls in case there are errors running fs::dir_ls. 

## Version 0.0.0.31 (2023-08-11)

* Fixed bug in movePictures where it may include timelapse rows that are NA when a combined timelapse file is provided. 

## Version 0.0.0.32 (2023-08-17)

* Fixed bug in cameraRename3 that properly requires the new ext input instead of file.type. 

## Version 0.0.0.33 (2023-09-04)

* Fixed bug in cameraRename3 that handles an issue with the Tag expansion for do.call. 

## Version 0.0.0.34 (2023-10-11)

* Fixed a bug in doFolder where files wouldn't load properly. 
* Altered the code to locate images using fs::dir_ls() to improve stability and speed. 
* Updated cameraRename3 to use this new method for dir_ls() to hopefully increase speed and stability. 

## Version 0.0.0.35 (2023-10-23)

* Fixed a bug in actPlot where the function failed to run when the input was not an "actmod" class object. 

## Version 0.0.0.36 (2023-10-27)

* Fixed a bug in actFun where the function would fail if some levels of the grouping factor did not exist for all species. 
* Added print output for the start and end times of the function. This is especially useful when running the function using parallel processing. 

## Version 0.0.0.37 (2023-11-01)

* Updated movePictures to run faster when using a dataorganize file. 
* Updated movePictures to determine if files exist in the in and out directories and adjust the transfer appropriately. 
* Fixed a bug in cameraRename3 where the function fails when you do not include a "." in the file extensions. 

## Version 0.0.0.38 (2023-11-03)

* Added the copyFiles function to create backups of camera data. 

## Version 0.0.0.39 (2023-11-21)

* Fixed a bug in movePictures where it couldn't create directories from a Timelapse file input. 

## Version 0.0.0.40 (2023-11-29)

* Fixed a bug in copyFiles where directories couldn't be created properly. 

## Version 0.0.0.41 (2023-12-05)

* Updated the summarizeEvents function to include an option to calculate number of detections/abundance per day instead of just per unit (i.e., month). This may be more beneficial than the original perUnit calculation for standardization purposes when there is missing data due to camera trap failures. 
* Bug fixes and updates to the documentation for summarizeEvents associated new functionality and discovered errors in the text. 
* Added the tidyr package to requirements due to its use in summarizeEvents to convert data from long to wide formats. 

## Version 0.0.0.42 (2024-01-02)

* Fixed a bug in copyFiles where it called the incorrect object when conducting a file transfer. 
* Added functionality for parallel processing in the copyFiles functions. 
* Updated the documentation for actFun to use accurate language when discussing the parallel processing functionality. Previously, the language was identical to that of cameraRename3 which was inaccurate for this task. 

## Version 0.0.0.43 (2024-03-28, 2024-04-01)

* Fixed a bug and updated documentation for subsetImages to require a value. NA or NULL will not work. This will be fixed a future version of this function. 
* Fixed a bug in subsetImages where the sorted option didn't work. 

## Version 0.0.0.44 (2024-04-02)

* Major update to the subsetImages function. The function has been nearly completely rewritten to hopefully make loading of files easier with fewer crashes. 
* Support has been added in the subsetImages function to allow for copying of interactions files. 

## Version 0.0.0.45 (2024-04-05)

* Updated summarizeEvents to be able to better handle differences in site names between the ct table and data as well as handle multiple sessions and independent cameras at sites. 

## Version 0.0.0.46 (2024-05-10)

* Updated interactionsTimelapse to better handle duplicate files. 

## Version 0.0.0.47 (2024-07-02)

* Added a new version of the cameraRename function to handle renaming of the date collected directory as well. 

## Version 0.0.0.48 (2024-07-03)

* Updated the cameraRename4 function to better handle massive camera directories. 
* Fixed some bugs in cameraRename4 to handle directories with only one camera folder. 
* Added the deleteEmptyDirs function to help find and delete empty directories after running cameraRename4 with the date collected folders specified. 

## Version 0.0.0.49 (2024-07-18)

* Bug fix on cameraRename4 so that it can properly rename files. The file paths were mis-specified in the rename section.


