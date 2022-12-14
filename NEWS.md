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
