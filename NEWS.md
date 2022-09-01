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

## Versopm 0.0.0.4 (2022-08-31)

* Added an additional error code to the APFun_env function to ensure that camera names are included in the envdata file
* Modified several functions so that the input requirements are more consistent. "timelapse" is used any time a timelapse file is required as the input. "x" is used for other inputs related to camera data. "ds" is used for other inputs not related to camera data.

