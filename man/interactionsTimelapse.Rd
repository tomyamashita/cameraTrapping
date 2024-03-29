% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Interactions.R
\name{interactionsTimelapse}
\alias{interactionsTimelapse}
\title{Create an Interactions File from a Timelapse Output}
\usage{
interactionsTimelapse(
  timelapse,
  envdata,
  in.dir,
  out.dir = NULL,
  create.dirs = T,
  type = "copy",
  exclude = c("ghost")
)
}
\arguments{
\item{timelapse}{A timelapse csv, formatted using our Timelapse template.}

\item{envdata}{Environmental variables data frame. This file must have header called "Camera" containing the list of cameras, "Site" containing the site name, "Side" indicating which side the camera is on, and "Type" indicating what type of structure the camera is at.}

\item{in.dir}{String. The directory where the original image data is. This should be the same folder containing the images folder.}

\item{out.dir}{String. Defaults to NULL. The directory where you want to store the Interactions images. The default specifies the in.dir. Options include c("in.dir") to more explicitly refer to the in.dir.}

\item{create.dirs}{Logical. Defaults to TRUE. Should new directories be checked for and created by R if necessary?}

\item{type}{String. Defaults to "none". Should image files be copied or not to the appropriate directories?}

\item{exclude}{species to exclude from the output data frame. Use c() to specify species. If you want keep all items use c(""). Unlike \code{\link{calculateEvents}}, this has no default. Use it to exclude any "species" from the output file.}
}
\value{
A list containing the data file and a file of the old and new paths and names.

Interactions:
The Interactions data file.

Files:
File used for copying files.
}
\description{
This function copies images for interactions to a new folder and creates an interactions file.
}
\details{
NOTE: This function will add a (#) to duplicate file names, similar to the way Windows handles copying files with the same name into the same folder.  of handling duplicate file names so it will skip duplicates when copying.
}
\section{{Standard Disclaimer}}{
 As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
If you want to make these functions work, I would recommend either adjusting your formatting or using this function as a template to build your own.
These functions are built for very specific purposes and may not generalize well to whatever you need it for.
I build them for my own use and make no promises that they will work for different data formatting situations.
As I come across errors, I attempt to further generalize the functions but this is done as I go.
}

\examples{
\dontrun{
# No example provided
}
}
\references{
Tewes, M., J. Lombardi, Z. Wardle, and T. Yamashita. 2020. Ocelot and Jaguarundi Monitoring Project: Evaluating the Effectiveness of Wildlife Crossings, Cattle Guards, and Fencing on Road Facilities in Cameron County, Contract No. 57-9XXIA003, 0000018485. Feline Research Program, Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville.

Kline, R., K. Ryer, A. Rivera, T. Yamashita, and T. Hopkins. 2020. Post-construction Monitoring Bi-annual Report for SH 100: May 2019 thru October 2019 (Contract No 57-9XXIA001). The University of Texas Rio Grande Valley.
}
\seealso{
\code{\link{calculateEvents}}

\code{\link{interactionsDataOrganize}}
}
\concept{interactions}
\concept{timelapse}
\keyword{manip}
