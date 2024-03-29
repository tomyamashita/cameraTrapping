% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QualityControl.R
\name{timelapseQC}
\alias{timelapseQC}
\title{Quality control for timelapse}
\usage{
timelapseQC(timelapse, exclude = NULL, detailed_res = F)
}
\arguments{
\item{timelapse}{A data frame created from a csv file exported from Timelapse. Note, this function is set up to use my timelapse template.}

\item{exclude}{String. Which species should not be checked for number of individuals? The default is NULL which will only exclude c("ghost", "human", "", "rodent", "bird").}

\item{detailed_res}{Logical. Should a detailed output of errors be produced? If set to FALSE, only simple diagnostic information will be displayed (full information will be in the output file).}
}
\value{
LIST containing unique species and data frames of missing species files and missing individuals files.

Unique Species:
String. The list of the unique species identified from the file.

Missing Species:
Data frame. Any entries that were not sorted by species is included in this file.

Missing Ind:
Data frame. Any entries that should have been but were not sorted by individual are included here.
}
\description{
This function outputs the unique species from a timelapse file, a list of rows that were not sorted by species, and a list of rows that were not sorted by individual. You should use this to check whether a set of images was completely sorted.
}
\details{
This function will check if there are video files (.MP4) then check the number of unique species, whether any image was not sorted by species by checking if the species1 column contains any "" values, and whether any image was not sorted by individuals by checking if a column sorted into a species was not sorted by individual. Ghosts, humans, "", birds, rodents, lizards, and amphibians are not sorted by individual under my current protocol, and are therefore not included in this check. Video files are also not checked by individual.

Once the timelapse file has passed this check, it can be used in the standard workflow for images.
}
\section{{Standard Disclaimer}}{
 As with most of the functions in this package, using this function assumes that you have been following my normal workflow, including the particular formatting that these functions assume.
These functions are built for very specific purposes and may not generalize well to whatever you need it for.
I build them for my own use and make no promises that they will work for different data formatting situations.
As I come across errors, I attempt to further generalize the functions but this is done as I go.
}

\examples{
\dontrun{
# No example provided
}
}
\seealso{
\code{\link{doTimelapse}}
}
\concept{camera trapping}
\concept{timelapse}
\keyword{debugging}
\keyword{manip}
