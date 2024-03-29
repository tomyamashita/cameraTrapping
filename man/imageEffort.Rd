% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QualityControl.R
\name{imageEffort}
\alias{imageEffort}
\title{Camera trapping image effort}
\usage{
imageEffort(timelapse = NULL, do = NULL)
}
\arguments{
\item{timelapse}{List. A list object containing a list of timelapse files. If you name your files, the names will be outputted in the result.}

\item{do}{List. A list object containing a list of DataOrganize files. If you name your files, the names will be outputted in the result.}
}
\value{
A data frame containing total number of pictures, number of pictures of animals, ghosts, and humans, and the success rate for animal pictures in each file added as well as a row for the total number of pictures.
}
\description{
This function calculates the total number of pictures, number of animals, ghosts, and humans from one or more timelapse or dataorganize files.
}
\details{
If a timelapse file is given to the function, it will run an internal version of the \code{\link{doTimelapse}} function to convert it to a DataOrganize file.

This function can accept either a timelapse file or a DataOrganize file, not both. Be sure to only specify one.
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
\code{\link{doFolder}}, \code{\link{doTimelapse}}
}
\concept{camera trapping}
\keyword{manip}
