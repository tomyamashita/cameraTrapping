% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QualityControl.R
\name{copyFiles}
\alias{copyFiles}
\title{Copy files from one location to another}
\usage{
copyFiles(in.dir, out.dir, create.dirs, type, pp = FALSE, cores.left = NULL)
}
\arguments{
\item{in.dir}{String. The directory containing the files you want to transfer. This can be anything but all folders and files below the level specified will be copied}

\item{out.dir}{String. The directory where you want to copy the files to.}

\item{create.dirs}{Logical. Should the function create the directories it needs?}

\item{type}{String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none').}

\item{pp}{Logical. Should this function take advantage of parallel processing.
The default is FALSE. Because the function separates tasks by camera directory, it can use parallel processing to run multiple cameras at the same time.
This is currently set up to run in Windows OS so I do not know if this will work on a Mac or Linux system.
If you want this functionality on a Unix device and know how to set it up, let me know and I can incorporate it.}

\item{cores.left}{Numeric. How many cores should be left available when using parallel processing?
The default is NULL. This is only necessary when pp=TRUE.
If left at the default, the function will default to 2 cores remaining which is generally enough to continue using a PC while the function runs.
I would set this to be greater than 0, otherwise the function will use the entire processing power of your computer.
To see how many cores you have available for parallel processing, use: parallel::detectCores().}
}
\value{
data.frame containing the file paths of the input and output files.
}
\description{
This function copies or moves files from one location to another
}
\details{
This function can be used to copy or move files from one location to another.
It is useful for creating backups and duplicates of file structure and files on multiple drives or in multiple locations.
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
\seealso{
\code{\link{movePictuers}}
}
\concept{camera trapping}
\concept{data backup}
\keyword{files}
\keyword{manip}
