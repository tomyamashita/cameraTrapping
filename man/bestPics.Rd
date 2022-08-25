\name{bestPics}
\alias{bestPics}
\title{Extract Best Pictures from Timelapse}

\description{This function uses a timelapse csv to extract photos tagged as a best picture and copies them to a specified location.
}
\usage{bestPics(x, in.dir, out.dir, copy = T, sorted = F)}

\arguments{
  \item{x}{A data frame from a timelapse file}
  \item{in.dir}{String. The directory containing the pictures. This can either be an unsorted directory or a sorted directory. Keep in mind, these assume my very specific directory format.}
  \item{out.dir}{String. The directory where you want the best pictures to be stored. This can be anything.}
  \item{copy}{Logical. Should the images be copied into the new directory?}
  \item{sorted}{Logical. Is the in.dir an unsorted folder or a sorted folder?}
}

\details{%%  ~~ If necessary, more details than the description above ~~}

\value{A data frame containing the file path info of the best pics.}


\author{Thomas J. Yamashita}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ files }
\keyword{ manip }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ timelapse }
% \concept{ ~cpt2 }
% Use only one concept per line.
