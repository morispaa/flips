\name{flips.copy}
\alias{flips.copy}

\title{Copy FLIPS environment and files}
\description{Copies an existing FLIPS environment and files into a new FLIPS environment}

\usage{e.new <- flips.copy(from,new.fileID,overwrite=FALSE)}
\arguments{
	\item{from}{Existing FLIPS environment to be copied}
	\item{new.fileID}{Integer. The file ID for the new FLIPS environment \code{e.new}}
	\item{overwrite}{Flag for checking if files with file ID \code{new.fileID} already exist. Default is \code{FALSE}, i.e. if the file ID is already in use, error is displayed.}
	}

\value{
	FLIPS environment \code{e.new}. It is a copy of old environment \code{from} with a new file ID \code{new.fileID}. All internal variables and existing files are copied. 
}

%\details{}

%\note{}

\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

%\seealso{}

%\references{}


%\examples{}


\keyword{algebra}