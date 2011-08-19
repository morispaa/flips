\name{flips.resize}
\alias{flips.resize}

\title{Resize FLIPS problem}
\description{Marginalizes and/or adds unknowns to an existing FLIPS problem.}

\usage{flips.resize(e,remove=NULL,add=0,buffersize=100)}
\arguments{
	\item{e}{An existing FLIPS environment with some data added.}
	\item{remove}{An integer vector giving the indices of unknowns that are to be marginalized/deleted.}
	\item{add}{Integer giving the number of unknowns to be added to the problem.}
	\item{buffersize}{Rotation buffer size. See details.}
	}

\value{None. The FLIPS environment \code{e} will be updated.}

\details{Vector \code{remove} contains the indices of unknowns that are going to be marginalized. 

If \code{add > 0}, \code{add} new unknowns are added to the problem. They will be put in the end index-wise.

The number of unknowns in the problem after resizing will be \code{e$ncols - length(remove) + add}. 

The size of the rotation buffer can be controlled by \code{buffersize}. The default 100 should be good for most situations. For very large problems increasing \code{buffersize} may have positive effect on the performance.}



%\note{}

\author{Mikko Orispaa (university of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

%\seealso{}

%\references{}


%\examples{}



\keyword{algebra}
