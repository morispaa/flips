\name{flips.rotate}
\alias{flips.rotate}

\title{Force Givens rotation}
\description{Rotates all unrotated data of a FLIPS environment}

\usage{flips.rotate(e,buffersize=100)}
\arguments{
	\item{e}{Existing FLIPS environment with unrotated data}
	\item{buffersize}{Rotation buffer size. See details.}
	}

\value{
	None. The unrotated data is rotated into FLIPS system.
}

\details{\code{flips.rotate} forces the Givens rotations on unrotated FLIPS data. Especially for large problems this can reduce the memory consumption and solution time remarkably. On the other hand, overusing this command will decrease the performance.

The correct interval to use this command depends on used platform (CPU, memory, disk speed, etc). A general rule-of-thumb would be using this after every couple of hundred added data rows for large problems (with several thousand unknowns).

The size of the rotation buffer can be controlled by \code{buffersize}. The default 100 should be good for most situations. For very large problems increasing \code{buffersize} may have positive effect on the performance.
}

%\note{}

\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

%\seealso{}

%\references{}


%\examples{}


\keyword{algebra}