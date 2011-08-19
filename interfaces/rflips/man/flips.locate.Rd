\name{flips.locate}
\alias{flips.locate}

\title{Set path to Flipseng}
\description{Sets the \code{FLIPS$path} variable to point to the FLIPS driver (Flipseng27)}

\usage{flips.locate(path)}
\arguments{
	\item{path}{Path to \code{flipseng27}}
	}

\value{
	None.
}

\details{The default for \code{FLIPS$path} is just \code{flipseng27}, i.e. rflips assumes that the FLIPS
driver is somewhere in the PATH. With this command this behavior can be changed. It is usually necessary only when
debugging and/or testing.
}

%\note{}

\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

%\seealso{}

%\references{}


%\examples{}


\keyword{algebra}