\name{flips.dispose}
\alias{flips.dispose}

\title{Delete FLIPS environment}

\description{Deletes the FLIPS enviroment's internal variables and the files
used by the environment.}

\usage{flips.dispose(h)}

\arguments{
	\item{h}{FLIPS environment to be deleted}
}

\value{None. \code{h} will be an empty environment afterwards.}

\details{\code{flips.dispose} will delete all variables in FLIPS environment \code{h} and delete the data directory (and all files inside it) of \code{h}.}

\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

\keyword{algebra}
