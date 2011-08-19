\name{flips.get}
\alias{flips.get}
\alias{flips.get.R}
\alias{flips.get.Y}

\title{Fetch target matrix or target vector}
\description{Get the current target matrix R or the target vector Y of the FLIPS problem.}

\usage{R <- flips.get.R(h)
Y <- flips.get.Y(h)}

\arguments{
	\item{h}{An existing and initialized FLIPS environment.}
	}

\value{\code{flips.get.R} returns an upper triagonal matrix of size \code{h$ncols} x \code{h$ncols}

\code{flips.get.Y} returns a matrix of size \code{h$ncols} x \code{h$nrhs}.}

\note{The target matrix and target vector must be written into a file before using there commands. Use \code{flips.rotate} to force the rotations to ensure that all data has been fed into FLIPS.}


\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

\keyword{algebra}