\name{flips.put}
\alias{flips.put}
\alias{flips.put.Y}

\title{Insert target vector}

\description{Explicitely insert a target vector Y into an existing
FLIPS problem.}

\usage{flips.put.Y(h,data)}

\arguments{
	\item{h}{An existing and initialized FLIPS environment.}
	\item{data}{Numerical values of the target vector Y. See details.}
	}
	
\value{None. The \code{data} vector or matrix is written to a binary file as the 
the target vector Y.}

\details{The \code{data} can be given as a vector (in a row-major order) or as a matrix. 

In \code{flips.put.Y}, \code{data} can be given as a matrix of size \code{dim(data) = c(h$ncols,h$nrhs)}
or as a (row-major) vector of the size \code{length(data) = h$ncols*h$nrhs}.}

\note{Any existing data in target vector are overwritten.}


\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

\keyword{algebra}
