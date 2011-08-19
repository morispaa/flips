\name{flips.add}
\alias{flips.add}

\title{Add data to FLIPS problem}

\description{
Add data (theory matrix, measurements and errors) into the FLIPS system
row(s)-by-row(s).}

\usage{
flips.add(e,A.data,M.data,E.data=1)
}

\arguments{
\item{e}{Initialized FLIPS environment}
\item{A.data}{Matrix or vector containing the theory matrix rows.}
\item{M.data}{Matrix or vector containing the measurement matrix rows.}
\item{E.data}{Vector or scalar containing the measurement errors (variances), or the full error covariance matrix. See details.}
}

\value{None.}

\details{
Theory matrix rows (\code{A.data}) can be given as a vector (row-wise) or as a matrix. 

If given as a vector, the size
of the vector must be \code{num.rows*ncols}, where \code{ncols} is the number
of unknowns in the problem (i.e. the number of columns in the theory matrix) and 
\code{num.rows} is the number of rows in the theory matrix \code{A.data}.

If given as a matrix, its dimensions must be \code{c(num.rows,ncols)}.

In the same way, the measurements can be given as a vector or a matrix. If given as 
a vector, its size nust be \code{num.rows*nrhs}, where \code{nrhs} is the number of
columns in the measurement vector. If it is given as a matrix its size must be 
\code{c(num.rows,nrhs)}.

The error variances can be given as a vector (size \code{num.rows}) or if the error 
variance is constant for all measurements added into FLIPS, it can also be given as a 
single scalar value. If error is omitted scalar value 1.0 will be used.

If the full error covariance matrix is given, it must be a symmetric positive-definite 
square matrix with size \code{num.rows}-by-\code{num.rows}. 
}



\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

\examples{

}

\keyword{algebra}
