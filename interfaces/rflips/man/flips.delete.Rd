\name{flips.delete}
\alias{flips.delete}

\title{Delete data from FLIPS problem}

\description{
Deletes data (theory matrix, measurements and errors) from the FLIPS system
row(s)-by-row(s).}

\usage{
flips.delete(e,A.data,M.data,E.data=1,buffersize=100)
}

\arguments{
\item{e}{Initialized FLIPS environment}
\item{A.data}{Matrix or vector containing the theory matrix rows.}
\item{M.data}{Matrix or vector containing the measurement matrix rows.}
\item{E.data}{Vector or scalar containing the measurement errors (standard deviations), or the full error covariance matrix. See details.}
\item{buffersize}{Rotation buffer size. See details.}
}

\value{None.}

\details{
NB: This is very experimental feature! Please read the issues and limitations below!

Theory matrix rows (\code{A.data}) can be given as a vector (row-wise) or as a matrix. 

If given as a vector, the size
of the vector must be \code{num.rows*ncols}, where \code{ncols} is the number
of unknowns in the problem (i.e. the number of columns in the theory matrix) and \code{num.rows} is the number of rows in the theory matrix \code{A.data}.

If given as a matrix, its dimensions must be \code{c(num.rows,ncols)}.

In the same way, the measurements can be given as a vector or a matrix. If given as a vector, its size nust be \code{num.rows*nrhs}, where \code{nrhs} is the number of
columns in the measurement vector. If it is given as a matrix its size must be 
\code{c(num.rows,nrhs)}.

The errors can be given as a vector (size \code{num.rows}) or if the error is
constant for all data rows added into FLIPS, it can also be given as a scalar. If error is omitted scalar value 1 will be used.

Only standard deviations are supported at this time, i. e., full covariance matrices can not be used!

\code{flips.delete} is available only for real valued problems. Complex problems are not currently supported.

It is safe to delete only the exactly same data rows that have been previously entered into FLIPS. Trying to delete arbitrary data probably (but nor necessarily) causes FLIPS to crash. There is no possibility to check this beforehand at this moment.

The FLIPS problem must be overdetermined (i. e., it must contain at least as many data rows that there are unknowns)  before data rows are going to be deleted, and it must remain overdetermined (or square) after the data rows have been deleted! Otherwise, crash will (probably) occur. 

The size of the rotation buffer can be controlled by \code{buffersize}. The default 100 should be good for most situations. For very large problems increasing \code{buffersize} may have positive effect on the performance. 

}



\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

\examples{
\dontrun{
## Let h be a FLIPS problem with 150 unknowns

## Marginalize first 25 unknowns
flips.resize(h,remove=seq(1,25))
## Now h will have 125 unknowns

## Marginalize last 25 unknowns (indices 101..125) and
## add 50 new unknowns
flips.resize(h,remove=seq(101,125),add=50)
## Now h will have 150 unknowns and the new ones will be
## indices 101..150

#########

## To do the above two commands at once
flips.resize(h,remove=c(1:25,126:150),add=50)


}
}

\keyword{algebra}
