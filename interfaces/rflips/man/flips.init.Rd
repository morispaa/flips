\name{flips.init}
\alias{flips.init}

\title{Initialize FLIPS environment}

\description{
	Initializes a new FLIPS environment.}
	
\usage{
	h <- flips.init(ncols, nrhs,type='d',ID=tempfile(pattern='',tmpdir=''))
	}
	
\arguments{
	
	\item{ncols}{Integer giving the number of unknowns, i.e. the number of
	columns in the theory matrix.}
	
	\item{nrhs}{Integer giving the number of columns in the measurement matrix.}
	
	\item{type}{Problem numerical type. One of the following character strings:
		\describe{
			\item{'s'}{Single precision real}
			\item{'d'}{Double precision real}
			\item{'c'}{Single precision complex}
			\item{'z'}{Double precision complex}
		}}
	
	\item{ID}{Character string giving unique label to the directory created by FLIPS. If omitted, a random character string will be used.}
	}
	


\value{
R environment. This environment contains number of internal variables needed by
\code{rflips}. After the FLIPS problem is solved (see \code{\link{flips.solve}})
it also contains the solution, residual and posteriori covariance matrix.
}



\seealso{\code{\link{flips.dispose}}, \code{\link{flips.add}}, \code{\link{flips.solve}}.}

\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

\keyword{algebra}

