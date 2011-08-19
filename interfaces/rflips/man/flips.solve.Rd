\name{flips.solve}
\alias{flips.solve}

\title{Solve FLIPS problem}
\description{Solve the problem fed into FLIPS environment.}

\usage{flips.solve(e,mode='',buffersize=100)}
\arguments{
	\item{e}{An existing FLIPS environment with enough data feeded in.}
	\item{mode}{Solving mode. See details.}
	\item{buffersize}{Rotation buffer size. See details.}
	}

\value{None. The solution is written into FLIPS environment variable \code{e$solution}. The residual and the posteriori covariance matrix are written into environment variables \code{e$residual} and \code{e$covariance}, respectively (if calculated at all, depending on the flags given).}

\details{Solving mode must be one of the following character strings:
    \describe{
	\item{''}{Calculate only the solution}
	\item{'r'}{Calculate the solution and the residual}
	\item{'rc'}{Calculate the solution, the residual and the diagonal of the posteriori covariance matrix}
	\item{'rfc'}{Calculate the solution, the residual and the full posteriori covariance matrix}
	\item{'c'}{Calculate the solution and the diagonal of the posteriori covariance matrix}
	\item{'fc'}{Calculate the solution and the full posteriori covariance matrix}
    }

The size of the rotation buffer can be controlled by \code{buffersize}. The default 100 should be good for most situations. For very large problems increasing \code{buffersize} may have positive effect on the performance.

}

\note{RFLIPS does not check that there exists enough data in the problem, nor that the problem is solvable (i.e. there exists enough linearly independent data rows in the theory matrix).}

\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

%\seealso{}

%\references{}


%\examples{}


\keyword{algebra}
