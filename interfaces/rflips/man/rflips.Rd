\name{RFLIPS}
\alias{rflips}
\alias{Rflips}
\alias{RFlips}
\alias{FLIPS}
\alias{flips}
\alias{Flips}

\title{RFLIPS}

\description{R interface for Fortran Linear Inverse Problem Solver}

\details{RFLIPS is a linear solver for large (overdetermined) linear systems.

\bold{RFLIPS functions:}

\code{\link{flips.init}} FLIPS initialization

\code{\link{flips.dispose}} FLIPS object deletion

\code{\link{flips.add}} Data feeding

\code{\link{flips.solve}} Problem solving
 
\code{\link{flips.copy}} Prbolem cloning

\code{\link{flips.resize}} Unknown marginalization/addition

\code{\link{flips.rotate}} Force Givens rotations on added data

\code{\link{flips.put}} Insert target matrix or target vector into a FLIPS problem manually

\code{\link{flips.get}} Fetch the target matrix or target vector manually

\code{\link{flips.demo}} Simple FLIPS demonstration

}

\note{Copyright (c) 2006--2007, University of Oulu, Finland.

FLIPS and RFLIPS are licensed under GNU GPL version 2. }

\author{Mikko Orispaa (University of Oulu, Finland) \email{mikko.orispaa@oulu.fi}}

%\seealso{}

%\references{}


%\examples{}


\keyword{algebra}
