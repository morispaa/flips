## File: flips.demo.R

## Part of FLIPS

##    FLIPS - Fortran Linear Inverse Problem Solver
##    Copyright (C) 2005--2009 University of Oulu
##    Written by Mikko Orispaa <mikko.orispaa@oulu.fi>
##
##    Licensed under BSD License.


# Construct a function for Hamming window
hamming <- function(L)
{
	hamming <- rep(0,L)
	for (n in 0:(L-1))
	{
		hamming[n+1] <- 0.54 - 0.46 * cos(2*pi*n/(L-1))
	}
	return(hamming)
}

flips.demo <- function()
{

# Load rflips
require(rflips)

# Set up graphics
def.par <- par(no.readonly=TRUE)
layout(matrix(1:6,3,2,byrow=TRUE))

# Construct solution, theory matrix and measurement

# Solution
x<-seq(0,1,len=500)
sol<-abs(100*x*(x-0.2)*(x-0.6)*(x-1))
# Plot solution
plot(x,sol,type="l",main="Solution")

# Construct measurement by convolving solution with Hamming window of length 100.
ham100 <- hamming(100)
meas <- convolve(sol,ham100,type='open')

# Plot measurement
plot(meas,type="l",main="Measurement (no error)")

# Add error to the measurement
noisy.meas <- 0.1 * mean(meas) * rnorm(length(meas)) + meas

# Plot noisy data
plot(noisy.meas,type="l",main="Noisy measurement ")

# Theory matrix
A <- matrix(0,length(meas),500)
for ( i in 1:500)
{
	A[i:(i+99),i] <- ham100
}

# Solve ("dummy boy")
h <- flips.init(500,1,'d')
flips.add(h,A,noisy.meas)
flips.solve(h)
fsol1 <- h$solution
plot(x,h$solution,type="l",main="Dummy boy solution (no regularization)")

# Add regularization
reg.mat <- matrix(0,500,500)
reg.mat[1,1:2] <- c(2,-1)
reg.mat[500,499:500] <- c(-1,2)
for (i in 2:498)
{
	reg.mat[i,(i-1):(i+1)] <- c(-1,2,-1)
}
reg.meas <- rep(0,500)
reg.err <- 0.000001

flips.add(h,reg.mat,reg.meas,reg.err)
flips.solve(h)
fsol2 <- h$solution


# Plot regularized solution
plot(x,fsol2,type="l",main="Regularized solution and the real solution")
lines(x,sol,type="l",col="red")

# Initialize new problem, add the same data 
# and regulation and marginalize away
# some of the unknowns
h2 <- flips.init(500,1,'d')
flips.add(h2,A,noisy.meas)
flips.add(h2,reg.mat,reg.meas,reg.err)

# Marginalize fisrt and last 100 unknowns
remove.vec <- c(1:100,401:500)
flips.resize(h2,remove=remove.vec,add=0)

# Solve the resized problem
flips.solve(h2,'rfc')
fsol3 <- h2$solution

# Plot the original and the resized solutions
plot(x,fsol2,type="l",main="Original and resized solutions")
lines(x[101:400],fsol3,type="b",col="red")

# Delete FLIPS files
flips.dispose(h)
flips.dispose(h2)

par(def.par)
}