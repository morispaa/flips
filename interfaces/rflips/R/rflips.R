## File: rflips.R

## Part of FLIPS

##    FLIPS - Fortran Linear Inverse Problem Solver
##    Copyright (C) 2005--2009 University of Oulu
##    Written by Mikko Orispaa <mikko.orispaa@oulu.fi>
##
##    Licensed under BSD License. See file LICENSE for details.



FLIPS <- new.env()

FLIPS$path <-"flipseng32"


flips.locate <- function(path)
  {
    FLIPS$path <- path
  }

## Problem initialization ##############################

flips.init <- function(ncols,nrhs,type='d',common=0,band=1,ID=tempfile(pattern=format(Sys.time(),"%OS6"),tmpdir=''))
  {
    e <- new.env()
    
    DirName <- ID

	# Trim automagically created filename
	
	# OSX and *NIX
	if (substr(DirName,1,1)=="/")
	{
		DirName <- substr(DirName,2,nchar(DirName))
	}
	
	# Windows
	if (substr(DirName,1,1)=="\\")
	{
		DirName <- substr(DirName,3,nchar(DirName))
	}
	
	dn <- paste("FLIPS_",DirName,.Platform$file.sep,sep="")
	
	
	 
    e$ID <- DirName
    e$type <- type
    
    if (type=='d')
    {
    	e$real <- TRUE
    	e$single <- FALSE
    }
    else if (type=='s')
    {
    	e$real <- TRUE
    	e$single <- TRUE
    }
	else if (type=='c')
	{
    	e$real <- FALSE
    	e$single <- TRUE
    }
	else if (type=='z')
    {
    	e$real <- FALSE
    	e$single <- FALSE
    }
	else
	{
		stop('Unknown type!')
	}
    
    e$band <- band
    	
    e$ncols <- ncols
    e$nrhs <- nrhs
    e$common <- common
    e$append <- FALSE

    e$crows <- 0
    e$newrows <- 0

    e$r.written <- FALSE
    e$sol.written <- FALSE
    e$rinv.written <- FALSE
    e$residual.written <- FALSE
    e$covariance.written <- FALSE
    e$covariance.full <- FALSE


    
    e$R.file <- paste(dn,"R.dat",sep="")
    e$Y.file <- paste(dn,"Y.dat",sep="")
    e$S.file <- paste(dn,"sol.dat",sep="")
    e$resid.file <- paste(dn,"resid.dat",sep="")
    e$cov.file <- paste(dn,"cov.dat",sep="")
    e$Rinv.file <- paste(dn,"Rinv.dat",sep="")
    e$A.file <- paste(dn,"A.dat",sep="")
    e$M.file <- paste(dn,"M.dat",sep="")
    e$E.file <- paste(dn,"E.dat",sep="")
    e$rmask.file <- paste(dn,"rmask.dat",sep="")
    e$tres.file <- paste(dn,"tres.dat",sep="")
    e$bw.file <- paste(dn,"cbw.dat",sep="")
    e$cv.file <- paste(dn,"cv.dat",sep="")
   
    ## Create directory
    ## print(DirName)
    dir.create(dn)
    
    e$n.addvar <- 0
    e$n.delvar <-0
     

    e$solution <- FALSE
    e$residual <- FALSE
    e$covariance <- FALSE

	if (e$real)
	{
		if (e$single)
		{
			e$read.file <- read.bin.file.single
			e$append.file <- append.bin.file.single
			e$write.file <- write.bin.file.single
			e$write.err <- write.bin.file.single
			e$append.err <- append.bin.file.single
			e$read.resid <- read.bin.file.single
		}
		else
		{
			e$read.file <- read.bin.file
			e$append.file <- append.bin.file
			e$write.file <- write.bin.file
			e$write.err <- write.bin.file
			e$append.err <- append.bin.file
			e$read.resid <- read.bin.file
		}
	}
	else
	{
		if (e$single)
		{
			e$read.file <- read.bin.file.scplx
			e$append.file <- append.bin.file.scplx
			e$write.file <- write.bin.file.scplx
			e$write.err <- write.bin.file.single
			e$append.err <- append.bin.file.single
			e$read.resid <- read.bin.file.single
		}
		else
		{
			e$read.file <- read.bin.file.dcplx
			e$append.file <- append.bin.file.dcplx
			e$write.file <- write.bin.file.dcplx
			e$write.err <- write.bin.file
			e$append.err <- append.bin.file
			e$read.resid <- read.bin.file
		}
	}


    return(e)
    
  }

########################################################


## Problem deletion ####################################

flips.dispose <- function(h)
  {
    # Delete files
    unlink(paste('FLIPS_',h$ID,sep=""),recursive=TRUE)
     #if (file.exists(h$R.file)) file.remove(h$R.file)
     #if (file.exists(h$Y.file)) file.remove(h$Y.file)
     #if (file.exists(h$S.file)) file.remove(h$S.file)
     #if (file.exists(h$resid.file)) file.remove(h$resid.file)
     #if (file.exists(h$cov.file)) file.remove(h$cov.file)
     #if (file.exists(h$Rinv.file)) file.remove(h$Rinv.file)
     #if (file.exists(h$A.file)) file.remove(h$A.file)
     #if (file.exists(h$M.file)) file.remove(h$M.file)
     #if (file.exists(h$E.file)) file.remove(h$E.file)
     #if (file.exists(h$rmask.file)) file.remove(h$rmask.file)
     #if (file.exists(h$tres.file)) file.remove(h$tres.file)    
	if (file.exists("rflips.nml")) file.remove("rflips.nml")

     ## Empty environment
     rm(list=ls(env=h),envir=h)
     ##rm(h,envir=parent.env(h))
  }



## Binary file I/O routines ############################

## Read bw ##

read.bw <- function(h)
{
	fid <- file(h$bw.file,"rb")
	bw <- readBin(fid,integer(),2,size=4)	
	close(fid)
	return(bw)
	
}

## Append ##

append.bin.file.single <- function(fname,data)
  {
    fid <- file(fname,"ab")

    writeBin(as.vector(as.single(data)),fid,size=4)

    close(fid)

  }
  
append.bin.file <- function(fname,data)
  {
    fid <- file(fname,"ab")

    writeBin(as.vector(as.double(data)),fid,size=8)

    close(fid)

  }

append.bin.file.scplx <- function(fname,data)
  {
    fid <- file(fname,"ab")

    w.data<-vector(len=length(data)*2)
    w.data[seq(1,2*length(data),by=2)] <- Re(data)
    w.data[seq(2,2*length(data),by=2)] <- Im(data)
    writeBin(as.vector(as.single(w.data)),fid,size=4)

    close(fid)

  }
  
append.bin.file.dcplx <- function(fname,data)
  {
    fid <- file(fname,"ab")

    w.data<-vector(len=length(data)*2)
    w.data[seq(1,2*length(data),by=2)] <- Re(data)
    w.data[seq(2,2*length(data),by=2)] <- Im(data)
    writeBin(as.vector(as.double(w.data)),fid,size=8)

    close(fid)

  }

## Read ##

read.bin.file.single <- function(fname,n.elem)
  {
    fid <- file(fname,"rb")

    zz <- readBin(fid,single(),n.elem,size=4)

    close(fid)

    return(as.vector(zz))
  }
  
read.bin.file <- function(fname,n.elem)
  {
    fid <- file(fname,"rb")

    zz <- readBin(fid,double(),n.elem,size=8)

    close(fid)

    return(as.vector(zz))
  }
  
read.bin.file.scplx <- function(fname,n.elem)
  {
    fid <- file(fname,"rb")

    n.elem <- n.elem*2
    
    zz <- readBin(fid,single(),n.elem,size=4)

    close(fid)

    cc <- zz[seq(1,n.elem,by=2)] + 1i*zz[seq(2,n.elem,by=2)]
    
    return(as.vector(cc))
  }
  
read.bin.file.dcplx <- function(fname,n.elem)
  {
    fid <- file(fname,"rb")

    n.elem <- n.elem*2
    
    zz <- readBin(fid,double(),n.elem,size=8)

    close(fid)

    cc <- zz[seq(1,n.elem,by=2)] + 1i*zz[seq(2,n.elem,by=2)]
    
    return(as.vector(cc))
  }


## Write ##


write.bin.file.single <- function(fname,data)
  {
    fid <- file(fname,"wb")

    writeBin(as.vector(as.single(data)),fid,size=4)

    close(fid)
  }

write.bin.file <- function(fname,data)
  {
    fid <- file(fname,"wb")

    writeBin(as.vector(as.double(data)),fid,size=8)

    close(fid)
  }



write.bin.file.scplx <- function(fname,data)
  {
    fid <- file(fname,"wb")

    w.data<-vector(len=length(data)*2)
    w.data[seq(1,2*length(data),by=2)] <- Re(data)
    w.data[seq(2,2*length(data),by=2)] <- Im(data)
    writeBin(as.vector(as.single(w.data)),fid,size=4)

    close(fid)
  }
  
write.bin.file.dcplx <- function(fname,data)
  {
    fid <- file(fname,"wb")

    w.data<-vector(len=length(data)*2)
    w.data[seq(1,2*length(data),by=2)] <- Re(data)
    w.data[seq(2,2*length(data),by=2)] <- Im(data)
    writeBin(as.vector(as.double(w.data)),fid,size=8)

    close(fid)
  }
  
write.bin.file.int <- function(fname,data)
{
	## We are wasting disk space here, but for compatibility
	## this is somewhat sensible thing to do
	fid<-file(fname,"wb")
	writeBin(as.vector(as.integer(data)),fid,size=4)
	close(fid)
} 

########################################################



## Namelist writing routines ###########################

write.flips.namelist <- function(e,mode,resid=FALSE,cov=FALSE,f.cov=FALSE,bsize=100)
  {
    if (e$real)
      {
        cc <- 0
      }
    else
      {
        cc <- 1
      }
    
    #if (e$band)
    #{
    	bbb <- e$band
    #}
    #else
    #{
    #	bbb <- 0	
    #}
    
    
    fid <- file("rflips.nml","w")
    #header
    cat("&PARAMS",file=fid,sep="\n")
    #data
    cat("mode = ",sprintf('%.0f',mode),"\n",file=fid)
    cat("cplx = ",sprintf('%.0f',cc),"\n",file=fid)
    cat("id = ",'"',e$ID,'"',"\n",sep="",file=fid)
    cat("ncols = ",sprintf('%.0f',e$ncols),"\n",file=fid)
    cat("nrhs = ",sprintf('%.0f',e$nrhs),"\n",file=fid)
    cat("crows = ",sprintf('%.0f',e$crows),"\n",file=fid)
    cat("newrows = ",sprintf('%.0f',e$newrows),"\n",file=fid)
    cat("naddvar = ",sprintf('%.0f',e$n.addvar),"\n",file=fid)
    cat("ndelvar = ",sprintf('%.0f',e$n.delvar),"\n",file=fid)
    cat("bsize = ",sprintf('%.0f',bsize),"\n",file=fid)
    cat("common = ",sprintf('%.0f',e$common),"\n",file=fid)
    cat("band =",sprintf('%.0f',bbb),"\n",file=fid)
    
    if (e$single)
    {
    	cc <- 0
    }
    else
    {
    	cc <- 1
    }
    cat("dbl=",cc,"\n",file=fid)

    if (e$r.written)
      {
        cc <- 1
      }
    else
      {
        cc <- 0
      }
    cat("rexists =",cc,"\n",file=fid)
    
    if (resid)
      {
        cc <- 1
      }
    else
      {
        cc <- 0
      }
    cat("calc_resid =",cc,"\n",file=fid)

    if (cov)
      {
        cc <- 1
      }
    else
      {
        cc <- 0
      }
    cat("calc_cov =",cc,"\n",file=fid)

    if (f.cov)
      {
        cc <- 1
      }
    else
      {
        cc <- 0
      }
    cat("full_cov =",cc,"\n",file=fid)
    
    #footer
    cat("/\n\n",file=fid)
    close(fid)
  }

########################################################


## Adding data #########################################


flips.add <- function(e,A.data,M.data,E.data=1)
{
	## Check input arguments
	
	#A.data<-as.numeric(A.data)
	#M.data<-as.numeric(M.data)
	#E.data<-as.numeric(E.data)
	
	
	## Is error given as a matrix
	if (is.matrix(E.data))
	{
		Emat=TRUE
	}
	else
	{
		Emat=FALSE
	}
	
	## theory matrix
	if (is.vector(A.data))
	{
		if (length(A.data)/e$ncols != as.integer(length(A.data)/e$ncols))
		{
			stop('flips.add: theory matrix ha wrong size!')
		}
		else
		{
			num.rows <- length(A.data)/e$ncols
		}
		
		## Reshape into a matrix
		A.data <- t(matrix(A.data,e$ncols,num.rows))
	}	
	else
	{
		if (ncol(A.data) != e$ncols)
		{
			stop('flips.add: theory matrix has wrong number of columns!')
		}
		else
		{
			num.rows <- nrow(A.data)
		}
	}
	
	## measurement

	
	if (is.vector(M.data))
	{
		if (length(M.data) != e$nrhs * num.rows)
		{
			stop('flips.add: measurement vector has wrong size!')
		}
		else
		{
			## reshape as matrix
			M.data <- t(matrix(M.data,e$nrhs,num.rows))
		}
	}
	else
	{
		if (!all(c(num.rows,e$nrhs)==dim(M.data)))
		{
			stop('flips.add: measurement matrix has wrong shape!')
		}
	}
	

	
	## error
	if (Emat)
	{
		if (!all(c(num.rows,num.rows)==dim(E.data)))
		{
			stop('flips.add: error covariance matrix has wrong size!')
		}
		else
		{
			C <- chol(E.data)
			A.data <- backsolve(C,A.data,transpose=T)
			M.data <- backsolve(C,M.data,transpose=T)
			E.data <- rep(1.0,num.rows)	
		}	
	}
	else
	{
		if (length(E.data)==1)
		{
			E.data <- rep(E.data,num.rows)
		}
		else
		{
			if ( length(E.data) != num.rows)
			{
				stop('flips.add: error vector has wrong length!')
			}
		}
	}
	
	
	## All ok. Write data into files and update FLIPS env
	
	if (! e$append)
	{
		e$sol.written = FALSE
		e$append <-TRUE
		e$newrows <- e$newrows + num.rows	
		
		e$write.file(e$A.file,t(A.data))
        e$write.file(e$M.file,t(M.data))
        e$write.err(e$E.file,t(E.data))
	}
	else
	{
		e$newrows <- e$newrows + num.rows
	
		e$append.file(e$A.file,t(A.data))
        e$append.file(e$M.file,t(M.data))
        e$append.err(e$E.file,t(E.data))

		
	}
	
}


## Solve problem #######################################

flips.solve <- function(e,mode='',buffersize=100)
  {
  	if (mode=='')
  	{
  		residual = FALSE
  		covariance = FALSE
  		full.covariance = FALSE
  	}
	else if (mode=='r')
  	{
  		residual = TRUE
  		covariance = FALSE
  		full.covariance = FALSE
  	}
  	else if (mode=='rc')
  	{
  		residual = TRUE
  		covariance = TRUE
  		full.covariance = FALSE
  	}
  	else if (mode=='rfc')
  	{
  		residual = TRUE
  		covariance = TRUE
  		full.covariance = TRUE
  	}
  	else if (mode=='c')
  	{
  		residual = FALSE
  		covariance = TRUE
  		full.covariance = FALSE
  	}
  	else if (mode=='fc')
  	{
  		residual = FALSE
  		covariance = TRUE
  		full.covariance = TRUE
  	}
  	else
  	{
  		stop('flips.solve: unknown mode!')
  	}
  	
  	
    ## Write namelist file for solving
    write.flips.namelist(e,1,residual,covariance,full.covariance,buffersize)
    
    ## Call rflips engine
    system(FLIPS$path)
    
    ## Remove old datafiles
    if (file.exists(e$A.file)) file.remove(e$A.file)
    if (file.exists(e$M.file)) file.remove(e$M.file)
    if (file.exists(e$E.file)) file.remove(e$E.file)    

    ## update environment
    e$crows <- e$crows + e$newrows
    e$newrows <- 0
	if(e$crows > e$ncols) e$crows <- e$ncols    
    
    e$append <-FALSE
    
    e$r.written <- TRUE
    e$sol.written <- TRUE

    if (residual) e$residual.written <- TRUE
    if (covariance) e$covariance.written <- TRUE
    if (full.covariance) e$covariance.full <- TRUE
    
    		
	temp <- read.bw(e)
	e$band <- temp[1]
	e$common<- temp[2]

    
    ## Read solution
    e$solution <- e$read.file(e$S.file,e$ncols*e$nrhs)

    ## Read solution
    ##if (e$real)
    ##  {
    ##    e$solution <- read.bin.file.single(e$S.file,e$ncols*e$nrhs)
    ##  }
    ##else
    ##  {
    ##    e$solution <- read.bin.file.scmpx(e$S.file,e$ncols*e$nrhs)
    ##  }
    
    dim(e$solution) <- c(e$nrhs,e$ncols)
    e$solution <- t(e$solution)

    ## Read residual
    if (residual)
      {
        e$residual <- e$read.resid(e$resid.file,e$nrhs)
      }
      
    ## Read covariance
    if (covariance)
    {
    	if (full.covariance)
    	{
    		e$covariance <- e$read.file(e$cov.file,e$ncols**2)
            dim(e$covariance) <- c(e$ncols,e$ncols)
            e$covariance <- t(e$covariance)
        }
        else
        {
        	e$covariance <- e$read.file(e$cov.file,e$ncols)
        }
    }
  }
########################################################


## Copy FLIPS environments #############################

flips.copy <- function(from,new.ID=NULL)
## Copies FLIPS environment into a new FLIPS environment with new ID
{

	## Before copying anything, make sure that all data is rotated.
	if (from$newrows > 0)
	{
		#tmp <- e$newrows
		#cat('Rotating\n')
		flips.rotate(from)	
		#e$newrows <- tmp
	}

	
	## Create new FLIPS environment
	if (is.null(new.ID))
	{
		to <- flips.init(ncols=from$ncols,nrhs=from$nrhs,type=from$type,common=from$common)
	}
	else
	{
		to <- flips.init(ncols=from$ncols,nrhs=from$nrhs,type=from$type,common=from$common,ID=new.ID)			
	}

	
	## Copy variables
	to$covariance <-from$covariance
	to$residual<-from$residual
	to$solution<-from$solution
	to$covariance.full<-from$covariance
	to$covariance.written<-from$covariance.written
	to$residual.written<-from$residual.written
	to$rinv.written<-from$rinv.written
	to$sol.written<-from$sol.written
	to$r.written<-from$r.written
	to$newrows<-from$newrows
	to$crows<-from$crows
	to$common <- from$common
	to$band <- from$band
	

	## Copy files (not rmask)
    if (file.exists(from$R.file)) file.copy(from$R.file,to$R.file,overwrite=TRUE)
    if (file.exists(from$Y.file)) file.copy(from$Y.file,to$Y.file,overwrite=TRUE)
    if (file.exists(from$S.file)) file.copy(from$S.file,to$S.file,overwrite=TRUE)
    if (file.exists(from$resid.file)) file.copy(from$resid.file,to$resid.file,overwrite=TRUE)
    if (file.exists(from$cov.file)) file.copy(from$cov.file,to$cov.file,overwrite=TRUE)
    if (file.exists(from$Rinv.file)) file.copy(from$Rinv.file,to$Rinv.file,overwrite=TRUE)
    if (file.exists(from$A.file)) file.copy(from$A.file,to$A.file,overwrite=TRUE)
    if (file.exists(from$M.file)) file.copy(from$M.file,to$M.file,overwrite=TRUE)
    if (file.exists(from$E.file)) file.copy(from$E.file,to$E.file,overwrite=TRUE)
    if (file.exists(from$tres.file)) file.copy(from$tres.file,to$tres.file,overwrite=TRUE)
    if (file.exists(from$cv.file)) file.copy(from$cv.file,to$cv.file,overwrite=TRUE)
    if (file.exists(from$bw.file)) file.copy(from$bw.file,to$bw.file,overwrite=TRUE)
	
	return(to)
		
}

## Resize FLIPS problem ################################

## If there are unrotated datarows, rotate them in first
## Then marginalize and/or add variables.
## Everything is done in rflipseng. We just write a namelist
## with mode==2 and the "remove mask vector" on the disk.

flips.resize <- function(e,remove=NULL,add=0,buffersize=100)
{
	## Construct remove mask vector. Variables to be removed will be marked with 1's
	## others by zeros.
	
	e$n.addvar <- add
	e$n.delvar <- length(remove)
	
	if (!is.null(remove))
	{
		rmask <- rep(0,e$ncols)
		rmask[remove] <- 1
		if (e$common>0)
		{
			nremcv <- sum(rmask[(e$ncols-e$common+1):e$ncols])
		}
		else
		{
			nremcv<-0	
		}
		#cat(rmask[(e$ncols-e$common+1):e$ncols],'\n')
			
		## Write remove mask vector
		write.bin.file.int(e$rmask.file,rmask)
	}
	else
	{
		nremcv <- 0	
	}
		
		## Write namelist
		write.flips.namelist(e,2,bsize=buffersize)
		
		## Run rflipseng
		system(FLIPS$path)
		
        ## Remove old datafiles
        if (file.exists(e$A.file)) file.remove(e$A.file)
        if (file.exists(e$M.file)) file.remove(e$M.file)
        if (file.exists(e$E.file)) file.remove(e$E.file)  
		
		## Update internal variables
		e$crows <- e$crows + e$newrows
		e$newrows <- 0
		
	if (e$crows > e$ncols) e$crows <- e$ncols

		
		## New size of the problem
		e$ncols <- e$ncols - e$n.delvar + e$n.addvar
		
		## Current rows in resized problem
		e$crows <- e$crows - e$n.delvar
		
		## All rows are rotated in, so
		##e$newrows <- 0
		## Solution is no longer valid
		e$sol.written <- FALSE
		## New R exists
		e$r.written <- TRUE
		## Residuals and covariances are no longer valid
		e$residual.written <- FALSE
		e$covariance.written <- FALSE
		e$covariance.full <- FALSE
		e$rinv.written <- FALSE
		e$append <- FALSE
		
		## Update the number of common variables
		#nremcv <- sum(rmask[(e$ncols-e$common+1):e$ncols])
		##cat('nremcv',nremcv,'add',add,'\n')
		
		temp <- read.bw(e)
		e$band <- temp[1]
		e$common<- temp[2]
		
		e$n.addvar <- 0
		e$n.delvar <- 0	
		
		#e$common <- e$common - nremcv + add
		
		## if number of common variables is zero, delete the cv-file
		##cat('ECOMMON=',e$common,'\n')
		#if (e$common==0)
		#{
		#	if (file.exists(e$cv.file)) file.remove(e$cv.file)  
		#}
}


########################################################

flips.rotate <- function(e,buffersize=100)
{
	## Write namelist with mode 3
	write.flips.namelist(e,3,bsize=buffersize)
		
	## Run rflipseng
	system(FLIPS$path)
		
    ## Remove old datafiles
    if (file.exists(e$A.file)) file.remove(e$A.file)
    if (file.exists(e$M.file)) file.remove(e$M.file)
    if (file.exists(e$E.file)) file.remove(e$E.file)  
		
	## Update internal variables
	e$crows <- e$crows + e$newrows
	e$newrows <- 0
	
	if (e$crows > e$ncols) e$crows <- e$ncols
		
	## New size of the problem
	##e$ncols <- e$ncols - e$n.delvar + e$n.addvar
		
	## Current rows in resized problem
	##e$crows <- e$crows - e$n.delvar
		

	e$sol.written <- FALSE
	e$r.written <- TRUE
	e$residual.written <- FALSE
	e$covariance.written <- FALSE
	e$covariance.full <- FALSE
	e$rinv.written <- FALSE
	e$append <- FALSE
	
			
	temp <- read.bw(e)
	e$band <- temp[1]
	e$common<- temp[2]


	
}


########################################################

flips.delete <- function(e,A.data,M.data,E.data=1.0,buffersize=100)
{


	# Rotate unrotated rows
	if (e$newrows > 0)
	{
		#tmp <- e$newrows
		#cat('Rotating\n')
		flips.rotate(e)	
		#e$newrows <- tmp
	}
	
	# Write the rows-to-be-deleted into files
	#cat('Adding\n')
	flips.add(e,A.data,M.data,E.data)
	e$append <- FALSE
	#cat('Newrows',e$newrows,'Crows',e$crows,'\n')
	
	# Antirotate
	write.flips.namelist(e,4,bsize=buffersize)
	#stop
	#cat('Running FLIPSeng\n')
	system(FLIPS$path,wait=T)
	
    ## Remove old datafiles
    if (file.exists(e$A.file)) file.remove(e$A.file)
    if (file.exists(e$M.file)) file.remove(e$M.file)
    if (file.exists(e$E.file)) file.remove(e$E.file)
    
	## Update internal variables
	# 2013-09-27 (Mikko O): Fixed crows calculation
	# There should be a check that crows is equal to ncols
	# before attempting deletion.
	# Maybe we should have total rows stored somewhere?
	# e$crows <- e$crows - e$newrows
	e$newrows <- 0   
	
if(e$crows > e$ncols) e$crows <- e$ncols
	
			
	temp <- read.bw(e)
	e$band <- temp[1]
	e$common<- temp[2]

	
	e$sol.written <- FALSE
	e$r.written <- TRUE
	e$residual.written <- FALSE
	e$covariance.written <- FALSE
	e$covariance.full <- FALSE
	e$rinv.written <- FALSE
	e$append <- FALSE		
}

########################################################


flips.get.R <- function(h)
{
	# Fetches target matrix R from the binary file
	
	# Fix 2013-09-27 (Mikko O):
	# First rotate unrotated rows
	if (h$newrows > 0)
	{
		#tmp <- e$newrows
		#cat('Rotating\n')
		flips.rotate(h)	
		#e$newrows <- tmp
	}
	
	if ( h$r.written)
	{
		temp <- read.bw(h)
		bw <- temp[1]
		n <- h$ncols
		cv <- temp[2]
		nn <- n -cv
		r.len <- (nn-bw)*bw + bw*(bw+1)/2
		#r.len <- (nn-bw)*(nn-bw+1)/2
		R <- h$read.file(h$R.file,r.len)
		if (cv > 0)
		{
			cv.len <- nn*cv + cv*(cv+1)/2
			R.cv <- h$read.file(h$cv.file,cv.len)
			Rcv1 <- matrix(R.cv[1:(nn*cv)],nn,cv,byrow=T)
			Rcv2 <- R.cv[(nn*cv+1):cv.len]	
		}
	}
	else
	{
		stop('flips.get.R: Target matrix is not written!')
	}
	
	RR<-matrix(0,n,n)
	R1 <- matrix(R[1:((nn-bw)*bw)],nn-bw,bw,byrow=T)
	
	R2 <- R[((nn-bw)*bw+1):r.len]
	
	if (bw < nn){		
		for (i in 1:(nn-bw))
		{
			RR[i,i:(i+bw-1)] <- R1[i,]	
		}
	}
	
	j<-0
	for (i in (nn-bw+1):nn)
	{
		j<-j+1
		st <- j + (j-1)*(2*bw-j)/2
		en <- bw + (j-1)*(2*bw-j)/2
		RR[i,i:nn] <- R2[st:en]	
	}
	
	if (cv > 0)
	{
		for (i in 1:nn)
		{
			RR[i,(n-cv+1):n] <- Rcv1[i,]	
		}
		j <- 0
		for(i in (nn+1):n)
		{
			j <- j + 1
			st <- j + (j-1)*(2*cv-j)/2
			en <- cv + (j-1)*(2*cv-j)/2
			RR[i,i:n] <- Rcv2[st:en]	
		}
	}
	
	return(RR)	
}

#########################################################

flips.get.Y <- function(h)
{
	# Fetches target matrix R from the binary file
	
	# Fix 2013-09-27 (Mikko O):
	# First rotate unrotated rows
	if (h$newrows > 0)
	{
		#tmp <- e$newrows
		#cat('Rotating\n')
		flips.rotate(h)	
		#e$newrows <- tmp
	}
	
	if ( h$r.written)
	{
		n <- h$ncols*h$nrhs
		Y <- h$read.file(h$Y.file,n)
	}
	else
	{
		stop('flips.get.Y: Target vector is not written!')
	}
	
	YY <- matrix(Y,h$ncols,h$nrhs,byrow=T)
	
	return(YY)	
}

#########################################################

# Not available anymore
# This is pretty stupid function anyway
# 
#flips.put.R <-function(h,data)
#{
#	# Write R into a file
#	# R can be given either as a matrix or 
#	# a vector (in row-major order) consisting
#	# of upper triangular elements
#	
#	# Check data format
#	vData<-F
#	
#	if (!is.matrix(data))
#	{
#		l <- length(data)
#		data <- matrix(data,1,l)
#		vData<-T		
#	}
#		
#	if (dim(data)[2] == 1)
#	{
#		data <- t(data)
#		vData<-T
#	}
#	
#	if (vData)
#	{
#		# Vector data
#		
#		if (length(data) != h$ncols*(h$ncols+1)/2)
#		{
#			stop('flips.put.R: length of data vector is wrong!')
#		}
#		
#		h$write.file(h$R.file,data)
#			
#	}
#	else
#	{
#		if (any(dim(data) != c(h$ncols,h$ncols)))
#		{
#			stop('flips.put.R: data matrix has a wrong size!')
#		}
#		
#		n<-h$ncols
#		RR<-rep(0,n*(n+1)/2)
#		for (i in 1:n)
#		{
#			st <- i + (i-1)*(2*n-i)/2 
#			en <- n + (i-1)*(2*n-i)/2
#			RR[st:en] <- data[i,i:n]	
#		}
#		
#		h$write.file(h$R.file,RR)
#	}
#
#	h$r.written = TRUE
#		
#}

##########################################################

flips.put.Y <- function(h,data)
{
	# Write Y into file
	# Y can be given as a matrix or row-major ordered vector
	
	# Fix 2013-09-27 (Mikko O):
	# First rotate unrotated rows
	if (h$newrows > 0)
	{
		#tmp <- e$newrows
		#cat('Rotating\n')
		flips.rotate(h)	
		#e$newrows <- tmp
	}
	
	# Check data format
	vData<-F
	
	if (!is.matrix(data))
	{
		l <- length(data)
		data <- matrix(data,1,l)
		vData<-T		
	}
		
	if (dim(data)[2] == 1)
	{
		data <- t(data)
		vData<-T
	}

	if (vData)
	{
		if (length(data)!=h$ncols*h$nrhs)
		{
			stop('flips.put.Y: data vector has a wrong size!')		}
			
		h$write.file(h$Y.file,data)	
	}
	else
	{
		if (any(dim(data) != c(h$ncols,h$nrhs)))
		{
			stop('flips.put.Y: data matrix has a wrong size!')
		}	
		
		n<-h$ncols
		m <- h$nrhs
		YY<-rep(0,n*m)
		for ( i in 1:n)
		{
			YY[((i-1)*m+1):(i*m)] <- data[i,]	
		}
		
		h$write.file(h$Y.file,YY)
		
	}
	
}


# RFLIPS testing 
flips.test <- function(type,size,buffersizes,loop=1)
{
  	ncols <- size[2]
	rows <- size[1]
	A<-matrix(rnorm(ncols*rows),rows,ncols)
	sol<-rnorm(ncols)
	if (type=='c' || type=='z')
	{
		A <- A + 1i*matrix(rnorm(ncols*rows),rows,ncols)
		sol <- sol + 1i*rnorm(ncols)
	}

	m<-A%*%sol
	
	n<-length(buffersizes)
	acc <- rep(0,n)
	times <- rep(0,n)
  	flops <- 2 * ncols**3 + 3 * ncols**2 - 5 * ncols + 6 * (rows - ncols) * ncols + 3 * (rows - ncols) * ncols * (ncols + 1)
	
	for(k in 1:loop)
	{
	  for (i in 1:n)
	  {
	  	ss<-flips.problem(type,A,m,buffersizes[i])
	  	times[i] <- times[i] + ss$time[3]
	  	acc[i] <- acc[i] + max(abs(sol - ss$sol))

			
	  }
	}
  
	times <- times/loop
	acc <- acc/loop
  Gflops <- flops/1.0E9 / times
	
	return(list(times=times,accuracy=acc,Gflops=Gflops))
}

flips.problem <- function(type,A,m,bsize,wg.size)
{
	ncols <- ncol(A)
	h<-flips.init(ncols,1,type)
	tt <- proc.time()
	flips.add(h,A,m,1)
	flips.solve(h)
	tt2<-proc.time()
	#cat("LIPS time:",tt2-tt,"\n")
	aa <- h$solution
	flips.dispose(h)
	#cat(' init:',t1,"\n",sep=" ")
	#cat('  add:',t2,"\n",sep=" ")
	#cat('solve:',t3,"\n",sep=" ")
	return(list(sol=aa,time=tt2-tt))	
}
