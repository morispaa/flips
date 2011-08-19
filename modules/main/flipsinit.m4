!-*-f90-*-

!   FLIPS (Fortran Linear Inverse Problem Solver)
!   
!   Copyright 2005-2009 University of Oulu, Finland. All rights reserved.
!   Written by  Mikko Orispaa <mikko.orispaa@oulu.fi>
!   
!   Redistribution and use in source and binary forms, with or without modification, 
!   are permitted provided that the following conditions are met:
!   
!      1. Redistributions of source code must retain the above copyright notice, this 
!         list of conditions and the following disclaimer.
!      2. Redistributions in binary form must reproduce the above copyright notice, this list 
!         of conditions and the following disclaimer in the documentation and/or other materials 
!         provided with the distribution.
!   
!   THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY OF OULU ``AS IS'' AND ANY EXPRESS OR IMPLIED 
!   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
!   FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF OULU 
!   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
!   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS 
!   OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
!   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
!   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
!   ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!   
!   The views and conclusions contained in the software and documentation are those of 
!   the authors and should not be interpreted as representing official policies, either 
!   expressed or implied, of University of Oulu.

include(`m4/'PTYPE`_type.m4')
define(`FUNC',``$1'_`$2'')
define(`SUBRNAME',FUNC(flipsinit,PTYPE))
!'

!*****************************************************************
! flips_init - initialize flips data type
!*****************************************************************
subroutine SUBRNAME (ftype,ncol,nrhs,bandwidth,common,idnum,buffersize,zerothreshold)
  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  integer, intent(in) :: ncol
  integer, intent(in) :: nrhs
  integer, intent(in), optional :: bandwidth
  integer, intent(in), optional :: common
  integer, intent(in), optional :: idnum
  integer, intent(in), optional :: buffersize
  real(PRECISION), intent(in), optional :: zerothreshold

  integer :: rsize, ysize, status,nn,cvsize,n
  character(len=32) :: auxstring

  DTYPE (PRECISION) :: rl_test
  




  inquire(IOLENGTH=ftype%rl) rl_test

  ! Size of Y-matrix
  ysize = ncol*nrhs

  ! Load values to ftype

  ftype%cplx = CPLTYPE
  ftype%dbl = DBLTYPE


  ftype%solexists = .FALSE.
  ftype%covexists = .FALSE.
  ftype%rinverted = .FALSE.
  ftype%ncols = ncol
  ftype%nrhs = nrhs
  ftype%nbuf = 0
  ftype%nrows = 0
  ftype%bbw = 0





  ! Initial bandwidth of the theory matrix
  ! If not given, bandwidth = no. of cols
  if (present(bandwidth)) then
     ftype%bw = bandwidth
  else
     ftype%bw = ncol
  end if



  ! Common variables
  ! Fix the bandwidth
  if (present(common)) then
     ftype%common = common
     if (ftype%bw + ftype%common > ftype%ncols) then
        ftype%bw = ftype%ncols - ftype%common
        if (ftype%bw <= 0) then
           if (verbose) then
              write(0,*) 'FLIPS WARNING: flips_init: number of common variables not compatible' 
              write(0,*) 'with the problem size and/or bandwidth!'
              write(0,*) 'Setting number of common variables to zero and bandwidth to the problem size!'
           end if
           ftype%bw = ncol
           ftype%common = 0
        end if
     end if
  else
     ftype%common = 0
  end if



  ! Check that the sum of bandwidth and common variables does not exceed ncol
  if (ftype%common + ftype%bw > ncol) then
     ! Exit with error
     write(0,*) 'FLIPS ERROR: flips_init: the sum of bandwidth and common variables' 
     write(0,*) 'exceed the number of unknowns!'
     stop
  end if



  nn = ncol - ftype%common

  ! Size of common matrix
  if (ftype%common > 0) then
     cvsize = ftype%common*nn + ftype%common*(ftype%common+1)/2
  end if



  ! Size of R-matrix
  if (ftype%bw == ncol) then
     rsize = ncol*(ncol+1)/2
  else
     rsize = ftype%bw*(nn-ftype%bw)+ftype%bw*(ftype%bw+1)/2
  end if

  ! User defined rotation buffer size
  if (present(buffersize)) then
     ftype%nrotbuf = buffersize
  else
     ftype%nrotbuf = 100
  end if

  ! Allocate buffers
  allocate(ftype%tmpres(ftype%nrhs),ftype%residual(ftype%nrhs),&
       ftype%arotbuf(ftype%nrotbuf*ftype%ncols),&
       ftype%mrotbuf(ftype%nrotbuf*ftype%nrhs),STAT=status)
  if (status/=0) then
     write(0,*) 'flips ERROR: flips_init: Could not allocate flips data type. Status:',status
     stop
  end if

  ftype%tmpres = 0.0

  ! Allocate matrices/files
  if (present(idnum)) then
     ! Using files
     ftype%use_files = .TRUE.
     ftype%idnum = idnum
     ! File unit numbers
     ftype%rfileunit = idnum*100 + 11
     ftype%yfileunit = idnum*100 + 12
     ftype%cfileunit = idnum*100 + 14
     ftype%irfileunit = idnum*100 + 15
     ftype%solfileunit = idnum*100 + 13

     if (ftype%bw<ncol) then
        ftype%bw = ncol
        rsize = ncol*(ncol+1)/2
        write(0,*) 'FLIPS WARNING: flips_init: bandwidth argument not implemented',&
             ' for  file storage! Bandwidth is set to the maximum.'
     end if

     if (ftype%common > 0) then
        ftype%common = 0
        cvsize = 0
        write(0,*) 'FLIPS WARNING: flips_init: common argument not implemented for file storage! Common variables set to zero.'
     end if


     call FUNC(open_files,PTYPE) (ftype)

  else
     ! Using memory
     ftype%use_files =.FALSE.
     ! Allocate matrices (NB: Posteriori covariance matrix is NOT 
     ! allocated here.)
     allocate(ftype%rmat(rsize), ftype%ymat(ysize), &
          ftype%solmat(ysize),STAT=status)
     if (status/=0) then
        write(0,*) 'flips ERROR: flips_init: Could not allocate flips data type. Status:',status
        stop
     end if

     ftype%rmat = 0.0     
     ftype%ymat = 0.0
     ftype%solmat = 0.0

     ! If we have common variables, allocate vector for them
     if (ftype%common > 0 ) then
        allocate(ftype%cvmat(cvsize))
        ftype%cvmat = 0.0
     end if

     ! Fill other fields
     ftype%idnum = -1
     auxstring ='not used'
     ftype%rfile = auxstring
     ftype%yfile = auxstring
     ftype%cfile = auxstring
     ftype%solfile = auxstring

  end if



  ! Row length book keeping
  allocate(ftype%rst(ncol),ftype%rle(ncol),ftype%bst(ftype%nrotbuf),ftype%ble(ftype%nrotbuf))

  ftype%rst = 0
  ftype%bst = 0
  ftype%rle = 0
  ftype%ble = 0

  ! If common variables exist, set the common variable part in advance
  ! Makes thing simpler later...
  !if (ftype%common > 0 ) then
  !   do n = nn+1,ftype%ncols
  !      ftype%rst(n) = nn+1
  !      ! Maybe this works better...
  !      ftype%rle(n) = 0
  !   end do
  !end if



if (present(zerothreshold)) then
   ftype%zeroth = zerothreshold
else
   ftype%zeroth = 1.0E-10
end if
   

  ! If verbose TRUE, print info
  if (verbose) then
     write(6,*) '*****************************************'
     write(6,*) '* New flips data type initialized'

     call FUNC(print_info,PTYPE) (ftype)


  end if


end subroutine SUBRNAME
