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
define(`SUBRNAME',FUNC(get_vector,PTYPE))
!'

subroutine SUBRNAME (mtype,ftype,vec)
  ! Fetches FLIPS data matrix in vector form

  implicit none

  character(len=4) :: mtype
  type(FOBTYPE) :: ftype
  DTYPE (PRECISION), dimension(:) :: vec

  logical :: get_vec, utri
  integer :: veclen, rlen, ulen, fmlen, i
  DTYPE (PRECISION), dimension(:), allocatable :: datarow, datarow2
  character(len=200) :: etext


  get_vec=.TRUE.
  veclen = size(vec)
  ulen = ftype%ncols*(ftype%ncols+1)/2
  fmlen = ftype%ncols*ftype%nrhs

  select case (mtype)

  case ('solu')
     if (ftype%solexists) then
        rlen = fmlen
        utri = .FALSE.
     else
        get_vec = .FALSE.
        etext='Can not get solution. Solution does not exist.'
     end if

  case ('cova')
     if (ftype%covexists) then
        if (ftype%fullcov) then
           rlen = ulen
           utri = .TRUE.
        else
           rlen = ftype%ncols
           utri = .FALSE.
        end if
     else
        get_vec=.FALSE.
        etext='Can not get covariance. Covariance not calculated.'
     end if

  case ('rmat')
     rlen = ulen
     utri = .TRUE.

  case ('ymat')
     rlen = fmlen
     utri = .FALSE.

  case ('invr')
     if (ftype%rinverted) then
        rlen = ulen
        utri = .TRUE.
     else
        get_vec = .FALSE.
        etext='Can not get R inverse. R inverse is not calculated.'
     end if

  case default
     write(0,*) 'FLIPS error: get_vector: Unknown vector type ',mtype
     stop

  end select

  ! Check vector length
  if (veclen /= rlen) then
     write(0,*) 'FLIPS error: get_vector: Vector given as argument has wrong size!'
     stop
  end if

  if (get_vec) then
     vec = 0.0

     if (utri) then
        allocate(datarow(ftype%ncols))
     else
        allocate(datarow(ftype%nrhs))
     end if

     if (mtype=='cova' .AND. (.NOT. ftype%fullcov)) then
        allocate(datarow2(ftype%ncols))
        call FUNC(get_row,PTYPE) ('cova',ftype,size(datarow2),datarow2,1)
        vec = datarow2
        deallocate(datarow2)
     else
        do i = 1,ftype%nrows
           call FUNC(get_row,PTYPE) (mtype,ftype,size(datarow),datarow,i)

           if (utri) then
              vec(rind(i,i,ftype%ncols):rind(i,ftype%ncols,ftype%ncols)) = &
                   datarow(1:ftype%ncols-i+1)
           else
              vec(yind(i,1,ftype%nrhs):yind(i,ftype%nrhs,ftype%nrhs)) = datarow
           end if
        end do
        deallocate(datarow)
     end if

  else
     write(0,*) 'FLIPS error: get_vector: ',trim(etext)
     stop
  end if


end subroutine SUBRNAME
