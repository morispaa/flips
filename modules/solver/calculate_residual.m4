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
define(`SUBRNAME',FUNC(calculate_residual,PTYPE))
!'

subroutine SUBRNAME (ftype)
  implicit none

  type(FOBTYPE) :: ftype

  DTYPE (PRECISION), dimension(ftype%ncols,ftype%nrhs) :: tmpmat
  integer :: i,j
  DTYPE (PRECISION), dimension(ftype%ncols) :: rrow
  DTYPE (PRECISION), dimension(ftype%nrhs) :: yrow
  DTYPE (PRECISION), dimension(ftype%ncols,ftype%nrhs) :: ymat

  ! Check that solution exists, if not print error and exit
  if (ftype%solexists .EQV. .FALSE.) then
     write(0,*) "Can't calculate residual if solution is not calculated!!"
     stop
  end if

  ! Load solmat
  do i = 1,ftype%ncols
     call FUNC(get_row,PTYPE) ('solu',ftype,size(yrow),yrow,i)
     ymat(i,:) = yrow
  end do


  do i = 1,ftype%ncols

     ! Get R row
     rrow = 0.0
     call FUNC(get_row,PTYPE) ('rmat',ftype,size(rrow),rrow,i)

     do j = 1,ftype%nrhs
        tmpmat(i,j) = sum(rrow(1:ftype%ncols-i+1) * ymat(i:ftype%ncols,j))
     end do
  end do

  ftype%residual = ftype%tmpres - sum(real(tmpmat,PRECISION)**2 ifdef(`M4_COMPLEX',`+ aimag(tmpmat)**2,1') )

end subroutine SUBRNAME
