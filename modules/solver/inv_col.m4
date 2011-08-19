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
define(`SUBRNAME',FUNC(inv_col,PTYPE))
!'

subroutine SUBRNAME (ftype,wvec,m)
  ! Inverts mth column of R (upper triag)
  ! and writes it in wvec(1:m)
  ! Used when inverting R

  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  DTYPE (PRECISION), dimension(ftype%ncols), intent(out) :: wvec
  integer, intent(in) :: m

  ! Local things
  DTYPE (PRECISION), dimension(ftype%ncols) :: rwrk
  integer :: j
  DTYPE (PRECISION) :: tmpsum

  ! Get the mth row of R
  rwrk = 0.0
  call FUNC(get_row,PTYPE) ('rmat',ftype,size(rwrk),rwrk,m)

  wvec(m) = 1.0/rwrk(1)

  do j = m-1,1,-1
     ! Get jth row of R
     rwrk = 0.0
     call FUNC(get_row,PTYPE) ('rmat',ftype,size(rwrk),rwrk,j)

     tmpsum = sum(rwrk(2:m-j+1) * wvec(j+1:m),1)
     wvec(j) = -tmpsum/rwrk(1)

  end do

end subroutine SUBRNAME
