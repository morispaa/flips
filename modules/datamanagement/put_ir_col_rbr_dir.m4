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
define(`SUBRNAME',FUNC(put_r_col_dir,PTYPE))
!'

subroutine SUBRNAME (ftype,datavec,n)
  ! In this version the columns are stored in rob-by-row packed
  ! matrix (the old one was col-by-col)
  ! This is most probably considerably slower but it makes 
  ! things more consistent
  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  DTYPE (PRECISION), dimension(ftype%ncols), intent(in) :: datavec
  integer, intent(in) :: n

  integer :: j, idx, k

  if (ftype%use_files) then
     ! Write to DIRECT ACCESS (!!!!!) file
     do j = 1,n
        k = rind(j,n,ftype%ncols)
        write(ftype%idnum*100+15,REC=k) datavec(j)
     end do
  else
     do j = 1,n
        idx = rind(j,n,ftype%ncols)
        ftype%invrmat(idx) = datavec(j)
     end do
  end if

end subroutine SUBRNAME
