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
define(`SUBRNAME',FUNC(find_first_and_len,PTYPE))

!'

subroutine SUBRNAME (ncols,n,zero_th,arows,arst,arle)
!******************************************************************
! find_first_and_len - finds first non-zero element and the length
!                      of non-zero elements on each row of a given
!                      matrix (in row-major vector form)
!******************************************************************

  implicit none

  integer, intent(in) :: ncols, n
  real(PRECISION), intent(in) :: zero_th
  DTYPE (PRECISION), dimension(ncols*n), intent(in) :: arows
  integer, dimension(n), intent(out) :: arle, arst
  
  integer :: i,j
  DTYPE (PRECISION), dimension(ncols) :: wrkrow



  rows: do j = 1,n

     arst(j) = 0
     arle(j) = 0

     wrkrow = arows(yind(j,1,ncols):yind(j,ncols,ncols))

     f_min: do i = 1,ncols
        if (abs(wrkrow(i)) > zero_th) then
           arst(j) = i
           exit f_min
        end if
     end do f_min

     f_max: do i = ncols,1,-1
        if (abs(wrkrow(i)) > zero_th) then
           arle(j) = i
           exit f_max
        end if
     end do f_max

  end do rows

end subroutine SUBRNAME
