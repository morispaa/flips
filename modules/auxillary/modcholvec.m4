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
define(`SUBRNAME',FUNC(modcholvec,PTYPE))
!'

subroutine SUBRNAME (n,nn,mat,fac)
  ! Modified Cholesky factorization
  ! Vector version

  ! Calculates upper triangular fac, such as
  ! matmul(fac,transpose(fac)) = mat

  ! Only the upper triangular part is used (in row-major ordered vector)

  ! This is a streamlined modification of the Algorithm 23.1
  ! of Trefethen-Bau, Numerical Linear Algebra, SIAM, 1997.

  implicit none

  integer, intent(in) :: n,nn
  DTYPE (PRECISION), dimension(nn), intent(in) :: mat
  DTYPE (PRECISION), dimension(nn), intent(out) :: fac

  DTYPE (PRECISION), dimension(n) :: tmpcol
  DTYPE (PRECISION) :: alpha,ss
  integer :: i,j,rst,ren
  real :: sss

  fac = mat

  do i = n,2,-1

     ss = 1.0/fac(rind(i,i,n))
     alpha = sqrt(ss)

     do j = 1,i-1
        tmpcol(j) = fac(rind(j,i,n))
     end do

     do j = 1,i-1
        rst = rind(j,j,n)
        ren = rst + i -j 
        sss = tmpcol(j) * ss
        fac(rst:ren) = fac(rst:ren) - tmpcol(j:i-1)*sss
        fac(rind(j,i,n)) = fac(rind(j,i,n))*alpha
     end do

     fac(rind(i,i,n)) = sqrt(fac(rind(i,i,n)))

  end do

  fac(1) = sqrt(fac(1))



end subroutine SUBRNAME
