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
define(`SUBRNAME',FUNC(modcholmat,PTYPE))  
!'

subroutine SUBRNAME (n,mat,fac)
  ! Subroutine: modcholmat_[sdcz]
  ! Modified Cholesky factorization
  ! A = C C^T
  ! where C is UPPER TRIAGONAL
  ! NB: The routine does not check the positive definedness nor the symmetriness
  ! of the factorized matrix.

  implicit none

  integer, intent(in) :: n
  DTYPE (PRECISION), dimension(n,n), intent(in) :: mat
  DTYPE (PRECISION), dimension(n,n), intent(out) :: fac

  integer :: k,j
  DTYPE (PRECISION) :: alpha 

  fac = mat

  do k = n,2,-1

     do j = 1,k-1

        fac(1:k-1,j) = fac(1:k-1,j) - fac(k,j) * fac(1:k-1,k) / fac(k,k)

     end do

     alpha = sqrt(fac(k,k))
     fac(1:k-1,k) = fac(1:k-1,k) / alpha
     fac(k,k) = alpha

  end do


  fac(1,1) = sqrt(fac(1,1))


  do j = 2,n

     fac(j,1:j-1) = 0.0

  end do

end subroutine SUBRNAME

