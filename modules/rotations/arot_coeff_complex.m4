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

!*****************************************************************
! arot_coeff_complex - calculate complex antirotation coefficients
!*****************************************************************

include(`m4/'PTYPE`_type.m4')
define(`FUNC',``$1'_`$2'')
define(`SUBRNAME',FUNC(arot_coeff,PTYPE))
!'

subroutine SUBRNAME (a,b,s,c,zeroth)
  implicit none

  DTYPE (PRECISION), intent(in) :: a,b
  DTYPE (PRECISION), intent(out) :: s,c
  real (PRECISION) :: zeroth

  real (PRECISION) :: a2,b2,ab2

  a2 = real(a)**2 + aimag(a)**2
  b2 = real(b)**2 +aimag(b)**2
  ab2 = a2 - b2

  if (b2 < zeroth) then
     c = conjg(a)/sqrt(a2)
     s = (0.0,0.0)
  elseif (a2 < zeroth) then
     ! This should not happen!!
     c = (0.0,0.0)
     s = -b/sqrt(b2)
  elseif (abs(a2-b2)<zeroth) then
     ! This should never happen either
     c = (1.0,0.0)
     s = (1.0,0.0)
  else

     c = conjg(a)/sqrt(ab2)
     s = conjg(b)/sqrt(ab2)

  end if



end subroutine SUBRNAME
