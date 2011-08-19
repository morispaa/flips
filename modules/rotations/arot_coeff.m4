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
! arot_coeff - calculate antirotation coefficients
!*****************************************************************

include(`m4/'PTYPE`_type.m4')
define(`FUNC',``$1'_`$2'')
define(`SUBRNAME',FUNC(arot_coeff,PTYPE))
!'

subroutine SUBRNAME (a,b,s,c,zeroth)
  implicit none

  DTYPE (PRECISION), intent(in) :: a,b,zeroth
  DTYPE (PRECISION), intent(out) :: s,c

  DTYPE (PRECISION) :: t

  if (abs(b) < zeroth) then
     if ( a<0) then
        c = -1.0
     else
        c = 1.0
     end if
     s = 0.0
  else if (abs(a) < zeroth) then
     ! This should never happen! R must be
     ! ready and full upper triangular
     s = -1.0
     c = 0.0
  else if ( abs(a-b) < zeroth) then
     ! This is another horrible hack...
     ! and should not happen if
     ! real data rows are deleted
     s = 1.0
     c = 1.0

  else if (abs(a) > abs(b)) then
     t = b/a
     c = 1.0/sqrt(1-t**2)
     s = c*t

  else
     t = a/b
     s = 1.0/sqrt(1-t**2)
     c = t*s

  end if


end subroutine SUBRNAME
