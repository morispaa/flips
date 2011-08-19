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
define(`SUBRNAME',FUNC(rot_coeff,PTYPE))
!'


subroutine SUBRNAME (a,b,rot_cos,rot_sin,zt,skip,swap)
  ! Calculates the rotation coefficients

  implicit none

  real(PRECISION), intent(in) :: a,b,zt
  real(PRECISION), intent(out) :: rot_cos, rot_sin

  real(PRECISION) :: rr,fg2,absa,absb,t,u, one
  
  logical, intent(out) :: skip, swap

  one = 1.0

  skip = .FALSE.
  swap = .FALSE.

  absa = abs(a)
  absb = abs(b)



  if (absb < zt) then
     rot_cos = sign(one,a)
     rot_sin = 0.0
     skip = .TRUE.
  else if (absa < zt) then
     rot_cos = 0.0
     rot_sin = sign(one,b)
     swap = .TRUE.
  else if (absb > absa ) then
     t = a/b
     u = sign(sqrt(1+t**2),b)
     rot_sin = 1/u
     rot_cos = rot_sin*t
  else
     t = b/a
     u = sign(sqrt(1+t**2),a)
     rot_cos = 1/u
     rot_sin = rot_cos*t
  end if






   end subroutine SUBRNAME



