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
  
  ! FLOPS: 26 ???

  implicit none

  complex(PRECISION), intent(in) :: a,b
  real(PRECISION), intent(in) :: zt
  complex(PRECISION), intent(out) :: rot_sin
  complex(PRECISION), intent(out) :: rot_cos

  real(PRECISION) :: n,sc,absa, f2,g2,fg2,d1
  complex(PRECISION) :: aa

  logical, intent(out) :: skip,swap

  skip = .FALSE.
  swap = .FALSE.

  f2 = real(a)**2 + aimag(a)**2
  g2 = real(b)**2 + aimag(b)**2
  fg2 = sqrt(f2+g2)

  if (f2+g2 < zt) then
     ! If both are zeros, skip rotation
     rot_cos = (1.0,0.0)
     rot_sin = (0.0,0.0)
     skip = .TRUE.
     return
  elseif (g2 < zt) then
     ! If b==0
     rot_cos = conjg(a)/sqrt(f2)
     rot_sin = (0.0,0.0)
     skip = .TRUE.
     return
  elseif (f2 < zt) then
     ! if a==0
     rot_cos = (0.0,0.0)
     rot_sin = conjg(b)/sqrt(g2)
     swap = .TRUE.
     return
  else
     rot_cos = conjg(a)/fg2
     rot_sin = conjg(b)/fg2
  end if
     

!!$  fg2 = f2 + g2
!!$
!!$  ! If b==0, skip this rotation
!!$  ! NB: This should not happen since this
!!$  ! is taken care of in rotation routines!
!!$  if (fg2 < zt) then
!!$     rot_cos = 1.0
!!$     rot_sin = (0.0,0.0)
!!$     skip = .TRUE.
!!$     return
!!$  end if
!!$
!!$  ! If a == 0 and b /= 0,
!!$  ! raise swap flag, i.e.,
!!$  ! swap R and buffer rows
!!$  if (f2 < zt) then
!!$     rot_cos = 0.0
!!$     rot_sin = (1.0,0.0)
!!$     swap = .TRUE.
!!$     return
!!$  end if
!!$
!!$  ! Calculate rotation coefficients
!!$  d1 = 1.0/sqrt(f2*fg2)
!!$
!!$  rot_cos = f2 * d1
!!$
!!$  rot_sin = a * d1
!!$  rot_sin = conjg(b) * rot_sin

end subroutine SUBRNAME
