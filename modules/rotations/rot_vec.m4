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
define(`SUBRNAME',FUNC(rot_vec,PTYPE))
!'


subroutine SUBRNAME (n,va,vb,rot_cos,rot_sin,skip,swap)
  implicit none

  ! ******************************************
  ! Replaces BLAS routine srot
  ! THIS SHOULD BE ALTIVEC'ED ASAP!
  ! FLOPS: 6*n
  ! ******************************************

  integer, intent(in) :: n
  DTYPE (PRECISION), dimension(n), intent(inout) :: va,vb
  DTYPE (PRECISION), intent(in) :: rot_cos
  DTYPE (PRECISION), intent(in) :: rot_sin

  DTYPE (PRECISION) :: tmpvec
  DTYPE (PRECISION), dimension(n) :: tmpv2

  logical :: skip,swap

  integer :: i

  ! Common special case (which should never happen)
  if (skip) then
     ifdef(`M4_REAL',`
     if (va(1) < 0.0) then
        va = -va
        vb = -vb
     end if')
     ifdef(`M4_COMPLEX',`
     if (aimag(va(1)) /= 0.0 .OR. real(va(1)) < 0) then
        va = rot_cos*va
        vb = conjg(rot_cos)*vb
     end if')
     return
  end if

  ! Swap va and vb rows
  if (swap) then

     ifdef(`M4_REAL',`
     tmpv2 = va
     if (vb(1) < 0 ) then
        va = -vb
        vb = tmpv2
     else
        va = vb
        vb = -tmpv2
     end if')

     ifdef(`M4_COMPLEX',`
     tmpv2 = va
     if (aimag(vb(1)) /= 0.0 .OR. real(vb(1)) < 0) then
        va = rot_sin*vb
        vb = tmpv2
     else
        va = vb
        vb = -tmpv2
     end if')

     return
  end if

 ! Rotate vectors normally
  do i = 1,n
     tmpvec = rot_cos * va(i) + rot_sin * vb(i)
     vb(i) = ifdef(`M4_COMPLEX',`conjg(',`(') rot_cos) * vb(i) - ifdef(`M4_COMPLEX',`conjg(',`(') rot_sin) * va(i) !'
     va(i) = tmpvec
  end do


end subroutine SUBRNAME
