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
define(`SUBRNAME',FUNC(flipsadd,PTYPE))

!'



!*****************************************************************
! flips_add - add data to system
!*****************************************************************
subroutine SUBRNAME (ftype,n,arows,mrows,errors,force_rotations)
  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  integer, intent(in) :: n
  DTYPE (PRECISION), dimension(ftype%ncols*n), intent(in) :: arows
  DTYPE (PRECISION), dimension(ftype%nrhs*n), intent(in) :: mrows
  real (PRECISION), dimension(n), optional, intent(in) :: errors
  logical, intent(in), optional :: force_rotations

  logical :: errors_exist, f_rot
  integer :: i, ncols, nrhs
  DTYPE (PRECISION), dimension(ftype%ncols) :: a_aux
  DTYPE (PRECISION), dimension(ftype%nrhs) :: m_aux

  if(present(force_rotations)) then
     f_rot = force_rotations
  else
     f_rot = .FALSE.
  end if

  ncols = ftype%ncols
  nrhs = ftype%nrhs

  if (present(errors)) then
     errors_exist = .TRUE.
  else
     errors_exist = .FALSE.
  end if

  ! Clear solution-exists-flag
  ftype%solexists = .FALSE.
  ! Clear R-inverted-flag
  ftype%rinverted = .FALSE.
  ! Clear covariance-exists-flag
  ftype%covexists = .FALSE.

  do i = 1,n

     ! If errors are given, scale and update residual calculation vector
     if (errors_exist) then
        a_aux = arows(yind(i,1,ncols):yind(i,ncols,ncols))/sqrt(errors(i))
        m_aux = mrows(yind(i,1,nrhs):yind(i,nrhs,nrhs))/sqrt(errors(i))
        
        ! Update tmpres for residual calculation 
        ftype%tmpres = ftype%tmpres + real(m_aux,PRECISION)**2 ifdef(`M4_COMPLEX',`+ aimag(m_aux)**2')

        ! Update flop count
        ftype%fc = ftype%fc + ncols + 3*nrhs
        !end if
     else
        a_aux = arows(yind(i,1,ncols):yind(i,ncols,ncols))
        m_aux = mrows(yind(i,1,nrhs):yind(i,nrhs,nrhs))
        
        ! Update tmpres for residual calculation
        ftype%tmpres = ftype%tmpres + real(m_aux,PRECISION)**2 ifdef(`M4_COMPLEX',`+ aimag(m_aux)**2')

        ! Update flop count
        ftype%fc = ftype%fc + 2*nrhs
        !end if
     end if



     ! Send data row to buffer (buffer counter increased in add_to_buffer routine)
     call FUNC(add_to_buffer,PTYPE) (ftype,a_aux,m_aux)


     ! Horrible bubble gum fix!!
     ! Force rotations when we hit the common variables barrier
     !if (ftype%nrows == ftype%ncols-ftype%common-1) then
     !   call FUNC(make_rotations,PTYPE) (ftype)
     !   ftype%nbuf = 0
     !   ftype%bst=0
     !   ftype%ble=0
     !   ftype%bbw = 0
     !end if

     ! If buffer full, make rotations and zero buffer counter
     if (ftype%nbuf == ftype%nrotbuf) then
        call FUNC(make_rotations,PTYPE) (ftype)
        ftype%nbuf = 0
        ftype%bst=0
        ftype%ble=0
        ftype%bbw = 0

     end if


!     ! Force rotations when we hit the common variables
!     if (ftype%common > 0) then
!        if (ftype%nrows == ftype%ncols - ftype%common) then
!           call FUNC(make_rotations,PTYPE) (ftype)
!           ftype%nbuf = 0
!           ftype%bst=0
!           ftype%ble=0
!           ftype%bbw = 0
!        end if
!     end if

  end do

  ! Force last rotations if force_rotations flag is up (added 070320 due to problems with interfaces)
  if (f_rot) then
     if (ftype%nbuf /= 0) then
        call FUNC(make_rotations,PTYPE) (ftype)
        ftype%nbuf = 0
        ftype%bst=0
        ftype%ble=0
        ftype%bbw = 0
     end if
  end if



end subroutine SUBRNAME
