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
define(`SUBRNAME',FUNC(add_to_buffer,PTYPE))
!'



!*****************************************************************
! add_to_buffer - add given vectors to rotation buffer
!*****************************************************************
subroutine SUBRNAME (ftype,avec,mvec)
  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  DTYPE (PRECISION), dimension(ftype%ncols), intent(in) :: avec
  DTYPE (PRECISION), dimension(ftype%nrhs), intent(in) :: mvec

  integer :: nn

if (verbose) then
   write(*,*) 'ADD_TO_BUFFER'
end if

  ! Because of the possible common variables
  nn = ftype%ncols - ftype%common

  ! Increase buffer counter
  ftype%nbuf = ftype%nbuf + 1

  ! Find the beginning and the end of the data row (no common variables)
  call FUNC(find_first_and_len,PTYPE) (ftype%ncols-ftype%common,1,ftype%zeroth,avec(1:nn),ftype%bst(ftype%nbuf),ftype%ble(ftype%nbuf))
    

  ! This one checks if new buffer rows will require the resizing of the regular part of rmat
  if (ftype%ble(ftype%nbuf) - ftype%bst(ftype%nbuf) + 1 > ftype%bbw) then
     ftype%bbw = ftype%ble(ftype%nbuf) - ftype%bst(ftype%nbuf) + 1
     !write(*,*) 'add_to_buffer: new bw will be ',ftype%bbw
     !write(*,*) '  currently',ftype%bw
  end if

  ! Move data rows into memory storage
  ftype%arotbuf(yind(ftype%nbuf,1,ftype%ncols):yind(ftype%nbuf,ftype%ncols,ftype%ncols)) = avec

  ftype%mrotbuf(yind(ftype%nbuf,1,ftype%nrhs):yind(ftype%nbuf,ftype%nrhs,ftype%nrhs)) = mvec

end subroutine SUBRNAME
