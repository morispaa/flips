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
define(`SUBRNAME',FUNC(flipskill,PTYPE))
!'

!*****************************************************************
! flips_kill - deallocate flips data type
!*****************************************************************
subroutine SUBRNAME (ftype,keepfiles)
  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  logical, optional :: keepfiles

  logical :: fexists, kf

  ! Deallocate buffers
  deallocate(ftype%arotbuf,ftype%mrotbuf,ftype%residual,ftype%tmpres)

  deallocate(ftype%rst,ftype%rle,ftype%bst,ftype%ble)


  if (ftype%use_files) then
     ! File storage. Keep files?
     if (.NOT. present(keepfiles)) then
        kf = .TRUE.
     else
        kf = keepfiles
     end if

     if (.NOT. kf) then
        ! Close and delete files
        close(UNIT=100*ftype%idnum+11,STATUS='DELETE')
        close(UNIT=100*ftype%idnum+12,STATUS='DELETE')
        close(UNIT=100*ftype%idnum+13,STATUS='DELETE')
        inquire (UNIT=100*ftype%idnum+14,OPENED=fexists)
        if (fexists) then
           close(UNIT=100*ftype%idnum+14,STATUS='DELETE')
        end if
        inquire (UNIT=100*ftype%idnum+15,OPENED=fexists)
        if (fexists) then
           close(UNIT=100*ftype%idnum+15,STATUS='DELETE')
        end if
     else
        ! Just close files
        close(UNIT=100*ftype%idnum+11)
        close(UNIT=100*ftype%idnum+12)
        close(UNIT=100*ftype%idnum+13)
        inquire (UNIT=100*ftype%idnum+14,OPENED=fexists)
        if (fexists) then
           close(UNIT=100*ftype%idnum+14)
        end if
        inquire (UNIT=100*ftype%idnum+15,OPENED=fexists)
        if (fexists) then
           close(UNIT=100*ftype%idnum+15)
        end if
     end if

  else
     ! Memory storage. Deallocate everything
     deallocate(ftype%rmat,ftype%ymat,ftype%solmat)
     if (allocated(ftype%cmat)) then
        deallocate(ftype%cmat)
     end if
     if (allocated(ftype%invrmat)) then
        deallocate(ftype%invrmat)
     end if
     if (allocated(ftype%cvmat)) then
        deallocate(ftype%cvmat)
     end if

  end if

  if (verbose) then
     ! Print info
     write(6,*) '**********************************'
     write(6,*) '* flips data type deallocated'
     write(6,*) '**********************************'
  end if

end subroutine SUBRNAME
