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
define(`SUBRNAME',FUNC(calculate_covariance,PTYPE))

!'

subroutine SUBRNAME (ftype,full)
  implicit none

  type(FOBTYPE) :: ftype
  logical, optional :: full

  integer :: ncol,nrhs,i,j
  DTYPE (PRECISION), dimension(ftype%ncols) :: irow,jrow,covrow
  integer :: astat
  logical :: fopen


  ! Check requirements
  if (.NOT. ftype%solexists) then
     write(0,*) 'flips WARNING: Trying to calculate covariance when solution is not calculated!' 
  end if

  ncol = ftype%ncols
  nrhs = ftype%nrhs

  ! Check optional argument (full matrix is calculated only if wanted)
  if (present(full)) then
     ftype%fullcov = full
  else
     ftype%fullcov = .FALSE.
  end if


  ! Allocate/open covariance matrix storage
  if (ftype%use_files) then
     call FUNC(open_cov_file,PTYPE) (ftype)
  else
     if (ftype%fullcov) then
        if (allocated(ftype%cmat)) then
           deallocate(ftype%cmat)
        end if
        allocate(ftype%cmat(ncol*(ncol+1)/2),STAT=astat)
     else
        if (allocated(ftype%cmat)) then
           deallocate(ftype%cmat)
        end if
        allocate(ftype%cmat(ncol),STAT=astat)
     end if
  end if

  ! Invert R matrix
  call FUNC(invert_r,PTYPE) (ftype)

  ! If using files, open invr file as direct access file
  if (ftype%use_files) then
     call FUNC(open_invr_file_dir,PTYPE) (ftype)
  end if

  ! Calculate iR x iR^T (or only diagonal) (TRANSPOSE)
  if (ftype%fullcov) then
     ! Full multiplication
     do i = 1,ncol
        ! Get i:th iR row
        call FUNC(get_ir_row,PTYPE) (ftype,i,irow)
        do j = i,ncol
           ! Get j:th iR row
           call FUNC(get_ir_row,PTYPE) (ftype,j,jrow)
           ! Multiply
           covrow(j-i+1) = sum(irow(j-i+1:ncol-i+1) * ifdef(`M4_COMPLEX',`conjg') (jrow(1:ncol-j+1))  )
        end do
        ! Store covrow
        call FUNC(put_row,PTYPE) ('cova',ftype,size(covrow),covrow,i)
     end do
  else
     ! Only diagonal
     do i = 1,ncol
        ! Get i:th row of iR
        call FUNC(get_ir_row,PTYPE) (ftype,i,irow)

        covrow(i) = sum(irow(1:ncol-i+1)* ifdef(`M4_COMPLEX',`conjg') (irow(1:ncol-i+1)))

     end do

     ! Store covrow
     call FUNC(put_row,PTYPE) ('cova',ftype,size(covrow),covrow,1)
  end if

  ! If using files close invr file
  if (ftype%use_files) then
     close(ftype%idnum*100+15)
  end if

  ! All done, raise flag
  ftype%covexists = .TRUE.

end subroutine SUBRNAME
