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
define(`SUBRNAME',FUNC(invert_r,PTYPE))
!'

subroutine SUBRNAME (ftype)
  ! Inverts upper triangular R
  ! Writes the TRANSPOSE of the inverse into new vector/file
  ! LOWER TRIANGULAR MATRIX!!

  implicit none

  type(FOBTYPE), intent(inout) :: ftype

  DTYPE (PRECISION), dimension(ftype%ncols) :: workvec
  integer :: n,i,rowlen
  DTYPE (PRECISION), dimension(ftype%ncols) :: initvec



  if (ftype%use_files) then

     ! Initialize invr file (fill with zeros)
     write(ftype%irfile,'("IR_",I6.6,".dat")') ftype%idnum
     initvec = 0.0
     ! Open invr file in stream mode 
     call FUNC(open_invr_file,PTYPE) (ftype)

     do i = 1,ftype%ncols
        rowlen=ftype%ncols+1-i
        call FUNC(put_row,PTYPE) ('invr',ftype,rowlen,initvec(1:rowlen),i)
     end do



  else
     ! Allocate invrmat
     if (allocated(ftype%invrmat)) deallocate(ftype%invrmat)
     allocate(ftype%invrmat(ftype%ncols*(ftype%ncols+1)/2))
  end if

  do n = ftype%ncols,1,-1

     ! invert nth column
     call FUNC(inv_col,PTYPE) (ftype,workvec,n)


     ! Put column in its place (TRANSPOSED)
     call FUNC(put_ir_col,PTYPE) (ftype,workvec,n)


  end do


  ! Update status flag
  if (ftype%rinverted) then
     ftype%rinverted = .FALSE.
  else
     ftype%rinverted = .TRUE.
  end if

  ! close invr file
  if (ftype%use_files) close(ftype%idnum*100+15)


end subroutine SUBRNAME
