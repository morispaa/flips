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
define(`SUBRNAME',FUNC(make_rotations,PTYPE))

!'

subroutine SUBRNAME (ftype)
  !*****************************************************************
  ! make_rotations - perform Givens rotations to the buffer
  !*****************************************************************
  implicit none

  type(FOBTYPE) :: ftype

  integer :: prot, curnrows, lastelement

  DTYPE (PRECISION),dimension(ftype%ncols) :: lrrow
  DTYPE (PRECISION), dimension(ftype%nrhs) :: lyrow
  DTYPE (PRECISION) :: lelement

  curnrows = ftype%nrows

  if (verbose) then
     write(*,*) 'MAKE_ROTATIONS'
  end if

  ! If buffer is empty, there is nothing to do
  if (ftype%nbuf == 0) then
     if (verbose) then
        write(*,*) '...NOTHING TO ROTATE. THE BUFFER IS EMPTY'
     end if
     return
  end if

  ! Is the buffer bandwidth greater than rmat bandwidth?
  ! If so, resize rmat
  if (ftype%bbw > ftype%bw) then
     call FUNC(rresize,PTYPE) (ftype,ftype%bbw)
  end if


  ! Is R matrix already full?
  ! i.e. is the current row count greater than the number of unknowns?
  if (ftype%ncols <= curnrows) then
     ! If so, make all rotations

     call FUNC(rotate_full,PTYPE) (ftype,1,ftype%nbuf)

     ! If not, will R be full within this buffer rotation?
  elseif (ftype%ncols <= curnrows + ftype%nbuf) then
     ! If yes, divide the buffer in two parts and handle them separately
     ! Number of partial rotations
     prot = ftype%ncols - curnrows

     ! Make partial rotations for first prot buffer rows

     call FUNC(rotate_partial,PTYPE) (ftype,1,prot)



     ! Fix 081031 Make sure that the last row of R is positive
     ifdef(`M4_REAL',`
     ! We have common variables (no file storage in this case!!) 
     if (ftype%common > 0) then
        lastelement = size(ftype%cvmat)
        if (ftype%cvmat(lastelement) < 0.0) then
           ftype%cvmat(lastelement) = -ftype%cvmat(lastelement)
           call FUNC(get_row,PTYPE) ("ymat",ftype,ftype%nrhs,lyrow,ftype%ncols)
           lyrow = -lyrow
           call FUNC(put_row,PTYPE) ("ymat",ftype,ftype%nrhs,lyrow,ftype%ncols)
        end if
     else
        ! No common variables, so check the last element of rmat
        ! Note that file storage can be used in this case
        call FUNC(get_row,PTYPE) ("rmat",ftype,1,lrrow,ftype%ncols)
        if ( lrrow(1) < 0.0) then
           lrrow(1) = -lrrow(1)

           call FUNC(get_row,PTYPE) ("ymat",ftype,ftype%nrhs,lyrow,ftype%ncols)
           lyrow = -lyrow

           call FUNC(put_row,PTYPE) ("rmat",ftype,1,lrrow,ftype%ncols)
           call FUNC(put_row,PTYPE) ("ymat",ftype,ftype%nrhs,lyrow,ftype%ncols) 
        end if
     endif')

	! Fix 110418 Previous fix fixed posteriori covariance matrix calculation but totally
	! broke complex solution. I am an idiot. I promise that I'll never again try to fix bugs
	! while in bar with Lassi Roininen.
	! Fix 101117 Make sure that the last element of R is positive and real in complex case
	ifdef(`M4_COMPLEX',`
     ! We have common variables (no file storage in this case!!) 
     if (ftype%common > 0) then
        lastelement = size(ftype%cvmat)
        
        lelement = ftype%cvmat(lastelement)
        
		if (lelement /= 0) then
        	ftype%cvmat(lastelement) = ftype%cvmat(lastelement) * conjg(lelement) / abs(lelement)
        	call FUNC(get_row,PTYPE) ("ymat",ftype,ftype%nrhs,lyrow,ftype%ncols)
        	lyrow = lyrow * conjg(lelement) / abs(lelement)
        	call FUNC(put_row,PTYPE) ("ymat",ftype,ftype%nrhs,lyrow,ftype%ncols)
        endif
     else
        ! No common variables, so check the last element of rmat
        ! Note that file storage can be used in this case
        call FUNC(get_row,PTYPE) ("rmat",ftype,1,lrrow,ftype%ncols)
        lelement = lrrow(1)
        if (lelement /= 0) then
        	lrrow(1) = lrrow(1) * conjg(lelement)/abs(lelement)

        	call FUNC(get_row,PTYPE) ("ymat",ftype,ftype%nrhs,lyrow,ftype%ncols)
        	lyrow = lyrow * conjg(lelement)/abs(lelement)

        	call FUNC(put_row,PTYPE) ("rmat",ftype,1,lrrow,ftype%ncols)
        	call FUNC(put_row,PTYPE) ("ymat",ftype,ftype%nrhs,lyrow,ftype%ncols) 
        endif

     endif')
	


     ! Make full rotations for the rest of the buffer rows
     call FUNC(rotate_full,PTYPE) (ftype,prot+1,ftype%nbuf)



     ! If R is not full and will not be in this run, make partial rotations
  else

     call FUNC(rotate_partial,PTYPE) (ftype,1,ftype%nbuf)



  end if

  ! Reset buffer row counter
  ftype%nbuf = 0
  
  ! Reset bbw
  ftype%bbw = 0


end subroutine SUBRNAME
