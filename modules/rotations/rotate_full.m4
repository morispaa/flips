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
define(`SUBRNAME',FUNC(rotate_full,PTYPE))

!' 

subroutine SUBRNAME (ftype,srow,erow)
  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  integer, intent(in) :: srow,erow

  integer :: n, rlen, m, astart,aend,mstart,mend,llen, tle, tst
  integer, dimension(1) :: tmpst, tmple
  DTYPE (PRECISION) :: a, b, rot_sin
  DTYPE (PRECISION) :: rot_cos
  DTYPE (PRECISION), dimension(ftype%ncols) :: abufrow, rrow
  DTYPE (PRECISION), dimension(ftype%nrhs) :: yrow, mbufrow

  logical :: skip,swap

  if (verbose) then
     write(*,*) 'ROTATE_FULL'
  end if


  ! Rotate all columns
  do n = 1,ftype%ncols

     ! row length on this cycle
     rlen = ftype%ncols-n+1

     ! Get row from R
     rrow = 0.0
     call FUNC(get_row,PTYPE) ('rmat',ftype,size(rrow),rrow,n)

     ! Get row from Y
     call FUNC(get_row,PTYPE) ('ymat',ftype,size(yrow),yrow,n)

     do m = srow,erow

        if ( abs(ftype%arotbuf(yind(m,n,ftype%ncols))) < ftype%zeroth) then
           cycle
        else

           llen = max(ftype%rle(n),ftype%ble(m))

           astart = yind(m,n,ftype%ncols)
           aend = yind(m,ftype%ncols-ftype%common,ftype%ncols)
           mstart = yind(m,1,ftype%nrhs)
           mend = mstart + ftype%nrhs - 1

	   !abufrow = 0.0

           ! Get A buffer row (full row)
           !abufrow(1:rlen) = ftype%arotbuf(astart:aend)

           ! Get m buffer row
           mbufrow = ftype%mrotbuf(mstart:mend)

           ! Calculate the rotation coefficients
           a = rrow(1)
           b = ftype%arotbuf(astart)
           skip=.FALSE.
           swap=.FALSE.

           call FUNC(rot_coeff,PTYPE) (a,b,rot_cos,rot_sin,ftype%zeroth,skip,swap)
           ftype%fc = ftype%fc + 14

           if (skip) then
              cycle
           end if




           if (.TRUE.) then


              if (llen > 0) then
                 ! Rotate R and A
                 call FUNC(rot_vec,PTYPE) (llen-n+1,rrow(1:llen-n+1),&
                      ftype%arotbuf(astart:astart+llen-n),rot_cos,rot_sin,skip,swap)
              end if

              ! Possible common variables
              if (ftype%common > 0) then
                 call FUNC(rot_vec,PTYPE)(ftype%common,rrow(rlen-ftype%common+1:rlen),&
                      ftype%arotbuf(aend+1:aend+ftype%common),rot_cos,rot_sin,skip,swap) 
              end if

              ftype%fc = ftype%fc + 6 * (llen-n+1)

              ! Rotate Y and m
              call FUNC(rot_vec,PTYPE) (ftype%nrhs,yrow,mbufrow,rot_cos,rot_sin,skip,swap)
              ftype%fc = ftype%fc + 6*ftype%nrhs

              ! Write A and m rows back to their places
              !ftype%arotbuf(astart:aend) = abufrow(1:rlen)
              ftype%mrotbuf(mstart:mend) = mbufrow

           end if

           ! Proper row length and start checks after rotations
           if (swap) then
              tst = ftype%rst(n)
              tle = ftype%rle(n)
              ftype%rst(n) = ftype%bst(m)
              ftype%rle(n) = ftype%ble(m)
              ftype%bst(m) = tst
              ftype%ble(m) = tle

           else if (.NOT. skip) then
              ftype%ble(m) = llen
              ftype%bst(m) = n+1
              ftype%rle(n) = llen

           end if

           if (n>ftype%ncols-ftype%common) then
              ftype%rle(n) = 0
           end if

        end if



     end do

     call FUNC(put_row,PTYPE) ('rmat',ftype,size(rrow),rrow,n)
     call FUNC(put_row,PTYPE) ('ymat',ftype,size(yrow),yrow,n)

  end do




end subroutine SUBRNAME
