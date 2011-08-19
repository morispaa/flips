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
  define(`SUBRNAME',FUNC(rotate_partial,PTYPE))
  !'



  subroutine SUBRNAME (ftype,srow,erow)
    implicit none

    type(FOBTYPE), intent(inout) :: ftype
    integer, intent(in) :: srow,erow

    integer :: n,m, nrows, rlen, astart, aend, mstart, mend, llen, tst, tle
    integer, dimension(1) ::  tmpst,tmple
    DTYPE (PRECISION), dimension(ftype%ncols) :: rrow
    !DTYPE (PRECISION), dimension(:), pointer :: abufrow, cvarow
    DTYPE (PRECISION), dimension(ftype%nrhs) :: yrow, mbufrow
    DTYPE (PRECISION) :: a,b,rot_sin
    DTYPE (PRECISION) :: rot_cos
    logical :: skip,swap

    nrows = ftype%nrows

    if (verbose) then
       write(*,*) 'ROTATE_PARTIAL',nrows,srow,erow
    end if




    ! First rotate nrows first columns of the buffer
    ! with the rows in R matrix
    rotate_r_and_buffer: do n = 1,nrows

       ! row length on this cycle
       rlen = ftype%ncols-n+1

       ! Get row from R
       rrow = 0.0
       call FUNC(get_row,PTYPE) ('rmat',ftype,size(rrow),rrow,n)

       ! Get row from Y
       call FUNC(get_row,PTYPE) ('ymat',ftype,size(yrow),yrow,n)

       ! Loop through buffer rows
       buffer_row: do m = srow,erow

          ! If n'th element of buffer row is non-zero, rotate.
          ! Otherwise skip this rotation
          if ( abs(ftype%arotbuf(yind(m,n,ftype%ncols))) < ftype%zeroth ) then 
             cycle
          else

             ! Get the length of the rotation
             llen = max(ftype%rle(n),ftype%ble(m))

             ! Get the rotation start and end indices for the buffer row
             ! (Regular part)
             ! Length is llen - n + 1
             astart = yind(m,n,ftype%ncols)
             aend = yind(m,ftype%ncols-ftype%common,ftype%ncols)
             mstart = yind(m,1,ftype%nrhs)
             mend = mstart + ftype%nrhs - 1

             !abufrow = 0.0

             ! Get A buffer row in parts
             !abufrow => ftype%arotbuf(astart:aend)
             !if (ftype%common>0) then
             !   cvarow => ftype%arotbuf(aend+1:aend+ftype%common)
             !end if

             ! Get m buffer row
             mbufrow = ftype%mrotbuf(mstart:mend)

             ! Calculate the rotation coefficients
             a = rrow(1)
             b = ftype%arotbuf(astart)
             skip = .FALSE.
             swap = .FALSE.
             call FUNC(rot_coeff,PTYPE) (a,b,rot_cos,rot_sin,ftype%zeroth,skip,swap)

             !write(*,*) 'rot_cos',rot_cos
             !write(*,*) 'rot_sin',rot_sin
             !write(*,*) 'skip',skip
             !write(*,*) 'swap',swap

             ! Update flop count (FIX THIS! Not accurate anymore)
             ftype%fc = ftype%fc + 14

             ! If flagged to be skipped, cycle to the nex row
             if (skip) then
                cycle
             end if


             ! Make the rotations

             ! Measurement vectors

             ! Rotate Y and m
             call FUNC(rot_vec,PTYPE) (ftype%nrhs,yrow,mbufrow,rot_cos,rot_sin,skip,swap)

             ! Update flop count (not accurate)
             ftype%fc = ftype%fc + 6*ftype%nrhs

             ! Write mbufrow back
             ftype%mrotbuf(mstart:mend) = mbufrow

             ! Both regular and common variable part
             if (.TRUE.) then

                !if (llen == 0) then
                !   write(*,*) 'llen is zero!'
                !end if

                if (llen > 0) then
                   ! I can be zero, if there are only common variables...
                   ! Rotate R row and buffer row (regular part)
                   ! HOX! 100323
                   call FUNC(rot_vec,PTYPE) (llen-n+1,rrow(1:llen-n+1),ftype%arotbuf(astart:astart+llen-n),&
                        rot_cos,rot_sin,skip,swap)
                end if

                ! If common variables, rotate them also
                if (ftype%common > 0 ) then
                   call FUNC(rot_vec,PTYPE) (ftype%common,rrow(rlen-ftype%common+1:rlen),&
                        ftype%arotbuf(aend+1:aend+ftype%common),rot_cos,rot_sin,skip,swap)
                end if

                ! Update flop count (not accurate for common variables)
                ftype%fc = ftype%fc + 6 * (llen-n+1)

                !else

                ! if (llen /= 0) then
                !    write(*,*) 'llen is not zero!'
                !  end if
                ! Only common variables part
                ! call FUNC(rot_vec,PTYPE) (ftype%common,rrow(rlen-ftype%common+1:rlen),ftype%arotbuf(aend+1:aend+ftype%common),rot_cos,rot_sin,skip,swap)
             end if


             ! If the rows are swapped, swap also the row start and end vectors
             if (swap) then

                tst = ftype%rst(n)
                tle = ftype%rle(n)
                ftype%rst(n) = ftype%bst(m)
                ftype%rle(n) = ftype%ble(m)
                ftype%bst(m) = tst
                ftype%ble(m) = tle
             else if (.NOT. skip) then
                ! Otherwise, update the row start and end vectors
                ! Row ends will be llen for both R row and buffer row
                ftype%rle(n) = llen
                ftype%ble(m) = llen




                ! Row start for R row will be n (as it was before)
                ! and for buffer row n+1 (since the rotation zeroes he first element)

                ftype%bst(m) = n+1

             end if

             if (n>ftype%ncols-ftype%common) then
                ftype%rle(n) = 0
             end if


             ! Write A and m rows back to their places
             !ftype%arotbuf(astart:aend) = abufrow(1:rlen)

             ! Nullify pointers
             !nullify(abufrow)
             !if (ftype%common > 0 ) then
             !   nullify(cvarow)
             !end if


          end if



       end do buffer_row


       ! Write R and Y rows back

       call FUNC(put_row,PTYPE) ('rmat',ftype,size(rrow),rrow,n)
       call FUNC(put_row,PTYPE) ('ymat',ftype,size(yrow),yrow,n)

    end do rotate_r_and_buffer


    ! Buffer rows are now rotated with existing R rows
    ! Next we rotate buffer rows together, and move rotated rows
    ! to R matrix


    ! Now if srow==erow we are done rotating! Move the only row into R (and Y).
    ! Also, increase nrows by one.
    if (srow==erow) then

       ftype%nrows = ftype%nrows + 1
       rrow = 0.0
       yrow = 0.0

       ! get R row
       rrow(1:ftype%ncols-ftype%nrows+1) = ftype%arotbuf(yind(srow,&
            ftype%nrows,ftype%ncols):yind(srow,ftype%ncols,ftype%ncols))

       call FUNC(put_row,PTYPE) ('rmat',ftype,size(rrow),rrow,ftype%nrows)

       ! get Y row
       yrow = ftype%mrotbuf(yind(srow,1,ftype%nrhs):yind(srow,ftype%nrhs,&
            ftype%nrhs))
       call FUNC(put_row,PTYPE) ('ymat',ftype,size(yrow),yrow,ftype%nrows)

       ! Update row start and end vectors
       ! NB: Put rst as ftype%nrows regardless of the real situation!!
       ! rst(n) should be n always!! 
       ftype%rst(ftype%nrows) = ftype%nrows
       ftype%rle(ftype%nrows) = ftype%ble(srow)

    else
       ! Otherwise, rotate buffer rows together
       ! starting from the column nrows+1
       ! (columns from 1 to nrows should be zeroes because of the previous 
       ! rotations with R matrix rows)

       ! Take care of the common variables here!

       buffer_row_1: do n = srow,(erow-1)

          ! increase nrows counter
          ftype%nrows = ftype%nrows+1
          nrows = ftype%nrows

          ! rotate column nrows, row length in this cycle
          rlen = ftype%ncols-nrows+1

          ! Get A and m rows
          rrow = 0.0
          rrow(1:rlen) = ftype%arotbuf(yind(n,nrows,ftype%ncols):yind(n,&
               ftype%ncols,ftype%ncols))
          yrow = ftype%mrotbuf(yind(n,1,ftype%nrhs):yind(n,ftype%nrhs,&
               ftype%nrhs))

          buffer_row_2: do m =  (n+1),erow

             ! If nrows'th element of m'th buffer row is non-zero, rotate
             ! Otherwise skip this rotation

             if (abs(ftype%arotbuf(yind(m,nrows,ftype%ncols))) < ftype%zeroth) then

                ! Fix 081030 if rrow(1) is negative, change signs
                ! so that we get the diagonal of R positive all the time!
                ifdef(`M4_REAL',`
                if (rrow(1) .lt. 0.0 ) then
                   rrow = -rrow
                   yrow = -yrow
                end if')
                cycle
             else

                ! Length of the rotation
                llen = max(ftype%ble(n),ftype%ble(m))

                ! Start and end indices for the buffer row 2
                astart = yind(m,nrows,ftype%ncols)
                aend = yind(m,ftype%ncols-ftype%common,ftype%ncols)
                mstart = yind(m,1,ftype%nrhs)
                mend = mstart + ftype%nrhs - 1

                !abufrow = 0.0

                ! get A and m rows (whole A row)
                !abufrow => ftype%arotbuf(astart:aend)
                !if (ftype%common > 0 ) then
                !   cvarow => ftype%arotbuf(aend+1:aend+ftype%common)
                !end if

                mbufrow = ftype%mrotbuf(mstart:mend)

                ! Calculate rotation coefficients
                a = rrow(1)
                b = ftype%arotbuf(astart)
                skip = .FALSE.
                swap = .FALSE.
                call FUNC(rot_coeff,PTYPE) (a,b,rot_cos,rot_sin,ftype%zeroth,skip,swap)

                ! Update flop count (FIX THIS! Not accurate anymore)
                ftype%fc = ftype%fc + 14

                if (skip) then
                   cycle
                end if

                ! Rotate measurement vectors
                ! Rotate Y and m
                call FUNC(rot_vec,PTYPE) (ftype%nrhs,yrow,mbufrow,rot_cos,rot_sin,skip,swap)
                ftype%mrotbuf(mstart:mend) = mbufrow


                ! Make the rotations
                ! Both regular and cv parts
                if (.TRUE.) then

                   !if (llen == 0) then
                   !   write(*,*) 'llen is zero!'
                   !end if

                   if (llen > 0) then
                      ! Rotate R row and buffer row 
                      ! HOX! 100323
                      call FUNC(rot_vec,PTYPE) (llen-nrows+1,rrow(1:llen-nrows+1),&
                           ftype%arotbuf(astart:astart+llen-nrows),rot_cos,rot_sin,skip,swap)
                   end if

                   ! If common variables, rotate them also
                   if (ftype%common > 0 ) then
                      call FUNC(rot_vec,PTYPE) (ftype%common,rrow(rlen-ftype%common+1:rlen),&
                           ftype%arotbuf(aend+1:aend+ftype%common),rot_cos,rot_sin,skip,swap) 
                   end if

                   !else


                   !if (llen /= 0) then
                   !   write(*,*) 'llen is not zero!'
                   !end if

                   ! Only common variable part
                   !   call FUNC(rot_vec,PTYPE) (ftype%common,rrow(rlen-ftype%common+1:rlen),ftype%arotbuf(aend+1:aend+ftype%common),rot_cos,rot_sin,skip,swap) 
                   !end if



                   ! Update flop counts (not accurate anymore!)
                   ftype%fc = ftype%fc + 6 * (llen-n+1)
                   ftype%fc = ftype%fc + 6*ftype%nrhs     

                   ! If the rows are swapped, swap also the row start and end vectors
                   if (swap) then
                      tst = ftype%bst(n)
                      tle = ftype%ble(n)
                      ftype%bst(n) = ftype%bst(m)
                      ftype%ble(n) = ftype%ble(m)
                      ftype%bst(m) = tst
                      ftype%ble(m) = tle
                   else if (.NOT. skip) then
                      ! Otherwise, update the row start and end vectors
                      ! Row ends will be llen for both R row and buffer row
                      ftype%ble(n) = llen
                      ftype%bst(n) = nrows
                      ftype%ble(m) = llen
                      ftype%bst(m) = nrows+1
                   end if

                   if (n>ftype%ncols-ftype%common) then
                      ftype%ble(n) = 0
                   end if



                   ! Write back
                   !ftype%arotbuf(astart:aend) = abufrow(1:rlen)                   

                   ! Nullify pointers
                   !nullify(abufrow)
                   !if (ftype%common > 0 ) then
                   !   nullify(cvarow)
                   !end if

                end if

             end if


          end do buffer_row_2

          ! Write buffer row 1 and m row into R and Y and
          ! update R row start and end vectors

          call FUNC(put_row,PTYPE) ('rmat',ftype,size(rrow),rrow,nrows)
          call FUNC(put_row,PTYPE) ('ymat',ftype,size(yrow),yrow,nrows)
          ftype%rst(nrows) = ftype%bst(n)
          ftype%rle(nrows) = ftype%ble(n)

       end do buffer_row_1


       ! Finally, move the last row to R and Y and once again increase nrows
       ! and update R row start and end vectors
       ftype%nrows = ftype%nrows + 1

       rrow(1:ftype%ncols-ftype%nrows+1) = ftype%arotbuf(yind(erow,&
            ftype%nrows,ftype%ncols):yind(erow,ftype%ncols,ftype%ncols))

       call FUNC(put_row,PTYPE) ('rmat',ftype,size(rrow),rrow,ftype%nrows)

       yrow = ftype%mrotbuf(yind(erow,1,ftype%nrhs):yind(erow,ftype%nrhs,&
            ftype%nrhs))

       call FUNC(put_row,PTYPE) ('ymat',ftype,size(yrow),yrow,ftype%nrows) 

       ftype%rst(ftype%nrows) = ftype%bst(erow)
       ftype%rle(ftype%nrows) = ftype%ble(erow)


    end if

  end subroutine SUBRNAME
