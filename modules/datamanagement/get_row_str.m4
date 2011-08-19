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
define(`SUBRNAME',FUNC(get_row,PTYPE))
!'

subroutine SUBRNAME (mtype,fob,vecsize,wrkvec,n)


  implicit none

  character(len=4), intent(in) :: mtype
  type(FOBTYPE), intent(inout) :: fob
  integer, intent(in) :: vecsize
  DTYPE (PRECISION), dimension(vecsize), intent(out) :: wrkvec
  integer :: n, l, st

  integer :: rowlen
  integer :: fileunit, startpos, endpos, nn

  if (verbose) then
     write(*,*) 'GET_ROW',vecsize,n
  end if


  wrkvec = 0.0

  if (fob%use_files) then

     select case(mtype)

     case ('rmat')
        !rowlen = fob%rle(n) - n + 1
        rowlen = fob%ncols-n+1
        fileunit = 100*fob%idnum + 11
        startpos = rind(n,n,fob%ncols)

     case ('invr')
        rowlen = fob%ncols-n+1
        fileunit = 100*fob%idnum + 15
        startpos = rind(n,n,fob%ncols)

     case ('cova')
        rowlen = fob%ncols-n+1
        fileunit = 100*fob%idnum + 14
        startpos = rind(n,n,fob%ncols)

     case ('ymat')
        rowlen = fob%nrhs
        fileunit = 100*fob%idnum + 12
        startpos = yind(n,1,fob%nrhs)

     case ('solu')
        rowlen = fob%nrhs
        fileunit = 100*fob%idnum + 13
        startpos = yind(n,1,fob%nrhs)

     case default
        write(0,*) 'FLIPS error: get_row_str: Unknown matrix type ',mtype
        stop

     end select

ifdef(`F2003_STREAM',
     `startpos = 1 + (startpos - 1) * fob%rl
     read(fileunit,POS=startpos) wrkvec(1:rowlen)'
)


ifdef(`DIRECT',
     `do l = 0,rowlen-1
        read(fileunit,REC=startpos+l) wrkvec(l+1)
     end do'
)

  else

     select case(mtype)

     case ('rmat')

        ! In case of common variables
        nn = fob%ncols - fob%common

        ! both regular and cv parts
        if (n <= nn) then
           ! Regular part of R
           startpos = rind(n,n,nn,fob%bw)
           if (fob%bw < nn) then
              if (n <= nn - fob%bw) then
                 rowlen = fob%bw
              else
                 rowlen = nn - n + 1
              end if
           else
              rowlen = nn-n+1
           end if
           endpos = rind(n,n+rowlen-1,nn,fob%bw)
           wrkvec = 0.0
           wrkvec(1:rowlen) = fob%rmat(startpos:endpos) 

           if (fob%common > 0 ) then
              ! Common variables
              rowlen = nn-n+1
              startpos = rind(n,n,fob%ncols,fob%common)
              endpos = startpos + fob%common - 1
              wrkvec(rowlen+1:rowlen+fob%common) = fob%cvmat(startpos:endpos)
           end if

           ! Only common variables part
        else
           startpos = rind(n,n,fob%ncols,fob%common)
           rowlen = fob%ncols - n + 1
           endpos = startpos + rowlen - 1
           wrkvec(1:rowlen) = fob%cvmat(startpos:endpos)
        end if
           


     case ('invr')
        rowlen = fob%ncols-n+1
        startpos = rind(n,n,fob%ncols)
        endpos = rind(n,fob%ncols,fob%ncols)
        wrkvec(1:rowlen) = fob%invrmat(startpos:endpos) 

     case ('cova')
        rowlen = fob%ncols-n+1
        startpos = rind(n,n,fob%ncols)
        endpos = rind(n,fob%ncols,fob%ncols)
        wrkvec(1:rowlen) = fob%cmat(startpos:endpos) 

     case ('ymat')
        rowlen = fob%nrhs
        startpos = yind(n,1,rowlen)
        endpos = yind(n,rowlen,rowlen)
        wrkvec(1:rowlen) = fob%ymat(startpos:endpos) 

     case ('solu')
        rowlen = fob%nrhs
        startpos = yind(n,1,rowlen)
        endpos = yind(n,rowlen,rowlen)
        wrkvec(1:rowlen) = fob%solmat(startpos:endpos)        

     case default
        write(0,*) 'FLIPS error: get_row_str: Unknown matrix type ',mtype
        stop

     end select

  end if



end subroutine SUBRNAME
