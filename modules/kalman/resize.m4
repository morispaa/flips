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
  define(`SUBRNAME',FUNC(resize,PTYPE))
  !'

  subroutine SUBRNAME (nfob,ofob,ns,nadd,nrem,nremcv,re,id,bs)
    implicit none

    type(FOBTYPE), intent(inout) :: nfob,ofob
    integer, intent(in) :: ns, nadd, nrem, nremcv
    logical, dimension(ofob%ncols) :: re
    integer, intent(in) :: id, bs

    type(FOBTYPE) :: wrkfob
    DTYPE (PRECISION), dimension(:), allocatable :: nrrow,nmrow,orrow,omrow,rwrk
    real (PRECISION), dimension(1) :: dummyerror
    integer, dimension(ofob%ncols) :: permvec
    integer :: i, fa,tr, n, db1(1), db2(1), rl, rrr,nnn

    dummyerror = 1.0

    ! Bughunt
    if (verbose) then
       write(*,*) 'FLIPS resize----------------'
       write(*,*) '  newsize',ns
       write(*,*) '  add',nadd
       write(*,*) '  remove',nrem
       write(*,*) '  remove cv',nremcv
       write(*,*) '----------------------------'
    end if



    ! Initialize the new flips data type and work data type
    if (id==-1) then
       ! New problem will have ofob%common-nremcv+nadd common variables (new variables are automatically common)...
       call FUNC(flipsinit,PTYPE) (nfob,ns,ofob%nrhs,bandwidth=1,common=ofob%common-nremcv+nadd,buffersize=bs)
       ! ...and wrkfob will have ofob%common-nremcv common variables
       call FUNC(flipsinit,PTYPE) (wrkfob,ofob%ncols,ofob%nrhs,bandwidth=1,common=ofob%common-nremcv,buffersize=ofob%ncols+nadd+1)

    else
       call FUNC(flipsinit,PTYPE) (nfob,ns,ofob%nrhs,idnum=id,buffersize=bs)
       call FUNC(flipsinit,PTYPE) (wrkfob,ofob%ncols,ofob%nrhs,idnum=9999,buffersize=bs)
    end if

    ! Allocate working vectors
    allocate(orrow(ofob%ncols),rwrk(ofob%ncols),omrow(ofob%nrhs),nrrow(ns),nmrow(ofob%nrhs))

    ! Create permutation vector
    fa = 1
    tr = nrem + 1
    do i = 1,ofob%ncols
       if(re(i)) then
          permvec(fa) = i
          fa = fa + 1
       else
          permvec(tr) = i
          tr = tr + 1
       end if
    end do

    ! Empty the rotation buffer of the old object
    if (ofob%nbuf > 0) then
       call FUNC(make_rotations,PTYPE) (ofob)
    end if

    old_r_rows: do n = 1,ofob%ncols


       ! Take rows from the old problem in same order as the permutation vector
       ! This takes care of possible zero columns

       rrr = permvec(n)

       ! Get R row from the old object
       orrow = 0.0
       call FUNC(get_row,PTYPE) ('rmat',ofob,size(orrow),orrow,rrr)
       ! Get measurement row from the old object
       call FUNC(get_row,PTYPE) ('ymat',ofob,size(omrow,1),omrow,rrr)

       ! Shift orrow to the right n-1 steps and pad the first elements
       ! with zeros
       rwrk = orrow
       orrow = 0.0
       orrow(rrr:ofob%ncols) = rwrk(1:ofob%ncols-rrr+1)

       ! Permutate orrow into working vector
       rwrk = orrow(permvec)

       call FUNC(find_first_and_len,PTYPE) (ofob%ncols,1,ofob%zeroth,rwrk(1:ofob%ncols),db1,db2)

       ! Feed the new row and the old measurement row into temporary flips object
       ! flips_add takes care of the calculation of row lengths
       ! The remaining ofob%common-nremcv common variables are still in the last places (indexwise).
       call FUNC(flipsadd,PTYPE) (wrkfob,1,rwrk,omrow)



    end do old_r_rows

      ! write(*,*) 'wrkfob nrows',wrkfob%nrows

    ! Make the last rotations
    if (wrkfob%nbuf > 0 ) then
       call FUNC(make_rotations,PTYPE) (wrkfob)
    end if

    ! Now move remaining rows into the new problem nfob

    nnn = 0
    do n = nrem+1,ofob%ncols

       ! Get n'th R row form ofob

       ! Row length
       rl = ofob%ncols - n + 1

       rwrk = 0.0
       nrrow = 0.0
       call FUNC(get_row,PTYPE) ('rmat',wrkfob,rl,rwrk(1:rl),n)

       ! Get n'th Y row from ofob
       call FUNC(get_row,PTYPE) ('ymat',wrkfob,ofob%nrhs,omrow,n)

       ! The rows are already in upper triangular format so no rotations will occur.
       ! However, to make things easier, we feed them in as new rows. This is just to keep 
       ! all internal variables in order.

       nnn = nnn + 1
       nrrow(nnn:nnn+rl-1) = rwrk(1:rl)

       call FUNC(flipsadd,PTYPE) (nfob,1,nrrow,omrow)

    end do

    !  write(*,*) 'nfob nrows',nfob%nrows
    !  write(*,*) 'nfob nbuf',nfob%nbuf


       !if (nfob%nbuf>0) then
          ! Just to be sure, rotate nfob
          call FUNC(make_rotations,PTYPE) (nfob)
       !end if

     !  write(*,*) 'nfob nrows',nfob%nrows

    ! That's it!
    ! Destroy temporary object
    call FUNC(flipskill,PTYPE) (wrkfob,.FALSE.)

  end subroutine SUBRNAME


