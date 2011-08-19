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
define(`SUBRNAME',FUNC(flipsresize,PTYPE))
!'

subroutine SUBRNAME (nfob,ofob,newsize,remove,idnum,buffersize)
  ! FLIPS_RESIZE - copy, resize, marginalize unknowns
  implicit none

  type(FOBTYPE) :: nfob
  type(FOBTYPE) :: ofob
  integer, optional :: newsize
  logical, dimension(ofob%ncols), optional :: remove
  integer, optional :: idnum, buffersize

  logical :: nex,rex
  integer :: ns, os, nrem, nadd, id, bs,nremcv
  logical, dimension(ofob%ncols) :: re

  nex = .FALSE.
  rex = .FALSE.
  nrem = 0
  nremcv = 0
  nadd = 0
  ns = 0
  re=.FALSE.
  os = ofob%ncols

  ! Check arguments
  ! Presence of optional arguments
  if (present(newsize)) then
     ns = newsize
     nex = .TRUE.
  end if
  if (present(remove)) then
     re = remove
     rex = .TRUE.
     nrem = count(re)
     
     ! Are we removing any of the common variables?
     if (ofob%common > 0) then
        nremcv = count(re(ofob%ncols-ofob%common+1:ofob%ncols))
     end if

  end if
  if (present(idnum)) then
     id = idnum
     if (ofob%common > 0) then
        write(*,*) 'FLIPS warning: flips_resize: common variables are not implemented'
        write(*,*) 'for file storage! Using memory storage instead!'
        id = -1
     end if
  else
     id = -1
  end if

  if (present(buffersize)) then
     bs = buffersize
  else
     bs = defbufsize
  end if


  ! If both optional arguments are present, check that 
  ! they are compatible

  if (nex .AND. rex) then
     if (os-nrem > ns) then
        write(0,*) 'flips ERROR: flips_resize: newsize argument is not compatible with remove argument.'
        stop
     end if
     ! Both are present and compatible, so we will add new unknowns and
     ! remove some of the old ones

     ! Number of new unknowns:
     nadd = ns - (os-nrem)

  end if

  ! If only one of the optional arguments is present, create the other one.
  if (nex .AND. (.NOT.rex)) then
     ! Remove or add unknowns?
     if (ns<os) then
        nrem = os-ns
        re(1:nrem)=.TRUE.
        nadd = 0
     else
        nadd = ns-os
        nrem = 0
     end if
  end if

  if (rex .AND. (.NOT.nex)) then
     ns = os-nrem
  end if


  ! Check that nfob is not initialized
  if (allocated(nfob%mrotbuf)) then
     write(0,*) 'flips ERROR: flips_resize: flips data type is already initialized!'
     stop
  end if

  ! Do the resizing
  call FUNC(resize,PTYPE) (nfob,ofob,ns,nadd,nrem,nremcv,re,id,bs)


end subroutine SUBRNAME
