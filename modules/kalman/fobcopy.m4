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
define(`SUBRNAME',FUNC(fobcopy,PTYPE))
!'


subroutine SUBRNAME (nfob,ofob,idnum,buffersize)
  ! Copies FLIPS data types
  implicit none

  type(FOBTYPE) :: nfob,ofob
  integer, optional :: idnum,buffersize

  integer :: id,bs, ofobcovsize
  logical :: u_f

  ! First of all, make sure that the rotation buffer
  ! of ofob is empty
  call FUNC(make_rotations,PTYPE) (ofob)

  ! Check also, that nfob is uninitialized
  if(allocated(nfob%mrotbuf)) then
     write(0,*) 'FLIPS error: flips_copy: nfob is already allocated!'
     stop
  end if


  ! Optional arguments
  if (present(idnum)) then
     id = idnum
     u_f = .TRUE.
  else
     id = -1
     u_f = .FALSE.
  end if

  if (present(buffersize)) then
     bs = buffersize
  else
     bs = ofob%nrotbuf
  end if

!write(*,*) 'flips_copy:Trying to initialize nfob'

  ! Initialize nfob
  if (u_f) then
     call FUNC(flipsinit,PTYPE) (nfob,ofob%ncols,ofob%nrhs,idnum=id,buffersize=bs)
  else
     call FUNC(flipsinit,PTYPE) (nfob,ofob%ncols,ofob%nrhs,bandwidth=ofob%bw,&
          &common=ofob%common,buffersize=bs,zerothreshold=ofob%zeroth)
  end if

!write(*,*) '...........DONE!'

  ! Copy common things
  nfob%solexists = ofob%solexists
  nfob%covexists = ofob%covexists
  nfob%rinverted = ofob%rinverted
  nfob%fullcov = ofob%fullcov
  nfob%nrotbuf = ofob%nrotbuf
  nfob%nrows = ofob%nrows
  nfob%residual = ofob%residual
  nfob%tmpres = ofob%tmpres
  nfob%fc = ofob%fc
  nfob%bw = ofob%bw
  nfob%bbw = ofob%bbw
  ! Common variables
  nfob%common = ofob%common

  ! Allocate/open covariance matrix if necessary
  if (ofob%covexists .AND. nfob%use_files) then
     call FUNC(open_cov_file,PTYPE) (nfob)
  else if (ofob%covexists .AND. (.NOT. nfob%use_files)) then
     if (ofob%fullcov) then
        allocate(nfob%cmat(ofob%ncols*(ofob%ncols+1)/2))
     else
        allocate(nfob%cmat(ofob%ncols))
     end if
  end if

  ! Allocate rinvmat if necessary
  if (ofob%rinverted .AND. nfob%use_files) then
     call FUNC(open_invr_file,PTYPE) (nfob)
  else if (ofob%rinverted) then
     allocate(nfob%invrmat(ofob%ncols*(ofob%ncols+1)/2))
  end if



  ! Copy data

  ! Upper triangular matrix data
  ! Rmat
  call FUNC(copy_matrix,PTYPE) ('rmat',nfob,ofob)


  ! Invr
  if (ofob%rinverted) then
     call FUNC(copy_matrix,PTYPE) ('invr',nfob,ofob)
  end if


  ! Covariance
  if (ofob%covexists) then
     call FUNC(copy_matrix,PTYPE) ('cova',nfob,ofob)
  end if



  ! Full matrix data
  ! Ymat
  call FUNC(copy_matrix,PTYPE) ('ymat',nfob,ofob)
  ! Solution
  if (ofob%solexists) then
     call FUNC(copy_matrix,PTYPE) ('solu',nfob,ofob)
  end if



end subroutine SUBRNAME
