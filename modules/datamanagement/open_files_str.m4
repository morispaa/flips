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
define(`SUBRNAME',FUNC(open_files,PTYPE))
!'

subroutine SUBRNAME (ftype)

  ! Open binary files
  ! STREAM I/O version

  implicit none

  type(FOBTYPE) :: ftype
  integer :: idnum

  idnum = ftype%idnum

  ! Construct filenames and open files
  write(ftype%rfile,'("R_",I6.6,".dat")') idnum
  write(ftype%yfile,'("Y_",I6.6,".dat")') idnum
  write(ftype%cfile,'("C_",I6.6,".dat")') idnum
  write(ftype%solfile,'("sol_",I6.6,".dat")') idnum


ifdef(`DIRECT',
`  open(UNIT=ftype%idnum*100+11,FILE=ftype%rfile,STATUS="REPLACE",&
       FORM=flipsfileform,ACCESS=flipsfileaccess,RECL=ftype%rl)
  
  open(UNIT=ftype%idnum*100+12,FILE=ftype%yfile,STATUS="REPLACE",&
       FORM=flipsfileform,ACCESS=flipsfileaccess,RECL=ftype%rl)
  
  open(UNIT=ftype%idnum*100+13,FILE=ftype%solfile,STATUS="REPLACE",&
       FORM=flipsfileform,ACCESS=flipsfileaccess,RECL=ftype%rl)'
)

ifdef(`F2003_STREAM',
`  open(UNIT=ftype%idnum*100+11,FILE=ftype%rfile,STATUS="REPLACE",&
       FORM=flipsfileform,ACCESS=flipsfileaccess)
  
  open(UNIT=ftype%idnum*100+12,FILE=ftype%yfile,STATUS="REPLACE",&
       FORM=flipsfileform,ACCESS=flipsfileaccess)
  
  open(UNIT=ftype%idnum*100+13,FILE=ftype%solfile,STATUS="REPLACE",&
       FORM=flipsfileform,ACCESS=flipsfileaccess)'
)

end subroutine SUBRNAME
