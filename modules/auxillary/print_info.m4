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


! NB: This is out-of-date and quite useless...

include(`m4/'PTYPE`_type.m4')
define(`FUNC',``$1'_`$2'')
define(`SUBRNAME',FUNC(print_info,PTYPE))

!'

!*****************************************************************
!print_info - prints information about given flips data type
!*****************************************************************
subroutine SUBRNAME (ftype)
  type(FOBTYPE), intent(in) :: ftype

  write(6,*) '*****************************************'
  write(6,*) '* Flags:'
  write(6,*) '*        cplx:',ftype%cplx
  write(6,*) '*         dbl:',ftype%dbl
  write(6,*) '*   use_files:',ftype%use_files
  write(6,*) '* '
  write(6,*) '* Size:'
  write(6,*) '*   nrotbuf:',ftype%nrotbuf
  write(6,*) '*     ncols:',ftype%ncols
  write(6,*) '*      nrhs:',ftype%nrhs
  write(6,*) '*      nbuf:',ftype%nbuf
  write(6,*) '*     nrows:',ftype%nrows
  write(6,*) '* '

  if (ftype%use_files) then
     write(6,*) '* Data stored in binary files'
     write(6,*) '*     IDnum:',ftype%idnum
     write(6,*) '*     Rfile:',ftype%rfile
     write(6,*) '*     Yfile:',ftype%yfile
     write(6,*) '*     Cfile:',ftype%cfile
     write(6,*) '*   solfile:',ftype%solfile
  else
     write(6,*) '* Data stored in memory'
     write(6,*) '*    R matrix size:', size(ftype%rmat)
     write(6,*) '*    Y matrix size:', size(ftype%ymat)
     write(6,*) '*    solution matrix size:', size(ftype%solmat)
     if (allocated(ftype%cmat)) then
        write(6,*) '*   covariance matrix size:',size(ftype%cmat)
     else
        write(6,*) '*   covariance matrix not yet allocated'
     end if
  end if

  write(6,*) '*****************************************'

end subroutine SUBRNAME
