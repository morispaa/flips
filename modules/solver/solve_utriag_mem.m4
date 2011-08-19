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
define(`SUBRNAME',FUNC(solve_utriag_mem,PTYPE))
!'


subroutine SUBRNAME (ftype)
  implicit none

  !*******************************************************
  ! Solves upper triangular system by using
  ! back substitution. Solution is kept in the
  ! memory. It is moved into bin file (if necessary)
  ! after completion. This is not a very good solution...
  ! 
  ! THIS NEEDS COMPLETE RE-THINKING!!
  !
  ! This is Stream I/O version!!!
  !
  !*******************************************************

  type(FOBTYPE), intent(inout) :: ftype

  integer :: k,m,nr,ny,i
  DTYPE (PRECISION) :: tmpsum
  DTYPE (PRECISION), dimension(ftype%ncols) :: rwrk
  DTYPE (PRECISION), dimension(ftype%nrhs) :: ywrk
  DTYPE (PRECISION), dimension(:,:), allocatable :: solution

  nr = ftype%ncols
  ny = ftype%nrhs

  ! Allocate space for solution matrix
  allocate(solution(nr,ny))

  ! If we are using binary files allocate solmat (otherwise it is already allocated!)
  if (ftype%use_files) allocate(ftype%solmat(nr*ny))


  ! Get the last rows of Y and R
  rwrk = 0.0
  call FUNC(get_row,PTYPE) ('ymat',ftype,size(ywrk),ywrk,nr)
  call FUNC(get_row,PTYPE) ('rmat',ftype,size(rwrk),rwrk,nr)

  ! Last row of solution
  solution(nr,:) = ywrk/rwrk(1)

  do k = (nr-1),1,-1
     ! Get k:th rows of R and Y (k+1:th row of solmat is in swrk!!!)
     call FUNC(get_row,PTYPE) ('ymat',ftype,size(ywrk),ywrk,k)
     rwrk = 0.0
     call FUNC(get_row,PTYPE) ('rmat',ftype,size(rwrk),rwrk,k)

     do m = 1,ny
        tmpsum = sum(rwrk(2:(nr-k+1)) * solution(k+1:nr,m),1)
        solution(k,m) = (ywrk(m) - tmpsum)/rwrk(1)
     end do
  end do

  ! Reshape solution matrix into vector (this is stupid)
  do i = 1,nr
     ftype%solmat(yind(i,1,ny):yind(i,ny,ny)) = solution(i,:)
  end do

  ! If we are using binary files, write solmat into file and deallocate solmat
  if (ftype%use_files) then

     call FUNC(write_solmat_file,PTYPE) (ftype)
     deallocate(ftype%solmat)
  end if

  ! Deallocate solution matrix
  deallocate(solution)
  
  ! Kilpis
  ftype%fc = ftype%fc + ( 3*(nr-1)*nr + (nr-2)*(nr-1) + 13*(nr-1) + 11 ) * ny
end subroutine SUBRNAME


