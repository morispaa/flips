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
define(`SUBRNAME',FUNC(get_matrix,PTYPE))
!'

subroutine SUBRNAME (mtype,ftype,mat)
  ! Gives FLIPS matrix in matrix form

  implicit none

  character(len=4) :: mtype
  type(FOBTYPE) :: ftype
  DTYPE (PRECISION), dimension(:,:) :: mat
  integer :: i, j
  integer, dimension(2) :: matshape
  DTYPE (PRECISION), dimension(:), allocatable :: datarow
  logical :: get_mat, utri
  character(len=200) :: errortext

  get_mat =.TRUE.


  select case (mtype)

  case ('solu')
     if (ftype%solexists) then
        matshape = (/ ftype%ncols , ftype%nrhs /)
        utri=.FALSE.
     else
        get_mat=.FALSE.
        errortext='Can not get solution matrix. Solution not calculated.'
     end if

  case ('cova')
     if (ftype%covexists) then
        if (ftype%fullcov) then
           matshape = (/ ftype%ncols , ftype%ncols /)
           utri=.TRUE.
        else
           get_mat=.FALSE.
           errortext='Can not get covariance matrix. Full covariance not calculated. Use vector argument instead!'
        end if
     else
        get_mat=.FALSE.
        errortext='Can not get covariance matrix. Covariance is not calculated.'
     end if

  case ('rmat')
     matshape = (/ ftype%ncols , ftype%ncols /)
     utri=.TRUE.

  case ('ymat')
     matshape = (/ ftype%ncols , ftype%nrhs /)
     utri=.FALSE.

  case ('invr')
     if (ftype%rinverted) then
        matshape = (/ ftype%ncols , ftype%ncols /)
        utri=.TRUE.
     else
        get_mat=.FALSE.
        errortext='Can not get R inverse. R inverse is not calculated!'
     end if

  case default
     write(0,*) 'FLIPS error: get_matrix: Unknown matrix type ',mtype
     stop

  end select

  ! Check mat shape
  if (any(shape(mat) /= matshape)) then
     write(0,*) 'FLIPS error: get_matrix_s: Covariance matrix given as argument has wrong size!'
     stop
  end if

  if (get_mat) then
     ! Copy matrix into mat
     mat = 0.0
     allocate(datarow(matshape(2)))
     do i = 1,ftype%nrows
        call FUNC(get_row,PTYPE) (mtype,ftype,matshape(2),datarow,i)

        if (utri) then
           mat(i,i:matshape(2)) = datarow(1:matshape(2)-i+1)
        else
           mat(i,:) = datarow
        end if
     end do
     deallocate(datarow)
  else
     write(0,*) 'FLIPS error: get_matrix_s: ',trim(errortext)
     stop
  end if


  ! Handle full covariance case (upper triangular -> full matrix)
  if (mtype=='cova' .AND. ftype%fullcov) then
     do i = 1,ftype%ncols
        do j = 1,ftype%ncols
           mat(j,i) = mat(i,j)
        end do
     end do
  end if

end subroutine SUBRNAME
