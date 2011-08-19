  !    flips - Grand Unified Linear Inverse Problem Solver
  !    Copyright (C) 2005-2006 University of Oulu, Finland
  !    Written by  Mikko Orispaa <mikko.orispaa@oulu.fi>
  !
  !    This program is free software; you can redistribute it and/or modify
  !    it under the terms of the GNU General Public License as published by
  !    the Free Software Foundation; either version 2 of the License, or
  !    (at your option) any later version.
  !
  !    This program is distributed in the hope that it will be useful,
  !    but WITHOUT ANY WARRANTY; without even the implied warranty of
  !    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  !    GNU General Public License for more details.
  !
  !    You should have received a copy of the GNU General Public License
  !    along with this program; if not, write to the Free Software
  !    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


module flipstest

  use flips
  use rand

  ! This is a module that contains some auxillary routines and functions 
  !for testing FLIPS and its properties.
  
  implicit none

interface random_band_matrix
   module procedure random_band_matrix_s, random_band_matrix_d
end interface

interface random_band_matrix_with_common
   module procedure random_band_matrix_with_common_s, &
        random_band_matrix_with_common_d
end interface

interface random_sparse_matrix
   module procedure random_sparse_matrix_s, random_sparse_matrix_d
end interface
  

contains

  subroutine random_band_matrix_s(maxband,mat)
    implicit none

    ! Creates a random band-formed square matrix
    ! maxband - maximum length of the matrix row (non zero elements)
    ! mat - real single precision matrix
 
    integer, intent(in) :: maxband
    real(sp), dimension(:,:), intent(out) :: mat

    integer :: i, col, row,start,end,blen
    real(sp), dimension(:), allocatable :: randrow

!write(*,*) '****** rbm ******'

    ! Matrix size
    col = size(mat,1)
    row = size(mat,2)

    allocate(randrow(col))

    if (col/=row) then
       write(0,*) 'random_band_matrix error: only square matrices supported at this time!'
       stop
    end if

    if (col < maxband) then
       write(0,*) 'random_band_matrix error: maxband too big!'
       stop
    end if

    mat = 0.0

    do i = 1,row

       blen = randi(1,maxband)

!write(*,*) '****** randi ******', blen, maxband

       if (i<=blen) then
          start = 1
       else
          start = randi(i-blen+1,i)
       end if

       if (i+blen-1 > col) then
           start = col - blen + 1
       end if

       call nrand(randrow)

!write(*,*) '****** nrand ******'
!write(*,*) start, start+blen-1

       mat(i,start:start+blen-1) = randrow(start:start+blen-1)

!write(*,*) '****** end do ******'

    end do

    deallocate(randrow)

  end subroutine random_band_matrix_s
     


  subroutine random_band_matrix_d(maxband,mat)
    implicit none

    ! Creates a random band-formed square matrix
    ! maxband - maximum length of the matrix row (non zero elements)
    ! mat - real single precision matrix

    integer, intent(in) :: maxband
    real(dp), dimension(:,:), intent(out) :: mat

    integer :: i, col, row,start,end,blen
    real(dp), dimension(:), allocatable :: randrow

    ! Matrix size
    col = size(mat,1)
    row = size(mat,2)

    allocate(randrow(col))

    if (col/=row) then
       write(0,*) 'random_band_matrix error: only square matrices supported at this time!'
       stop
    end if

    if (col < maxband) then
       write(0,*) 'random_band_matrix error: maxband too big!'
       stop
    end if

    mat = 0.0

    do i = 1,row

       blen = randi(1,maxband)

       if (i<=blen) then
          start = 1
       else
          start = randi(i-blen+1,i)
       end if

       if (i+blen-1 > col) then
          start = col - blen + 1
       end if

       call nrand(randrow)

       mat(i,start:start+blen-1) = randrow(start:start+blen-1)

    end do

    deallocate(randrow)

  end subroutine random_band_matrix_d



  subroutine random_band_matrix_with_common_s(maxband,ncommon,mat)
    implicit none

    ! Creates a random band matrix with common variables

    integer, intent(in) :: maxband, ncommon
    real(sp), dimension(:,:) :: mat

    integer :: col, row
    real(sp), dimension(:,:), allocatable :: commat

    col = size(mat,1)
    row = size(mat,2)

    call random_band_matrix_s(maxband,mat)

    allocate(commat(row,ncommon))

    call nrand(commat)

    mat(:,col-ncommon+1:col) = commat

    deallocate(commat)

  end subroutine random_band_matrix_with_common_s




  subroutine random_band_matrix_with_common_d(maxband,ncommon,mat)
    implicit none

    ! Creates a random band matrix with common variables

    integer, intent(in) :: maxband, ncommon
    real(dp), dimension(:,:) :: mat

    integer :: col, row
    real(dp), dimension(:,:), allocatable :: commat

    col = size(mat,1)
    row = size(mat,2)

    call random_band_matrix_d(maxband,mat)

    allocate(commat(row,ncommon))

    call nrand(commat)

    mat(:,col-ncommon+1:col) = commat

    deallocate(commat)

  end subroutine random_band_matrix_with_common_d


  subroutine random_sparse_matrix_d(fullness,mat)
    implicit none

    ! Creates a sparse matrix, hopefully invertible
    ! To increase the possibility of invertibleness
    ! the diagonal is always filled with non-zero elements.
    ! Sparseness is the persentage of non-zero elements.

    real(dp) :: fullness
    real(dp), dimension(:,:) :: mat

    integer :: nelem, i, col, row, x,y
    real(dp) :: rnum

    row = size(mat,1)
    col= size(mat,2)

    mat = 0.0

    ! Calculate the number of non-zero elements
    nelem = ceiling(fullness * real(row) * real(col))
    ! Take off the diagonal elements
    nelem = nelem - col

    ! Fill the diagonal
    do i = i,col
       call nrand(rnum)
       mat(i,i) = rnum
    end do

    ! Put the rest of the elements in place
    do
       if (nelem <= 0) exit

       x = randi(1,row)
       y = randi(1,col)

       if (mat(x,y) /= 0.0) cycle

       call nrand(rnum)
       mat(x,y) = rnum
       nelem = nelem - 1
    end do

  end subroutine random_sparse_matrix_d


  subroutine random_sparse_matrix_s(fullness,mat)
    implicit none

    ! Creates a sparse matrix, hopefully invertible
    ! To increase the possibility of invertibleness
    ! the diagonal is always filled with non-zero elements.
    ! Sparseness is the persentage of non-zero elements.

    real(sp) :: fullness
    real(sp), dimension(:,:) :: mat

    integer :: nelem, i, col, row, x,y
    real(sp) :: rnum

    row = size(mat,1)
    col= size(mat,2)

    mat = 0.0

    ! Calculate the number of non-zero elements
    nelem = ceiling(fullness * real(row) * real(col))
    ! Take off the diagonal elements
    nelem = nelem - col

    ! Fill the diagonal
    do i = i,col
       call nrand(rnum)
       mat(i,i) = rnum
    end do

    ! Put the rest of the elements in place
    do
       if (nelem <= 0) exit

       x = randi(1,row)
       y = randi(1,col)

       if (mat(x,y) /= 0.0) cycle

       call nrand(rnum)
       mat(x,y) = rnum
       nelem = nelem - 1
    end do

  end subroutine random_sparse_matrix_s


end module flipstest
       

    
