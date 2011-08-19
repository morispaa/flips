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
define(`SUBRNAME',FUNC(flipsadd_full_cov,PTYPE))
!'

!*****************************************************************
! flips_add_fc - add data to system (full error covariance)
! NB: this routine is not optimized for low bandwidth case!!
!*****************************************************************
subroutine SUBRNAME (ftype,n,arows,mrows,errors,force_rotations)

  ! Full error covariance matrix
  ! Error covariance is given as vector 
  ! consisting of the upper triagonal part of
  ! the symmetric error covariance matrix
  ! (in row-major order)
  ! OR
  ! as a vector consisting of the full error
  ! covariance matrix in row-major order. Note
  ! that only the upper triangular part is used
  ! and the symmetricy or the positive definessness
  ! is not checked!

  ! We calculate the cholesky factorization of the error covariance,
  ! invert it, multiply A and m with it, and feed it to
  ! flips_add

  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  integer, intent(in) :: n
  DTYPE (PRECISION), dimension(ftype%ncols*n), intent(in) :: arows
  DTYPE (PRECISION), dimension(ftype%nrhs*n), intent(in) :: mrows
  ! **********************************************************
  ! size of error covariance vector. This is given because of what seems
  ! to be a bug in ifort (080201) Otherwise, the speed slows 
  ! down (a lot).
  !integer, intent(in) :: en 
  ! **********************************************************
  real(PRECISION), dimension(:), intent(in) :: errors
  logical, intent(in), optional :: force_rotations

  !real(sp), dimension(ftype%ncols*n) :: awrk
  !real(sp), dimension(ftype%nrhs*n) :: mwrk

  DTYPE (PRECISION), dimension(:), allocatable :: awrk
  DTYPE (PRECISION), dimension(:), allocatable :: mwrk

  !integer, dimension(:), allocatable :: facst,facle 

  integer :: e_len, i
  logical :: full_mat, frot
  real (PRECISION), dimension(:), allocatable :: cfac, utp
  type(FOBTYPE) :: faux



  ! Check arguments
  e_len = size(errors)

  if (e_len == n**2) then
     full_mat = .TRUE.
  else if (e_len == n*(n+1)/2) then
     full_mat = .FALSE.
  else
     write(0,*) 'FLIPS error: flips_add: Error covariance matrix has wrong size!'
     stop
  end if

  if (present(force_rotations)) then
     frot = force_rotations
  else
     frot = .FALSE.
  end if

  ! Calculate modified Cholesky decomposition of the error covariance
  ! E = C * C', C upper triangular

  allocate(cfac(n*(n+1)/2))

  if (full_mat) then
     allocate(utp(n*(n+1)/2))
     
     do i = 1,n
        utp(rind(i,i,n):rind(i,n,n)) = errors(yind(i,i,n):yind(i,n,n))
     end do


     call FUNC(modcholvec,ETYPE) (n,n*(n+1)/2,utp,cfac)

     deallocate(utp)

  else

     call FUNC(modcholvec,ETYPE) (n,n*(n+1)/2,errors,cfac)

  end if

  allocate(awrk(ftype%ncols*n),mwrk(ftype%nrhs*n))

  ! Calculate C^{-1} * A using FLIPS

  

  call FUNC(flipsinit,PTYPE) (faux,n,ftype%ncols,buffersize=min(n,100))

  ! Put C as R and A as Y and update internal parameters
  faux%rmat = cfac

  faux%ymat = arows

  faux%nrows = n
  faux%nbuf = 0
  faux%solexists = .FALSE.
  faux%rinverted = .FALSE.
  faux%rle = n
  faux%fc = 0
  faux%tmpres = 0.0

  ! Solve
  !call FUNC(flipsadd,PTYPE) (faux,n,cfac,arows)

  call FUNC(flipssolve,PTYPE) (faux,.FALSE.)

  awrk = faux%solmat

  call FUNC(flipskill,PTYPE) (faux)

  ! Calculate C^{-1} * m using FLIPS
  call FUNC(flipsinit,PTYPE) (faux,n,ftype%nrhs,buffersize=min(n,100))
  faux%rmat = cfac
  faux%ymat = mrows
  faux%nrows = n
  faux%nbuf = 0
  faux%rle = n
  !call flips_add(faux,n,cfac,mrows)
  call FUNC(flipssolve,PTYPE) (faux,.FALSE.)
  mwrk = faux%solmat
  call FUNC(flipskill,PTYPE) (faux)





  ! Feed A_new and m_new into normal flips_add

  call FUNC(flipsadd,PTYPE) (ftype,n,awrk,mwrk,force_rotations=frot)

  deallocate(awrk,mwrk)


end subroutine SUBRNAME
