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
define(`SUBRNAME',FUNC(flipsdelete,PTYPE))
!'

!*****************************************************************
! flips_delete - delete data rows from problem
!*****************************************************************

subroutine SUBRNAME (ftype,n,arows,mrows,errors)
  implicit none

  type(FOBTYPE), intent(inout) :: ftype
  integer, intent(in) :: n
  DTYPE (PRECISION), dimension(ftype%ncols*n), intent(in) :: arows
  DTYPE (PRECISION), dimension(ftype%nrhs*n), intent(in) :: mrows
  real (PRECISION), dimension(n), optional, intent(in) :: errors

  logical :: errors_exist, c_real
  integer :: i, ncols, nrhs, j, rlen
  DTYPE (PRECISION), dimension(ftype%ncols) :: a_aux, r_row
  DTYPE (PRECISION), dimension(ftype%nrhs) :: m_aux, y_row
  DTYPE (PRECISION), dimension(ftype%ncols) :: r_tmp
  DTYPE (PRECISION), dimension(ftype%nrhs) :: y_tmp
  DTYPE (PRECISION) :: c,s,t,a,b


  ! First of all, if rotation buffer is not empty, rotate the rows
  if (ftype%nbuf > 0) then
     call FUNC(make_rotations,PTYPE) (ftype)
  end if


  ncols = ftype%ncols
  nrhs = ftype%nrhs

  if (present(errors)) then
     errors_exist = .TRUE.
  else
     errors_exist = .FALSE.
  end if

  ! Clear solution-exists-flag
  ftype%solexists = .FALSE.
  ! Clear R-inverted-flag
  ftype%rinverted = .FALSE.
  ftype%covexists = .FALSE.

  ! --------------------------------------------------------------- !
  ! Notes:                                                          !
  ! - No rotation buffers here! Antirotations are made immediately  !
  ! - No full error covariance, just diagonal supported for now     !
  ! - It would be better to reverse the loops (take one R row       !
  !   and rotate through arows)                                     !
  ! - It could be possible in theory that deletion of rows could    !
  !   decrease the rmat bandwidth. However, it is NOT checked here! !
  !   (Too big bw just wastes memory a little)                      !
  ! --------------------------------------------------------------- !

  loop_arows: do i = 1,n

     ! get next arow and mrow
     ! Scale errors and multiply data by i
     if (errors_exist) then
        a_aux = arows(yind(i,1,ncols):yind(i,ncols,ncols))/sqrt(errors(i))
        m_aux = mrows(yind(i,1,nrhs):yind(i,nrhs,nrhs))/sqrt(errors(i))
     else
        a_aux = arows(yind(i,1,ncols):yind(i,ncols,ncols))
        m_aux = mrows(yind(i,1,nrhs):yind(i,nrhs,nrhs))
     end if

     ! Antirotate
     loop_R_rows: do j = 1,min(ftype%ncols,ftype%nrows)

        rlen = ftype%ncols-j+1

        !! Get R row and Y row (complex)
        call FUNC(get_row,PTYPE) ('rmat',ftype,ftype%ncols,r_row,j)
        call FUNC(get_row,PTYPE) ('ymat',ftype,ftype%nrhs,y_row,j)

        !! Calculate antirot coefficients. This is not good...
        !! if |b| > |a|, this fails, crashes and burns...

        a = r_row(1)
        b = a_aux(j)

        call FUNC(arot_coeff,PTYPE) (a,b,s,c,ftype%zeroth)

        ifdef(`M4_REAL',`
        !! Antirotate R and arow
        r_tmp = r_row
        r_row = c * r_row(1:rlen) - s * a_aux(j:ftype%ncols)
        a_aux(j:ftype%ncols) = c * a_aux(j:ftype%ncols) - s * r_tmp(1:rlen)
        !! Antirotate Y and mrow
        y_tmp = y_row
        y_row = c * y_row - s * m_aux
        m_aux = c * m_aux - s * y_tmp
        ')

        ifdef(`M4_COMPLEX',`
        !! Antirotate R and arow
        r_tmp = r_row
        r_row = c * r_row(1:rlen) - s * a_aux(j:ftype%ncols)
        a_aux(j:ftype%ncols) = conjg(c) * a_aux(j:ftype%ncols) - conjg(s) * r_tmp(1:rlen) 
        !! Antirotate Y and mrow
        y_tmp = y_row
        y_row = c * y_row - s * m_aux
        m_aux = conjg(c) * m_aux - conjg(s) * y_tmp
        ')       

        !! Put R and Y rows back
        call FUNC(put_row,PTYPE) ('rmat',ftype,ftype%ncols,r_row,j)
        call FUNC(put_row,PTYPE) ('ymat',ftype,ftype%ncols,y_row,j)

     end do loop_R_rows

  end do loop_arows


end subroutine SUBRNAME
