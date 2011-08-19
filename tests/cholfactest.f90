program cholfactest
  ! This program tests the modified cholesky factorization of form
  !
  !   A = C C^T
  !
  ! where A is a symmetric positive definite matrix and
  ! C is a upper triangular matrix.


  use flips

  implicit none

  real(4), dimension(4,4) :: amat, cmat
  complex(4), dimension(4,4) :: aamat,ccmat,tmat

  
  amat(1,:) = (/ 1.0,0.0,0.0,0.0 /)
  amat(2,:) = (/ 0.0,2.0,0.0,0.0 /)
  amat(3,:) = (/ 0.0,0.0,3.0,0.0 /)
  amat(4,:) = (/ 0.0,0.0,0.0,4.0 /)

  call modcholmat_s(4,amat,cmat)

  write(*,*) 'Test 1: real diagonal matrix'
  write(*,*) '-----------------------'

  write(*,*) amat(1,:)
  write(*,*) amat(2,:)
  write(*,*) amat(3,:)
  write(*,*) amat(4,:)  

  write(*,*) '-----------------------'

  write(*,*) cmat(1,:)
  write(*,*) cmat(2,:)
  write(*,*) cmat(3,:)
  write(*,*) cmat(4,:)

  write(*,*) '-----------------------'

  amat(1,:) = (/ 4.0, 1.0, 0.0, 1.0 /)
  amat(2,:) = (/ 1.0, 5.0, 1.0, 1.0 /)
  amat(3,:) = (/ 0.0, 1.0, 6.0, 1.5 /)
  amat(4,:) = (/ 1.0, 1.0, 1.5, 7.0 /)

  call modcholmat_s(4,amat,cmat)

  write(*,*) 'Test 2: real general matrix'
  write(*,*) '-----------------------'

  write(*,*) amat(1,:)
  write(*,*) amat(2,:)
  write(*,*) amat(3,:)
  write(*,*) amat(4,:)  

  write(*,*) '-----------------------'

  write(*,*) cmat(1,:)
  write(*,*) cmat(2,:)
  write(*,*) cmat(3,:)
  write(*,*) cmat(4,:)

  write(*,*) '-----------------------'

  cmat = matmul(cmat,transpose(cmat))

  write(*,*) cmat(1,:)
  write(*,*) cmat(2,:)
  write(*,*) cmat(3,:)
  write(*,*) cmat(4,:)

  write(*,*) '-----------------------'



  aamat(1,:) = (/ (1.0,1.0),(0.0,0.0),(0.0,0.0),(0.0,0.0) /)
  aamat(2,:) = (/ (0.0,0.0),(2.0,2.0),(0.0,0.0),(0.0,0.0) /)
  aamat(3,:) = (/ (0.0,0.0),(0.0,0.0),(3.0,3.0),(0.0,0.0) /)
  aamat(4,:) = (/ (0.0,0.0),(0.0,0.0),(0.0,0.0),(4.0,4.0) /)

  call modcholmat_c(4,aamat,ccmat)

!!$  write(*,*) 'Test 3: complex diagonal matrix'
!!$  write(*,*) '-----------------------'
!!$
!!$  write(*,*) aamat(1,:)
!!$  write(*,*) aamat(2,:)
!!$  write(*,*) aamat(3,:)
!!$  write(*,*) aamat(4,:)  
!!$
!!$  write(*,*) '-----------------------'
!!$
!!$  write(*,*) ccmat(1,:)
!!$  write(*,*) ccmat(2,:)
!!$  write(*,*) ccmat(3,:)
!!$  write(*,*) ccmat(4,:)
!!$
!!$  write(*,*) '-----------------------'
!!$
!!$ccmat = matmul(ccmat,transpose(ccmat))
!!$
!!$  write(*,*) ccmat(1,:)
!!$  write(*,*) ccmat(2,:)
!!$  write(*,*) ccmat(3,:)
!!$  write(*,*) ccmat(4,:)
!!$
!!$  write(*,*) '-----------------------'
!!$


  aamat(1,:) = (/ (4.0,0.0), (1.0,1.0), (0.0,0.0), (1.0,1.0) /)
  aamat(2,:) = (/ (1.0,-1.0), (5.0,0.0), (1.0,1.0), (1.0,1.0) /)
  aamat(3,:) = (/ (0.0,0.0), (1.0,-1.0), (6.0,0.0), (1.5,1.5) /)
  aamat(4,:) = (/ (1.0,-1.0), (1.0,-1.0), (1.5,-1.5), (7.0,0.0) /)

  call modcholmat_c(4,aamat,ccmat)

  write(*,*) 'Test 4: complex general matrix'
  write(*,*) '-----------------------'

  write(*,*) aamat(1,:)
  write(*,*) aamat(2,:)
  write(*,*) aamat(3,:)
  write(*,*) aamat(4,:)  

  write(*,*) '-----------------------'

  write(*,*) ccmat(1,:)
  write(*,*) ccmat(2,:)
  write(*,*) ccmat(3,:)
  write(*,*) ccmat(4,:)

  write(*,*) '-----------------------'


  ccmat = matmul(ccmat,transpose(conjg(ccmat)))

  write(*,*) ccmat(1,:)
  write(*,*) ccmat(2,:)
  write(*,*) ccmat(3,:)
  write(*,*) ccmat(4,:)

  write(*,*) '-----------------------'

!!$  tmat = transpose(ccmat)
!!$
!!$  write(*,*) tmat(1,:)
!!$  write(*,*) tmat(2,:)
!!$  write(*,*) tmat(3,:)
!!$  write(*,*) tmat(4,:)

end program cholfactest
