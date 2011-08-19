program get_test
  use flips

  type(flips_s) :: h

  real(sp), dimension(6,6) :: amat, r, cov, ir, test
  real(sp), dimension(6,1) :: sol, m, x, y, error, test2
  real(sp), dimension(6) :: tvec
  integer :: n

  call random_number(amat)
  call random_number(x)
  call random_number(error)
  error = 1.0

  m = matmul(amat,x)

  call flips_init(h,6,1)

  do n = 1,6
     call flips_add(h,1,amat(n,:),m(n,:),error(n,:))
  end do

  call flips_solve(h)
  call flips_calc_cov(h,.TRUE.)

  call get_matrix_s('solu',h,sol)
  call get_matrix_s('cova',h,cov)
  call get_matrix_s('rmat',h,r)
  call get_matrix_s('invr',h,ir)
  call get_matrix_s('ymat',h,y)
  test = matmul(ir,transpose(ir))
  test2 = matmul(ir,y)

  call flips_calc_cov(h)

  write(*,*) 'sol'
  write(*,*) '-------------------------------------------'
  do n=1,6
     write(*,'(2f10.5)') sol(n,:), x(n,:)
  end do
  write(*,*) '-------------------------------------------'

  write(*,*) 'test sol'
  write(*,*) '-------------------------------------------'
  do n=1,6
     write(*,'(2f10.5)') test2(n,:), x(n,:)
  end do
  write(*,*) '-------------------------------------------'

  write(*,*) 'cov'
  write(*,*) '-------------------------------------------'
  do n=1,6
     write(*,'(6f10.5)') cov(n,:)
  end do
  write(*,*) '-------------------------------------------'

 ! read(h%cfileunit,POS=1) tvec
 ! write(*,*) tvec

  write(*,*) 'test'
  write(*,*) '-------------------------------------------'
  do n=1,6
     write(*,'(6f10.5)') test(n,:)
  end do
  write(*,*) '-------------------------------------------'

  write(*,*) 'R'
  write(*,*) '-------------------------------------------'
  do n=1,6
     write(*,'(6f10.5)') r(n,:)
  end do
  write(*,*) '-------------------------------------------'

  write(*,*) 'invR'
  write(*,*) '-------------------------------------------'
  do n=1,6
     write(*,'(6f10.5)') ir(n,:)
  end do
  write(*,*) '-------------------------------------------'

  write(*,*) 'Y'
  write(*,*) '-------------------------------------------'
  do n=1,6
     write(*,'(f10.5)') y(n,:)
  end do
  write(*,*) '-------------------------------------------'

  !call flips_calc_residual(h)
  write(*,*) h%residual

end program get_test
