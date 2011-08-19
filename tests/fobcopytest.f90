program fobcopytest
  use flips

  implicit none

  ! Testing fobcopy_s (flips_copy)

  type(flips_s) :: fob1,fob2

  real(sp), dimension(10) :: arow, sol1,sol2,cov1,cov2
  real(sp), dimension(1) :: meas
  real(sp) :: err
  integer :: i

  call flips_init(fob1,10,1)

  do i = 1,20
     call random_number(arow)
     call random_number(meas)
     call random_number(err)

     err = err + 0.5

     call flips_add(fob1,1,arow,meas,err)

  end do

  call fobcopy_s(fob2,fob1)

write(*,*) '*'

  call flips_solve(fob1)
  call flips_solve(fob2)

  call flips_calc_cov(fob1,.FALSE.)
  call flips_calc_cov(fob2,.FALSE.)

  call flips_get('solu',fob1,sol1)
  call flips_get('solu',fob2,sol2)

  call flips_get('cova',fob1,cov1)
  call flips_get('cova',fob2,cov2)

  write(*,*) 'Solutions'
  write(*,*) sol1
  write(*,*) sol2

  write(*,*) 'Covariances'
  write(*,*) cov1
  write(*,*) cov2

  write(*,*) 'Residuals'
  write(*,*) fob1%residual
  write(*,*) fob2%residual

  call flips_kill(fob1)
  call flips_kill(fob2)

end program fobcopytest
