program manual_test
  use flips

  implicit none

  type(flips_s) :: gg
  type(flips_d) :: dd

  real(sp), dimension(3) :: arow, ssol
  real(sp), dimension(1) :: mrow
  real(sp), dimension(1) :: error
  real(sp), dimension(3) :: scov

  real(dp), dimension(3) :: darow, dcov, dsol
  real(dp), dimension(1) :: dmrow
  real(dp), dimension(1) :: derror

  integer :: id

  write(*,*) 'Type 1 for binary files'
  read(*,*) id

write(*,*) 'Haa!'

  if (id==1) then
     call flips_init(gg,3,1,idnum=100,buffersize=2)
     call flips_init(dd,3,1,idnum=200,buffersize=2)
  else
     call flips_init(gg,3,1,buffersize=2,common=1)
     call flips_init(dd,3,1,buffersize=2,common=1)
  end if

write(*,*) 'init ok'

  arow = (/ 1.0, 2.0, 3.0 /)
  mrow = (/ 0.0 /)
  error = 0.1**2

  darow = (/ 1.0D0, 2.0D0, 3.0D0 /)
  dmrow = (/ 0.0D0 /)
  derror = 0.1D0**2

  call flips_add(gg,1,arow,mrow,error)
  call flips_add(dd,1,darow,dmrow,derror)

write(*,*) 'add 1 ok'

  arow = (/ 2.0, 3.0, 1.0 /)
  mrow = (/ 1.0 /)
  error=0.2**2

  darow = (/ 2.0D0, 3.0D0, 1.0D0 /)
  dmrow = (/ 1.0D0 /)
  derror=0.2D0**2

  call flips_add(gg,1,arow,mrow,error)
  call flips_add(dd,1,darow,dmrow,derror)

  arow = (/ 3.0, 2.0, 1.0 /)
  mrow = (/ 2.0 /)
  error=0.3**2

  darow = (/ 3.0D0, 2.0D0, 1.0D0 /)
  dmrow = (/ 2.0D0 /)
  derror=0.3D0**2

  call flips_add(gg,1,arow,mrow,error)
  call flips_add(dd,1,darow,dmrow,derror)

  arow = (/ 1.0, 3.0, 2.0 /)
  mrow = (/ 2.0 /)
  error=0.4**2

  darow = (/ 1.0D0, 3.0D0, 2.0D0 /)
  dmrow = (/ 2.0D0 /)
  derror=0.4D0**2

  call flips_add(gg,1,arow,mrow,error)
  call flips_add(dd,1,darow,dmrow,derror)
  ! Force rotations
  !call make_rotations_s(gg)

  !write(*,*) 'R:',gg%rmat
  !write(*,*) 'Y:',gg%ymat

  ! Solve (preliminary version)
  !call solve_utriag_mem_s(gg)
!write(*,*) 'hoo!'
  call flips_solve(gg)
  call flips_solve(dd)
!write(*,*) 'Wii!'
  ! Calc covariance
  call flips_calc_cov(gg)
  call flips_calc_cov(dd)
!write(*,*) 'Woo!'

!  if (id==1) then
!write(*,*) 'solu'
     call flips_get('solu',gg,ssol)
!write(*,*) 'cova'
     call flips_get('cova',gg,scov)
     call flips_get('solu',dd,dsol)
     call flips_get('cova',dd,dcov)
!  end if

!write(*,*) 'Goo!'

  write(*,*) 'Solution (sp):',ssol
!write(*,*) 'zippadappa'
  write(*,*) 'Residual (sp):',gg%residual
  write(*,*) ' '
!write(*,*) 'ehe...'
  write(*,*) 'Solution (dp):',dsol
  write(*,*) 'Residual (dp):',dd%residual  
  write(*,*) ' '

!write(*,*) 'Zap!'


  write(*,*) 'Covariances (sp):',scov
  write(*,*) 'Covariances (dp):',dcov


  !call print_info(gg)

  call flips_kill(gg)
  call flips_kill(dd)

end program manual_test
