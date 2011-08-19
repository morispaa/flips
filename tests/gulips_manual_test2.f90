program gmantest2
  use flips

  implicit none

  ! GULIPS Manual test 
  ! with full error covariance matrix

  type(flips_s) :: h1,h2

  real(sp) :: A(12),m(4),c1(10),c2(16)

!write(*,*) '*'
  A(1:3) =   (/ 1.0, 2.0, 3.0/)
  A(4:6) =   (/ 2.0, 3.0, 1.0/)
  A(7:9) =   (/ 3.0, 2.0, 1.0/)
  A(10:12) = (/ 1.0, 3.0, 2.0/)
!write(*,*) '*'
  m = (/ 0.0, 1.0, 2.0, 2.0 /)
!write(*,*) '*'
  c1 = (/ 0.1,0,0,0,0.2,0,0,0.3,0,0.4 /)
!write(*,*) '*'
  c1 = c1**2
!write(*,*) '*'
  c2(1:4) = (/ 0.1,0,0,0 /)
  c2(5:8) = (/ 0.0,0.2,0,0 /)
  c2(9:12) = (/ 0.0,0,0.3,0 /)
  c2(13:16) = (/ 0.0,0,0,0.4 /)
!write(*,*) '*'
  c2 = c2**2
!write(*,*) '*'

  call flips_init(h1,3,1,buffersize=2)
!write(*,*) 'init 1'

  call  flips_add_fc(h1,4,A,m,c1)
!write(*,*) 'add 1'
  call flips_solve(h1)
!write(*,*) 'solve 1'
  call flips_calc_cov(h1,.FALSE.)
!write(*,*) 'cov 1'
  write(*,*) h1%solmat
  write(*,*) sqrt(h1%cmat)

  call flips_kill(h1)
!write(*,*) 'kill 1'

  call flips_init(h2,3,1,buffersize=2)
!write(*,*) 'init 2'
  call  flips_add_fc(h2,4,A,m,c2)
!write(*,*) 'add 2'
  call flips_solve(h2)
!write(*,*) 'solve 2'
  call flips_calc_cov(h2,.FALSE.)
!write(*,*) 'cov 2'
  write(*,*) h2%solmat
!write(*,*) ''
  write(*,*) sqrt(h2%cmat)
!write(*,*) 'zzz'
  call flips_kill(h2)
!write(*,*) 'kill 2'


end program gmantest2
