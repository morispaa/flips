program realcomp
  ! This program tests and compares the performance
  ! of single and double precision real problems

  use flips
  use rand

  implicit none

  ! types
  type(flips_c) :: gs
  type(flips_z) :: gd

  ! data matrices
  complex(sp), dimension(:,:), allocatable :: amat_s, smat_s, mmat_s
  complex(dp), dimension(:,:), allocatable :: amat_d, smat_d, mmat_d


  ! Errors
  complex(sp) :: meanerr_d,meanerr_s
  real(dp) :: maxerr_d,maxerr_s


  ! Residuals
  real(sp) :: res_s
  real(dp) :: res_d


  ! Clocks/times
  real(dp) :: cts,cte,ctime1,ctime2
  real(sp) :: wts,wte,wtime1,wtime2

  ! Aux variables
  integer :: ns,n,bs
  real(sp), dimension(:,:), allocatable :: sauxmat, sauxmat2
  real(dp), dimension(:,:), allocatable :: dauxmat, dauxmat2

  ! Set random seed
  call set_rand_seed()

  ! Ask size
  write(*,*) 'Give matrix size:'
  read(*,*) ns

  bs = 50000/ns

  ! allocate things
  allocate(amat_s(ns,ns),amat_d(ns,ns),smat_s(ns,1),smat_d(ns,1),&
       mmat_s(ns,1),mmat_d(ns,1),sauxmat(ns,ns),sauxmat2(ns,1),&
       dauxmat(ns,ns),dauxmat2(ns,1))

  ! Single pprecision

  ! Create data (theory matrix random, soluton 1's)
  call random_number(sauxmat)
  amat_s = sauxmat
  call random_number(sauxmat)
  amat_s = amat_s + (0.0,1.0)*sauxmat
  smat_s = (1.0,1.0)
  mmat_s = matmul(amat_s,smat_s)

  ! init flips
  call flips_init(gs,ns,1,buffersize=bs)

  ! add data and solve with residual
  call cpu_time(cts)
  !call system_clock(wts)
  do n = 1,ns
     call flips_add(gs,1,amat_s(n,:),mmat_s(n,:))
  end do
  call flips_solve(gs)
  call cpu_time(cte)
  !call system_clock(wte)
  !wtime1 = wte-wts
  ctime1 = cte-cts

  ! Calculate diagonal of the covariance
  call cpu_time(cts)
  !call system_clock(wts)
  call flips_calc_cov(gs,full=.FALSE.)
  call cpu_time(cte)
  !call system_clock(wte)
  !wtime2 = wte-wts
  ctime2 = cte-cts

  ! Errors
  maxerr_s = maxval(abs(smat_s(:,1) - gs%solmat))
  meanerr_s = sum(smat_s(:,1) - gs%solmat)/ns

  ! Print results
  write(*,*) '---------------------------------------'
  write(*,*) 'Single precision complex, size:',ns
  write(*,*) '---------------------------------------'
  write(*,*) '  Solution, CPU time:',ctime1
  !write(*,*) 'Solution, wall clock:',wtime1
  write(*,*) 'Covariance, CPU time:',ctime2
  !write(*,*) 'Covariance, wall clock:',wtime2
  write(*,*) '          Mean error:',meanerr_s
  write(*,*) '     Max. abs. error:',maxerr_s
  write(*,*) '            Residual:',gs%residual
  !  write(*,*) gs%cmat
  write(*,*) '---------------------------------------\n'



  ! Double precision

  ! Create data (theory matrix random, soluton 1's)
  call random_number(dauxmat)
  amat_d = dauxmat
  call random_number(dauxmat)
  amat_d = amat_d + (0.0D0,1.0D0)*dauxmat
  smat_d = (1.0D0,1.0D0)
  mmat_d = matmul(amat_d,smat_d)

  ! init flips
  call flips_init(gd,ns,1,buffersize=bs)

  ! add data and solve with residual
  call cpu_time(cts)
  !call system_clock(wts)
  do n = 1,ns
     call flips_add(gd,1,amat_d(n,:),mmat_d(n,:))
  end do
  call flips_solve(gd)
  call cpu_time(cte)
  !call system_clock(wte)
  !wtime1 = wte-wts
  ctime1 = cte-cts

  ! Calculate diagonal of the covariance
  call cpu_time(cts)
  !call system_clock(wts)
  call flips_calc_cov(gd,full=.FALSE.)
  call cpu_time(cte)
  !call system_clock(wte)
  !wtime2 = wte-wts
  ctime2 = cte-cts

  ! Errors
  maxerr_d = maxval(abs(smat_d(:,1) - gd%solmat))
  meanerr_d = sum(smat_d(:,1) - gd%solmat)/ns

  ! Print results
  write(*,*) '---------------------------------------'
  write(*,*) 'Double precision complex, size:',ns
  write(*,*) '---------------------------------------'
  write(*,*) '  Solution, CPU time:',ctime1
  !write(*,*) 'Solution, wall clock:',wtime1
  write(*,*) 'Covariance, CPU time:',ctime2
  !write(*,*) 'Covariance, wall clock:',wtime2
  write(*,*) '          Mean error:',meanerr_d
  write(*,*) '     Max. abs. error:',maxerr_d
  write(*,*) '            Residual:',gd%residual
  ! write(*,*) gd%cmat
  write(*,*) '---------------------------------------\n'


end program realcomp
