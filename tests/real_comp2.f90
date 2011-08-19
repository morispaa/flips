program realcomp
  ! This program tests and compares the performance
  ! of single and double precision real problems

  use flips
  use rand

  implicit none

  ! types
  type(flips_s) :: gs
  type(flips_d) :: gd

  ! data matrices
  real(sp), dimension(:,:), allocatable :: amat_s, smat_s, mmat_s
  real(dp), dimension(:,:), allocatable :: amat_d, smat_d, mmat_d


  ! Errors
  real(sp) :: maxerr_s,meanerr_s
  real(dp) :: maxerr_d,meanerr_d


  ! Residuals
  real(sp) :: res_s
  real(dp) :: res_d
  

  ! Clocks/times
  real(dp) :: cts,cte,ctime1,ctime2
  real(sp) :: wts,wte,wtime1,wtime2

  ! Aux variables
  integer :: ns,n,bs,id, j
  
!  verbose = .TRUE.

  id = 0

  ! Set random seed
  call set_rand_seed()

  ! Ask size
  write(*,*) 'Give matrix size:'
  read(*,*) ns
  write(*,*) 'Press 1 for binary files.'
  read(*,*) id

  bs = 10

  ! allocate things
  allocate(amat_s(ns,ns),amat_d(ns,ns),smat_s(ns,1),smat_d(ns,1),mmat_s(ns,1),mmat_d(ns,1))

  ! Single pprecision

  ! Create data (theory matrix random, soluton 1's)
  call random_number(amat_s)
  !call random_number(smat_s)
  amat_s = 2.0 * amat_s - 1.0
  smat_s = 1.0
  mmat_s = matmul(amat_s,smat_s)

  ! init flips
  if (id==1) then
     call flips_init(gs,ns,1,idnum=100,buffersize=bs)
  else
     call flips_init(gs,ns,1,buffersize=bs,bandwidth=1)
  end if

!write(*,*) 'FLIPS init'
  
  ! add data and solve with residual
  call cpu_time(cts)
  !call system_clock(wts)
  do n = 1,ns
     call flips_add(gs,1,amat_s(n,:),mmat_s(n,:))
     !write(*,*) 'n=',n
  end do

!write(*,*) 'FLIPS ADD ready'

  call flips_solve(gs)

!write(*,*) 'FLIPS solve'

  call cpu_time(cte)
  !call system_clock(wte)
  !wtime1 = wte-wts
  ctime1 = cte-cts

  ! Calculate diagonal of the covariance
  call cpu_time(cts)
  !call system_clock(wts)
  call flips_calc_cov(gs,full=.FALSE.)

!write(*,*) 'FLIPS cov'

  call cpu_time(cte)
  !call system_clock(wte)
  !wtime2 = wte-wts
  ctime2 = cte-cts

  if (id==1) then
     allocate(gs%solmat(ns))
  !   do j = 1,ns
  !      read(gs%idnum*100+13,REC=j) gs%solmat(j)
  !   end do
     call flips_get('solu',gs,gs%solmat)

!write(*,*) 'FLIPS sol'

  end if

  ! Errors
  !maxerr_s = maxval(abs(smat_s(:,1) - gs%solmat))
  maxerr_s = maxval(((smat_s(:,1) - gs%solmat)/smat_s(:,1)))
  meanerr_s = sum(smat_s(:,1) - gs%solmat)/ns

  ! Print results
  write(*,*) '----------------------------------'
  write(*,*) 'Single precision, size:',ns
  write(*,*) '----------------------------------'
  write(*,*) '  Solution, CPU time:',ctime1
  write(*,*) '          FLOP Count:',gs%fc
  write(*,*) '              GFLOPS:',gs%fc/ctime1/1.0E9
  !write(*,*) 'Solution, wall clock:',wtime1
  write(*,*) 'Covariance, CPU time:',ctime2
  !write(*,*) 'Covariance, wall clock:',wtime2
  write(*,*) '          Mean error:',meanerr_s
  write(*,*) '     Max. rel. error:',maxerr_s
  write(*,*) '            Residual:',gs%residual
  !write(*,*) gs%solmat(1:5)
  write(*,*) '----------------------------------\n'



  ! Double precision
  ! Create data (theory matrix random, soluton 1's)
  call random_number(amat_d)
  !call random_number(smat_d)
  amat_d = 2.0D0 * amat_d - 1.0D0
  smat_d = 1.0D0
  mmat_d = matmul(amat_d,smat_d)

  ! init flips
  if (id==1) then
     call flips_init(gd,ns,1,idnum=200,buffersize=bs)
  else
     call flips_init(gd,ns,1,buffersize=bs)
  end if
  ! add data and solve with residual
  call cpu_time(cts)
  !call system_clock(wts)
  do n = 1,ns
     call flips_add(gd,1,amat_d(n,:),mmat_d(n,:))
 !    write(*,*) 'n=',n
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

  if (id==1) then
     allocate(gd%solmat(ns))
  !   do j = 1,ns
  !      read(gd%idnum*100+13,REC=j) gd%solmat(j)
  !   end do
  call flips_get('solu',gd,gd%solmat)
  end if

  ! Errors
  !maxerr_d = maxval(abs(smat_d(:,1) - gd%solmat))
  maxerr_d = maxval(((smat_d(:,1) - gd%solmat)/smat_d(:,1)))
  meanerr_d = sum(smat_d(:,1) - gd%solmat)/ns

  ! Print results
  write(*,*) '----------------------------------'
  write(*,*) 'Double precision, size:',ns
  write(*,*) '----------------------------------'
  write(*,*) '  Solution, CPU time:',ctime1
  write(*,*) '          FLOP Count:',gd%fc
  write(*,*) '              GFLOPS:',gd%fc/ctime1/1.0D9
  !write(*,*) 'Solution, wall clock:',wtime1
  write(*,*) 'Covariance, CPU time:',ctime2
  !write(*,*) 'Covariance, wall clock:',wtime2
  write(*,*) '          Mean error:',meanerr_d
  write(*,*) '     Max. rel. error:',maxerr_d
  write(*,*) '            Residual:',gd%residual
  !write(*,*) gd%solmat(1:5)
  write(*,*) '----------------------------------\n'





end program realcomp
