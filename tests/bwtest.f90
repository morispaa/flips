program bwtest
  ! tests the stair-FLIPS bandwidth system
  use flips

  implicit none

  type(flips_s) :: ss

  real(sp), dimension(:,:), allocatable :: amat,bmat,cmat
  real, dimension(:), allocatable :: sol,meas, vec,cov1

  ! amat diagonal
  ! bmat low bw
  ! cmat one full row

  integer :: n = 1000

  ! n problem size

  integer :: st(10),en,i


  allocate(amat(n,n),bmat(n,n),cmat(10,n),sol(n),meas(2*n+10),vec(n),cov1(n*(n+1)/2))
  amat = 0.0
  bmat = 0.0
  cmat = 0.0

  ! solution
  sol = 1.0

  ! amat is a diagonal matrix
  do i = 1,n
     call random_number(vec(1))
     amat(i,i) = vec(1)
  end do

  ! bmat has bw of 9 elements
  do i = 5,n-4
     call random_number(vec(1:9))
     bmat(i,(i-4):(i+4)) = vec(1:9)
  end do
  
  do i = 1,4
     call random_number(vec(1:4))
     bmat(i,1:i) = vec(1:i)
  end do

  do i = n-3,n
     call random_number(vec(n-3:n))
     bmat(i,i:n) = vec(i:n)
  end do     
  
  ! cmat has random bw
  do i = 1,10
     call random_number(vec(1))
     st(i) = 500.0*vec(1) + 1
     call random_number(vec(1:st(i)))
     cmat(i,1:st(i)) = vec(1:st(i))
  end do

  ! measurement

  meas(1:n) = matmul(amat,sol)
  meas(n+1:2*n) = matmul(bmat,sol)
  meas(2*n+1:2*n+10) = matmul(cmat,sol)


  ! Initializing FLIPS with bw 1
  call flips_init(ss,n,1,bandwidth=1)

  ! Add amat and measuremnts, chech bw
  do i = 1,n-1
     call flips_add(ss,1,amat(i,:),meas(i))
  end do
  call flips_add(ss,1,amat(n,:),meas(n),force_rotations=.TRUE.)

  write(*,*) 'Diagonal matrix added. BW=',ss%bw

  ! Add bmat and meas, check bw
  do i = 1,n-1
     call flips_add(ss,1,bmat(i,:),meas(n+i))
  end do
  call flips_add(ss,1,bmat(n,:),meas(2*n),force_rotations=.TRUE.)

  write(*,*) 'Band matrix added. BW=',ss%bw


  ! Add cmat row by row
  do i = 1,10
     call flips_add(ss,1,cmat(i,:),meas(2*n+i),force_rotations=.TRUE.)
     write(*,*) 'Row length=',st(i)
     write(*,*) 'BW=',ss%bw
  end do


  ! Solve and check solution
  call flips_solve(ss)

  write(*,*) 'Max. error=',maxval(sol-ss%solmat)

  call flips_calc_cov(ss,.TRUE.)

  cov1=ss%cmat
  


  call flips_kill(ss)


  ! Calculate in trad way
  call flips_init(ss,n,1)

  do i = 1,n-1
     call flips_add(ss,1,amat(i,:),meas(i))
  end do
  call flips_add(ss,1,amat(n,:),meas(n),force_rotations=.TRUE.)

  do i = 1,n-1
     call flips_add(ss,1,bmat(i,:),meas(n+i))
  end do
  call flips_add(ss,1,bmat(n,:),meas(2*n),force_rotations=.TRUE.)

  do i = 1,10
     call flips_add(ss,1,cmat(i,:),meas(2*n+i),force_rotations=.TRUE.)
  end do  

  write(*,*) '--------------------------------------------------'
  write(*,*) 'BW=',ss%bw

  call flips_solve(ss)

   write(*,*) 'Max. error=',maxval(sol-ss%solmat)

   call flips_calc_cov(ss,.TRUE.)

   write(*,*) 'Cov diff =', maxval(cov1-ss%cmat)

   call flips_kill(ss)


end program bwtest
