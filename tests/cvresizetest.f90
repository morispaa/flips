program cvresizetest
  use flips
  use rand

  implicit none

  integer, parameter :: n = 200, cv = 30, bw = 20, bs = 10

  type(flips_d) :: ha,hb

  real(dp), dimension(:,:), allocatable :: Amat, wAmat, sol,meas
  real(dp), dimension(:), allocatable ::  fsol, rsol, wbw, wcv,err

  real(dp), dimension(8) :: ttt

  integer :: i

  logical, dimension(:), allocatable :: resizemask

  call set_rand_seed()

  allocate(Amat(n,n),wAmat(n+bw,n+bw),meas(n,1),sol(n,1),fsol(n),wbw(bw),wcv(cv),resizemask(n))

  ! solution
  call random_number(sol)

  ! Theory matrix
  wAmat = 0.0
  do i = 1,n
     call random_number(wbw)
     wAmat(i,i:(i+bw-1)) = wbw
     call random_number(wbw)
     wAmat(i:(i+bw-1),i) = wbw
  end do

  Amat = wAmat(1:n,1:n)
  if (cv > 0) then
     call random_number(Amat(1:n,n-cv+1:n))
  end if

  do i = 1,n
     Amat(i,i) = 1.0
  end do

  ! measurement
  meas = matmul(Amat,sol)

  !call get_row_d('rmat',ha,n,fsol,145)
  !write(*,*) 'rrow=',Amat(145,:)
  !writew(*,*) ' ' 

  ! Full FLIPS problem
  call flips_init(ha,200,1,bandwidth=1,common=cv,buffersize=bs)

  do i = 1,n
     call flips_add(ha,1,Amat(i,:),meas(i,1))
  end do

  call flips_solve(ha)

  fsol = ha%solmat

 ! write(*,*) fsol
  write(*,*) 'Orig. Maxerr=',maxval(fsol-sol(:,1))
  !write(*,*) ' '
  write(*,*) 'bw=',ha%bw,'cv=',ha%common


write(*,*) '** **'
  ! Resize 1
  ! Marginalize unknowns 1..100, add none
  resizemask = .FALSE.
  resizemask(1:100) = .TRUE.

  allocate(err(100),rsol(100))

  call flips_resize(hb,ha,100,remove=resizemask,buffersize=bs)

  call flips_solve(hb)

  !call get_row_s('rmat',hb,100,rsol,50)
  !write(*,*) 'rrow=',rsol
  !write(*,*) ' '
!  write(*,*) 'bw=',hb%bw

  rsol = hb%solmat

  !write(*,*) rsol

  err(1:100) = rsol(1:100)-sol(101:200,1)
  !err(51:100) = rsol(51:100) - sol(151:200,1)

  write(*,*) 'Resize 1. Max err=',maxval(err)
  write(*,*) 'bw=',hb%bw,'cv=',hb%common

  deallocate(err,rsol)

  call flips_kill(hb)

write(*,*) '** **'
  ! Resize 2
  ! Marginalize unknowns 176..200
  
  resizemask = .FALSE.
  resizemask(176:200) = .TRUE.

  allocate(err(175),rsol(175))

  call flips_resize(hb,ha,remove=resizemask,buffersize=bs)

  call flips_solve(hb)

  rsol=hb%solmat

  err = rsol-sol(1:175,1)
  write(*,*) 'Resize 2. Max err=',maxval(err)
    write(*,*) 'bw=',hb%bw,'cv=',hb%common
  
  deallocate(err,rsol)

  call flips_kill(hb)

write(*,*) '** **'
  ! Resize 3
  ! Marginalize unknowns 161..180
  
  resizemask = .FALSE.
  resizemask(161:180) = .TRUE.

  allocate(err(180),rsol(180))

!write(*,*) '**'

  call flips_resize(hb,ha,remove=resizemask,buffersize=bs)

!write(*,*) '** **'

  !call make_rotations_s(hb)
  call flips_solve(hb)

  rsol=hb%solmat

  err(1:160) = rsol(1:160)-sol(1:160,1)
  err(161:180) = rsol(161:180) - sol(181:200,1)
  !write(*,*) err
  write(*,*) 'Resize 3. Max err=',maxval(err)
  write(*,*) 'bw=',hb%bw,'cv=',hb%common
  
  call flips_kill(hb)



  
  
  call flips_kill(ha)

end program cvresizetest

  
  
  
  
