program rstest
  use flips
  use rand

  ! This program tests flips_resize in different situations
  ! Single precision real version

  implicit none

  type(flips_d) :: s1,s2,s3,s4

  real(dp), dimension(:), allocatable :: amat,amat2,rr,meas2
  real(dp), dimension(:), allocatable :: meas,erro
  logical, dimension(:), allocatable :: re
  real(dp) :: diff, maxdiff
  real(dp) :: ts,te
  logical, dimension(:), allocatable :: remo
  integer :: i

  call set_rand_seed()

  write(*,*) 'Test 1: Expand 1000 unknowns problem by 1000 new unknowns'
  ! ********************************************************************

  write(*,*) '  Memory storage'

  ! Initialize s1
  call flips_init(s1,1000,1,buffersize=50)
  call flips_init(s3,500,1,buffersize=50)



  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000),amat2(1500*500),rr(500*500),meas2(500),remo(1000))
  call random_number(amat)
  call random_number(meas)
  !call random_number(erro)
  erro = 1.0

 

  ! Feed data in s1
  call flips_add(s1,1000,amat,meas,erro)

  call cpu_time(ts)

  remo = .FALSE.

  call flips_resize(s2,s1,newsize=1500,remove=remo)

 ! write(*,*) s2%ymat
  write(*,*) 'ncols',s2%ncols
  write(*,*) 'nrhs',s2%nrhs
  write(*,*) 'nbuf',s2%nbuf
  write(*,*) 'nrows',s2%nrows
  write(*,*) 'bw',s2%bw
  write(*,*) 'bbw',s2%bbw
  write(*,*) 'common',s2%common
  write(*,*) 'rl',s2%rl
  write(*,*) 'zeroth',s2%zeroth
  write(*,*) 'nrotbuf',s2%nrotbuf

  !s2%nrows = 1000


  call cpu_time(te)

  

  ! Add some new data

  amat2 = 0.0
  call random_number(rr)
  do i = 1,500
     rr(yind(i,i,500)) = 1.0
  end do

  call random_number(meas2)
  
  do i = 1,500

     amat2(yind(i,1001,1500):yind(i,1500,1500)) = rr(yind(i,1,500):yind(i,500,500))

     !write(*,*) amat2(yind(i,996,1500):yind(i,1005,1500))

  end do

     !write(*,*) amat2(996:1005)

     !call random_number(meas2)

     call flips_add(s2,500,amat2,meas2,erro(1:500))
     call flips_add(s3,500,rr,meas2,erro(1:500))


  call flips_rotate(s1)
  call flips_rotate(s2)
  call flips_rotate(s3)

 ! call flips_resize(s4,s2,newsize=500)

  



  !write(*,*) s2%ymat(1:1000)-s1%ymat

  ! Solve all problems
  call flips_solve(s1)

  call flips_solve(s2)

  call flips_solve(s3)

 ! call flips_solve(s4)



write(*,*) s1%solmat(1:5)
write(*,*) s2%solmat(1:5)
write(*,*) s2%solmat(1001:1005)
write(*,*) s3%solmat(1:5)
!write(*,*) s4%solmat(1:5)

  ! Compare results
  diff = sum(s2%solmat(1:1000) - s1%solmat)/1000
  maxdiff = maxval(s2%solmat(1:1000) - s1%solmat)
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max diff:',maxdiff
  write(*,*) '    Resize time:',te-ts,'\n'

  call flips_kill(s1,.FALSE.)
  call flips_kill(s2,.FALSE.)
  call flips_kill(s3,.FALSE.)
 ! call flips_kill(s4,.FALSE.)
  deallocate(amat,amat2,meas,erro,rr,meas2)


end program rstest
