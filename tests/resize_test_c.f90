program rstest
  use flips

  ! This program tests flips_resize in different situations
  ! Single precision real version

  implicit none

  type(flips_c) :: s1, s2

  complex(sp), dimension(:), allocatable :: amat
  real(sp), dimension(:), allocatable :: tmat,erro
  complex(sp), dimension(:), allocatable :: meas,sol2
  real(sp), dimension(:), allocatable :: tvec
  logical, dimension(:), allocatable :: re
  complex(sp) :: diff
  real(sp) :: maxdiff
  real(dp) :: ts,te
  logical, dimension(:), allocatable :: remo

  write(*,*) 'Test 1: Marginalize 500 first unknowns from a 1000 unknowns problem'
  ! ********************************************************************

  write(*,*) '  Memory storage'

  ! Initialize s1
  call flips_init(s1,1000,1,buffersize=50)



  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000),tmat(1000*1000),tvec(1000))
  call random_number(tmat)
  amat = tmat
  call random_number(tmat)
  amat = amat + (0.0,1.0)*tmat
  call random_number(tvec)
  meas = tvec
  call random_number(tvec)
  meas = meas + (0.0,1.0)*tvec
  call random_number(erro)
  erro = erro + 0.5

 

  ! Feed data in s1
  call flips_add(s1,1000,amat,meas,erro)

  call cpu_time(ts)
  ! Resize by marginalizing 500 first unknowns

  call flips_resize(s2,s1,newsize=500)
  call cpu_time(te)



  ! Solve both problems
  call flips_solve(s1)

  call flips_solve(s2)



!write(*,*) s1%solmat(501:505)
!write(*,*) s2%solmat(1:5)

  ! Compare results
  diff = sum(s2%solmat - s1%solmat(501:1000))/500
  maxdiff = maxval(abs(s2%solmat - s1%solmat(501:1000)))
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max abs diff:',maxdiff
  write(*,*) '    Resize time:',te-ts,'\n'

  call flips_kill(s1,.FALSE.)
  call flips_kill(s2,.FALSE.)
  deallocate(amat,meas,erro,tmat,tvec)

write(*,*) '  File storage'

  ! Initialize s1
  call flips_init(s1,1000,1,idnum=1,buffersize=50)
  
  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000),tmat(1000*1000),tvec(1000),sol2(1000))
  call random_number(tmat)
  amat = tmat
  call random_number(tmat)
  amat = amat + (0.0,1.0)*tmat
  call random_number(tvec)
  meas = tvec
  call random_number(tvec)
  meas = meas + (0.0,1.0)*tvec
  call random_number(erro)
  erro = erro + 0.5
  
  ! Feed data in s1
  call flips_add(s1,1000,amat,meas,erro)

  call cpu_time(ts)
  ! Resize by marginalizing 500 first unknowns
  call flips_resize(s2,s1,newsize=500,idnum=2)
  call cpu_time(te)

  ! Solve both problems
  call flips_solve(s1)
  call flips_solve(s2)



  ! Read results from file
  read(113,pos=1) meas
  read(213,pos=1) sol2(1:500)

!write(*,*) meas(501:505)
!write(*,*) erro(1:5)

  ! Compare results
  diff = sum(sol2(1:500) - meas(501:1000))/500
  maxdiff = maxval(abs(sol2(1:500) - meas(501:1000)))
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max abs diff:',maxdiff
  write(*,*) '    Resize time:',te-ts,'\n'

  call flips_kill(s1,keepfiles=.FALSE.)
  call flips_kill(s2,keepfiles=.FALSE.)
  deallocate(amat,meas,erro,tmat,tvec,sol2)

write(*,*) 'Test 2: Remove last 500 unknowns from a 1000 unknowns problem'

 write(*,*) '  Memory storage'



  ! Initialize s1
  call flips_init(s1,1000,1,buffersize=50)


  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000),tmat(1000*1000),tvec(1000),remo(1000))
  call random_number(tmat)
  amat = tmat
  call random_number(tmat)
  amat = amat + (0.0,1.0)*tmat
  call random_number(tvec)
  meas = tvec
  call random_number(tvec)
  meas = meas + (0.0,1.0)*tvec
  call random_number(erro)
  erro = erro + 0.5
  remo = .FALSE.
  remo(501:1000) = .TRUE.

 

  ! Feed data in s1 
  call flips_add(s1,1000,amat,meas,erro)


  call cpu_time(ts)
  ! Resize by marginalizing last 500 unknowns
  call flips_resize(s2,s1,remove=remo)
  call cpu_time(te)


  ! Solve both problems
  call flips_solve(s1)
  call flips_solve(s2)

!write(*,*) s1%solmat(501:505)
!write(*,*) s2%solmat(1:5)

  ! Compare results
  diff = sum(s2%solmat - s1%solmat(1:500))/500
  maxdiff = maxval(abs(s2%solmat - s1%solmat(1:500)))
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max abs diff:',maxdiff
  write(*,*) '    Resize time:',te-ts,'\n'

  call flips_kill(s1,.FALSE.)
  call flips_kill(s2,.FALSE.)
  deallocate(amat,meas,erro,remo,tmat,tvec)

 write(*,*) '  File storage'

  ! Initialize s1
  call flips_init(s1,1000,1,idnum=1,buffersize=50)
  
  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000),tmat(1000*1000),tvec(1000),remo(1000),sol2(1000))
  call random_number(tmat)
  amat = tmat
  call random_number(tmat)
  amat = amat + (0.0,1.0)*tmat
  call random_number(tvec)
  meas = tvec
  call random_number(tvec)
  meas = meas + (0.0,1.0)*tvec
  call random_number(erro)
  erro = erro + 0.5
  remo = .FALSE.
  remo(501:1000) = .TRUE.
  
  ! Feed data in s1 
  call flips_add(s1,1000,amat,meas,erro)

  call cpu_time(ts)
  ! Resize by marginalizing last 500 unknowns
  call flips_resize(s2,s1,remove=remo,idnum=2)
  call cpu_time(te)

!write(*,*) '!!'

  ! Solve both problems
  call flips_solve(s1)

!write(*,*) 's1'

  call flips_solve(s2)

!write(*,*) 's2'
  ! Read results from file
  read(113,pos=1) meas
  read(213,pos=1) sol2(1:500)

!write(*,*) meas(501:505)
!write(*,*) erro(1:5)

  ! Compare results
  diff = sum(sol2(1:500) - meas(1:500))/500
  maxdiff = maxval(abs(sol2(1:500) - meas(1:500)))
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max abs diff:',maxdiff
  write(*,*) '    Resize time:',te-ts,'\n'

  call flips_kill(s1,.FALSE.)
  call flips_kill(s2,.FALSE.)
  deallocate(amat,meas,erro,remo,tmat,tvec)

end program rstest
