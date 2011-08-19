program rstest
  use flips

  ! This program tests flips_resize in different situations
  ! Single precision real version

  implicit none

  type(flips_s) :: s1, s2,s3,s4

  real(sp), dimension(:), allocatable :: amat
  real(sp), dimension(:), allocatable :: meas,erro
  logical, dimension(:), allocatable :: re
  real(sp) :: diff, maxdiff
  real(dp) :: ts,te
  logical, dimension(:), allocatable :: remo

  write(*,*) 'Test 1: Marginalize 500 first unknowns from a 1000 unknowns problem'
  ! ********************************************************************

  write(*,*) '  Memory storage'

  ! Initialize s1
  call flips_init(s1,1000,1,bandwidth=1,common=200)
  
  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000))
  call random_number(amat)
  call random_number(meas)
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
  maxdiff = maxval(s2%solmat - s1%solmat(501:1000))
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max diff:',maxdiff
  write(*,*) '    Resize time:',te-ts
  write(*,*) '    BW=',s2%bw,'CV=',s2%common

  call flips_kill(s1,.FALSE.)
  call flips_kill(s2,.FALSE.)
  deallocate(amat,meas,erro)

write(*,*) '  File storage'

  ! Initialize s1
  call flips_init(s1,1000,1,idnum=1,buffersize=100)
  
!write(*,*) 'FLIPS init OK'  

  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000))
  call random_number(amat)
  call random_number(meas)
  call random_number(erro)
  erro = erro + 0.5
  
  ! Feed data in s1
  call flips_add(s1,1000,amat,meas,erro)

!write(*,*) 'FLIPS add OK'  

  call cpu_time(ts)
  ! Resize by marginalizing 500 first unknowns
  call flips_resize(s2,s1,newsize=500,idnum=2)

!write(*,*) 'FLIPS resize OK'  

  call cpu_time(te)

  ! Solve both problems
  call flips_solve(s1)
  call flips_solve(s2)

!write(*,*) 'FLIPS solve OK'  



  ! Read results from file
  read(113,pos=1) meas
  read(213,pos=1) erro(1:500)

!write(*,*) meas(501:505)
!write(*,*) erro(1:5)

  ! Compare results
  diff = sum(erro(1:500) - meas(501:1000))/500
  maxdiff = maxval(erro(1:500) - meas(501:1000))
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max diff:',maxdiff
  write(*,*) '    Resize time:',te-ts,'\n'

  call flips_kill(s1,keepfiles=.FALSE.)
  call flips_kill(s2,keepfiles=.FALSE.)
  deallocate(amat,meas,erro)

write(*,*) 'Test 2: Remove last 500 unknowns from a 1000 unknowns problem'

 write(*,*) '  Memory storage'

  ! Initialize s1
  call flips_init(s1,1000,1,buffersize=50,bandwidth=1,common=10)
  


  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000),remo(1000))
  call random_number(amat)
  call random_number(meas)
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

!write(*,*) '!!'

  ! Solve both problems
  call flips_solve(s1)



!write(*,*) 's1'

  call flips_solve(s2)



!write(*,*) 's2'

!write(*,*) s1%solmat(501:505)
!write(*,*) s2%solmat(1:5)

  ! Compare results
  diff = sum(s2%solmat - s1%solmat(1:500))/500
  maxdiff = maxval(s2%solmat - s1%solmat(1:500))
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max diff:',maxdiff
  write(*,*) '    Resize time:',te-ts,'\n'
  write(*,*) '    BW=',s2%bw,'CV=',s2%common

  call flips_kill(s1,.FALSE.)
  call flips_kill(s2,.FALSE.)
  deallocate(amat,meas,erro,remo)

 write(*,*) '  File storage'

  ! Initialize s1
  call flips_init(s1,1000,1,idnum=1,buffersize=100)

!write(*,*) 'FLIPS init OK'  

  ! Create data
  allocate(amat(1000*1000),meas(1000),erro(1000),remo(1000))
  call random_number(amat)
  call random_number(meas)
  call random_number(erro)
  erro = erro + 0.5
  remo = .FALSE.
  remo(501:1000) = .TRUE.

!write(*,*) 'data created OK'  
  
  ! Feed data in s1 
  call flips_add(s1,1000,amat,meas,erro)

!write(*,*) 'FLIPS add OK'  

  call cpu_time(ts)
  ! Resize by marginalizing last 500 unknowns
  call flips_resize(s2,s1,remove=remo,idnum=2)

!write(*,*) 'FLIPS resize OK'  

  call cpu_time(te)

!write(*,*) '!!'

  ! Solve both problems
  call flips_solve(s1)

!write(*,*) 'FLIPS solve s1 OK'  

!write(*,*) 's1'

  call flips_solve(s2)

!write(*,*) 'FLIPS solve s2 OK'  

!write(*,*) 's2'
  ! Read results from file
  read(113,pos=1) meas
  read(213,pos=1) erro(1:500)
!meas = s1%solmat
!erro(1:500) = s2%solmat

!write(*,*) meas(501:505)
!write(*,*) erro(1:5)

  ! Compare results
  diff = sum(erro(1:500) - meas(1:500))/500
  maxdiff = maxval(erro(1:500) - meas(1:500))
  write(*,*) '    Mean diff:',diff
  write(*,*) '    Max diff:',maxdiff
  write(*,*) '    Resize time:',te-ts,'\n'

  call flips_kill(s1,.FALSE.)
  call flips_kill(s2,.FALSE.)
  deallocate(amat,meas,erro,remo)

end program rstest
