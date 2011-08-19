program fullcovtest
  use flips

  implicit none

  type(flips_s) :: fs
  integer :: n,i
  real(sp), dimension(:), allocatable :: amat,mvec,emat

  real(dp) :: ts,te

  write(*,*) 'Anna koko:'
  read(*,*) n

  allocate(amat(n**2),mvec(n),emat(n*(n+1)/2))

  call random_number(amat)
  call random_number(mvec)
  call random_number(emat)

  do i = 1,n
     emat(rind(i,i,n)) = n
  end do

  call cpu_time(ts)
  call flips_init(fs,n,1,buffersize=100)
write(*,*) 'Adding data...'
  call flips_add_fc(fs,n,amat,mvec,emat)
write(*,*) 'Done!'
  call flips_solve(fs)
  call cpu_time(te)

  write(*,*) 'Size:',n,'CPU time:',te-ts
  write(*,*) fs%solmat(1:5)

  deallocate(amat,emat,mvec)

  call flips_kill(fs)

end program fullcovtest
