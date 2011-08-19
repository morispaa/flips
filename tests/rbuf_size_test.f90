program rotsizetest
  use flips
  use rand

  implicit none

  type(flips_s) :: gg
  integer :: nsize, bufsize, n, id
  real :: ts, te, total
  real, dimension(:), allocatable :: arow
  real, dimension(:), allocatable :: yrow
  real ,dimension(1) :: error

  call set_rand_seed()

  write(*,*) 'Give the number of unknowns:'
  read(*,*) nsize
  write(*,*) 'Give the size of the rotation buffer:'
  read(*,*) bufsize
  write(*,*) 'Give file idnum (zero uses memory):'
  read(*,*) id

	error = 1.0

  ! initialize
  if (id <= 0) then
     call flips_init(gg,nsize,1,buffersize=bufsize)
  else
     call flips_init(gg,nsize,1,idnum=id,buffersize=bufsize)
  end if

  allocate(arow(nsize),yrow(1))

  total = 0.0

  ! Main loop
  do n = 1,nsize

     ! Create data
     call random_number(arow)
     call random_number(yrow)
     !yrow = 1.0

     ! add to flips  
     call cpu_time(ts)
     call flips_add(gg,1,arow,yrow)
     call cpu_time(te)
     total = total + (te-ts)

  end do

  ! make the last rotation
  !call cpu_time(ts)
  !call make_rotations_s(gg)
  !call cpu_time(te)

  !total = total + (te-ts)

  !write(*,*) 'Total CPU time for rotations:',total

!	write(*,*) gg%solexists

  ! Solve (do not calculate residual)
  call cpu_time(ts)
  call flips_solve(gg,.FALSE.)
  call cpu_time(te)
  
  total = total + te - ts
  
  write(*,*) 'Total CPU time for solving:', total

!	write(*,*) gg%solexists

	 ! Calculate residual
	 
!	 write(*,*) 'TMPRES:',gg%tmpres
	 
	 call cpu_time(ts)
	 call calculate_residual_s(gg)
	 call cpu_time(te)
	 
	 total = te-ts
	 
	 write(*,*) 'Residual calculation time:', total
	 write(*,*) 'Residual:',gg%residual
	 
	 ! Calculate covariance (DIAGONAL)
	 call cpu_time(ts)
	 call flips_calc_cov(gg,full=.FALSE.)
	 call cpu_time(te)
	
	 write(*,*) 'Covariance (diag) calc time:',te-ts
	 
	 !if (allocated(gg%cmat)) deallocate(gg%cmat)
	 !print*, "hep"
	 
	 !call print_info_s(gg)
	 
	!if (allocated(gg%cmat)) deallocate(gg%cmat)
	
	!call print_info_s(gg)
	 
	 	 ! Calculate covariance (FULL)
	 call cpu_time(ts)
	 call flips_calc_cov(gg,full=.TRUE.)
	 call cpu_time(te)
	
	 write(*,*) 'Covariance (full) calc time:',te-ts

  !call print_info_s(gg)
	

  call flips_kill(gg)
  
  deallocate(arow)

end program rotsizetest
