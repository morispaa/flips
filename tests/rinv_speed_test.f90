program r_inv_speed
	! Tests the speed of the inversion of
	! the R matrix
	use flips
	
	implicit none
	
	type(flips_s) :: gg
	real, dimension(:), allocatable :: r,invr
	real :: ts, te ,tot
	integer :: n, id, k
	
	write(*,*) 'Give size of R:'
	read(*,*) n
	write(*,*) 'Give ID (0 uses memory):'
	read(*,*) id
	
	if (id > 0) then
		call flips_init(gg,n,1,idnum=id)
	else
		call flips_init(gg,n,1)
	end if
	
	allocate(r(n*(n+1)/2),invr(n*(n+1)/2))
	
	call random_number(r)
	
	if (gg%use_files) then
		!do k = 1,n*(n+1)/2
			write(gg%idnum*100+11,POS=1) r
		!end do
	else
		gg%rmat = r
	end if
	
	write(*,*) 'HEP!'
	
	call cpu_time(ts)
	call invert_r_s(gg)
	call cpu_time(te)
	
	write(*,*) 'Time: ',te-ts

	call flips_kill(gg,.FALSE.)

end program r_inv_speed
