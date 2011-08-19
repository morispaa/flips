program inverse_matrix
	! This program tests the speed of flips
	! in inverting square matrices
	
	use flips
	
	implicit none
	
	type(flips_s) :: gg
	integer :: n, nb,id,i
	real, dimension(:,:), allocatable :: amat, eye
	real :: ts,te,tot
	
	write(*,*) 'Give matrix size: '
	read(*,*) n
	write(*,*) 'Give rotation buffer size: '
	read(*,*) nb
	write(*,*) 'Give problem ID (0 uses memory): '
	read(*,*) id
	
	! Init flips
	if (id <= 0) then
		call flips_init(gg,n,n,buffersize=nb)
	else
		call flips_init(gg,n,n,id,nb)
	end if
	
	allocate(amat(n,n),eye(n,n))
	
	eye = 0.0
	do i = 1,n
		eye(i,i) = 1.0
	end do
	
	call random_number(amat)
	
	write(*,*) 'flips START!'
	
	call cpu_time(ts)
	! Feed into flips
	do i = 1,n
		call flips_add(gg,1,amat(i,:),eye(i,:))
	end	do
	
	! Do the last rotation
	call make_rotations_s(gg)
	
	call cpu_time(te)
	
	tot = te-ts
	
	write(*,*) 'Rotation time: ',tot
	
	
	! Solve
	call cpu_time(ts)
	call solve_utriag_mem_s(gg)
	call cpu_time(te)
	
	tot = te-ts + tot
	
	write(*,*) 'Solving time: ',te-ts
	write(*,*) 'Total time: ',tot
	
	! Kill
	call flips_kill(gg,.FALSE.)
	
end program inverse_matrix
			 