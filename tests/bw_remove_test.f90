program bwtest1
  use flips

  implicit none

  type(flips_s) :: h,h2

  real(sp), dimension(100,100) :: a
  real(sp), dimension(100) :: sol, meas , fs1
  real(sp), dimension(101) :: row 
  real(sp), dimension(11) :: ww
  real(sp), dimension(101) :: fs2
  logical, dimension(100) :: rmask

  integer :: i


  do i = 1,100
     sol(i) = real(i,8)
  end do

  a = 0.0
  do i = 1,90
     call random_number(ww)
     a(i,i:i+10) = ww
  end do

  do i = 1,100
     a(i,i) = 1.0
  end do

  meas = matmul(a,sol)

  call flips_init(h,100,1,bandwidth=1,buffersize=10)

  do i = 1,100
     call flips_add(h,1,a(i,:),meas(i))
  end do

  call flips_solve(h)

  fs1 = h%solmat

  rmask=.FALSE.
  rmask(41:60) = .TRUE.

  call flips_resize(h2,h,remove=rmask,newsize=101)

  write(*,*) 'h2cols=',h2%ncols

  do i = 1,21
     row = 0.0
     row(80+i) = 2.0
     if (i == 1) write(*,*) row
     call flips_add(h2,1,row,1.0)
  end do

  call make_rotations_s(h2)

  write(*,*) h2%ymat

  !write(*,*) h2%rmat(rind(101,101,101,h2%bw))

  call flips_solve(h2)

  fs2 = h2%solmat

  write(*,*) fs1

  write(*,*) '***********************************************************'

  write(*,*) fs2

  write(*,*) '***********************************************************'

  write(*,*) h2%bw,h%bw


  call flips_kill(h)
  call flips_kill(h2)



end program bwtest1
  
