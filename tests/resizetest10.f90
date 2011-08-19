program test
  use flips

  implicit none

  type(flips_d) :: d1,d2

  real(dp) :: a(10,10),b(10,20),meas1(10),meas2(10),c(10,20),d(10,20),rd2(20,20)
  integer :: i
  logical :: rr(20)

!  verbose=.TRUE.

  a = 0.0
  b=0.0
  c=0.0
  d=0.0
  meas2 = 0.0

  do i=1,10
     a(i,i) = 1.0_8
     d(i,i) = 1.0_8
     b(i,i) = 1.0_8
     b(i,10+i) = -1.0_8
     meas1(i) = real(10-i,8)
     c(i,10+i)=1.0_8
     !call random_number(c(i,11:20))
  end do

!  write(*,*) c(1,:)

  call flips_init(d1,10,1,buffersize=2,common=2)

  do i = 1,10

     call flips_add(d1,1,a(i,:),meas1(i),0.01_8)
     !call flips_rotate(d1)
     !write(*,*) 'i=',i,' nrows=',d1%nrows
     !write(*,*) 'hoo'
  end do

 ! write(*,*) 'd1%common',d1%common

  call flips_resize(d2,d1,newsize=20,buffersize=50)

!  write(*,*) 'common',d2%common
!  write(*,*) 'nbuf',d2%nbuf
!  write(*,*) 'nrows',d2%nrows
!  write(*,*) 'band',d2%bw
!  write(*,*) 'ncols',d2%ncols
!  write(*,*) 'nrhs',d2%nrhs

  call flips_kill(d1)

  do i=1,10
     call flips_add(d2,1,b(i,:),meas2(i),1.0_8)
     !call flips_rotate(d2)
    ! write(*,*) 'i=',i,' nrows=',d2%nrows
  end do

  call flips_rotate(d2)

!  write(*,*) 'nrows',d2%nrows



  !call print_info_d(d2)

  call flips_solve(d2)
  !call flips_calc_cov(d2,.FALSE.)

  verbose = .FALSE.

  d2%mrotbuf = 0.0

  !write(*,*) ' nrows=',d2%nrows

  !write(*,*) d2%solmat
  write(*,*) ' '
  !write(*,*) d2%cmat
  write(*,*) ' '
  !rr = .FALSE.
  !rr(1:10) = .TRUE.

  do i=1,10
     call flips_add(d2,1,c(i,:),meas1(i),1.0_8)
     call flips_rotate(d2) 

 !    write(*,*) 'i=',i,' nbuf=',d2%mrotbuf
 !    d2%mrotbuf = 0.0
!     write(*,*) ' '
  end do

  write(*,*) ' '

  call flips_solve(d2)
  !call flips_calc_cov(d2,.FALSE.)

  !call flips_resize(d1,d2,remove=rr)

  !call flips_solve(d1)
 !call flips_calc_cov(d1,.FALSE.)
!  write(*,*) d2%ymat
!write(*,*) 'xxxxxxxx'

  write(*,*) d2%solmat
  write(*,*) ' '
  !write(*,*) d2%cmat
  write(*,*) '---------------------------- '
  !rr = .FALSE.
  !rr(1:10) = .TRUE.

  !call get_matrix_d('rmat',d2,rd2)

  !call flips_kill(d1)


  call flips_init(d1,20,1,common=10,bandwidth=1,buffersize=50)

 ! write(*,*) 'd1%common',d1%common

  do i = 1,10
     call flips_add(d1,1,d(i,:),meas1(i),0.01_8)
  end do

 

  call flips_rotate(d1)

  do i=1,10
     call flips_add(d1,1,c(i,:),meas1(i),1.0_8)
     call flips_rotate(d1)
   !  write(*,*) 'i=',i,' nbuf=',d1%mrotbuf
   !  write(*,*) 'i=',i,' nbuf=',d1%ymat
   !  write(*,*) ' '
  end do

 do i=1,10
     call flips_add(d1,1,b(i,:),meas2(i),1.0_8)
  end do

  call flips_rotate(d1)

 ! write(*,*) '----'

 ! write(*,*) d1%ymat

 ! write(*,*) '-------'

  call flips_solve(d1)

  write(*,*) d1%solmat

  write(*,*) '----'

  !write(*,*) size(d1%rmat)
  !write(*,*) size(d2%rmat)
  !write(*,*) d2%common
  !write(*,*) d2%bw

 ! d2%ymat = d1%ymat

 ! call flips_solve(d2)

 ! write(*,*) ' ---- '
 ! write(*,*) d2%solmat

  !write(*,*) minval(d1%rmat-d2%rmat)

  call flips_kill(d2)
  call flips_kill(d1)
end program test
