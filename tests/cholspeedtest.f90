program cholspeed
  ! Tests the speed of modified Cholesky factorization

  use flips

  implicit none

  integer :: n,i
  character(len=1) :: type

  real(4), dimension(:,:), allocatable :: samat,scmat 
  real(8), dimension(:,:), allocatable :: damat,dcmat 
  complex(4), dimension(:,:), allocatable :: camat,ccmat 
  complex(8), dimension(:,:), allocatable :: zamat,zcmat 

  real(8) :: stime, etime, ttime

  write(*,*) 'Give size:'
  read(*,*) n
  write(*,*) 'Give type:'
  read(*,*) type

  select case (type)

  case ('s')
     
     allocate(samat(n,n),scmat(n,n))

     ! We do not really have to care about the symmetry
     call random_number(samat)

     ! Just to make sure that amat is positive definite
     do i = 1,n
        samat(i,i) = n
     end do

write(*,*) 'Start!'

     call cpu_time(stime)
     call modcholmat_s(n,samat,scmat)
     call cpu_time(etime)
     ttime = etime - stime

     deallocate(samat,scmat)

  case ('d')
     
     allocate(damat(n,n),dcmat(n,n))

     ! We do not really have to care about the symmetry
     call random_number(damat)

     ! Just to make sure that amat is positive definite
     do i = 1,n
        damat(i,i) = n
     end do

write(*,*) 'Start!'


     call cpu_time(stime)
     call modcholmat_d(n,damat,dcmat)
     call cpu_time(etime)
     ttime = etime - stime

     deallocate(damat,dcmat)

  case ('c')
     
     allocate(camat(n,n),ccmat(n,n),samat(n,n),scmat(n,n))

     ! We do not really have to care about the symmetry
     call random_number(samat)
     call random_number(scmat)

     camat = samat + (0.0,1.0) * scmat

     ! Just to make sure that amat is positive definite
     do i = 1,n
        camat(i,i) = n
     end do

write(*,*) 'Start!'


     call cpu_time(stime)
     call modcholmat_c(n,camat,ccmat)
     call cpu_time(etime)
     ttime = etime - stime

     deallocate(camat,ccmat,samat,scmat)


  case ('z')
     
     allocate(zamat(n,n),zcmat(n,n),damat(n,n),dcmat(n,n))

     ! We do not really have to care about the symmetry
     call random_number(damat)
     call random_number(dcmat)

     zamat = damat + (0.0,1.0) * dcmat

     ! Just to make sure that amat is positive definite
     do i = 1,n
        zamat(i,i) = n
     end do

write(*,*) 'Start!'


     call cpu_time(stime)
     call modcholmat_z(n,zamat,zcmat)
     call cpu_time(etime)
     ttime = etime - stime

     deallocate(zamat,zcmat,damat,dcmat)


  case default
     
     write(*,*) 'Unknown type. Exiting'
     stop

  end select


  write(*,*) 'Size:',n
  write(*,*) 'Type:',type
  write(*,*) 'CPU Time:',ttime



end program cholspeed
  
