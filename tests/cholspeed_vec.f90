program cholspeedvec
  ! Tests the speed of modified Cholesky factorization (vector version)

  use flips

  implicit none

  integer :: n,i
  character(len=1) :: type

  real(4), dimension(:), allocatable :: samat,scmat 
  real(8), dimension(:), allocatable :: damat,dcmat 
  complex(4), dimension(:), allocatable :: camat,ccmat 
  complex(8), dimension(:), allocatable :: zamat,zcmat 

  real(8) :: stime, etime, ttime

  write(*,*) 'Give size:'
  read(*,*) n
  write(*,*) 'Give type:'
  read(*,*) type

!n=2000
!type='d'

  select case (type)

  case ('s')
     
     allocate(samat(n*(n+1)/2),scmat(n*(n+1)/2))



     ! We do not really have to care about the symmetry
     call random_number(samat)

     ! Just make sure that amat is positive definite
     do i = 1,n

        samat(rind(i,i,n)) = n
     end do

write(*,*) 'Start!'

     call cpu_time(stime)
     call modcholvec_s(n,n*(n+1)/2,samat,scmat)
     call cpu_time(etime)
     ttime = etime - stime

     deallocate(samat,scmat)

  case ('d')
     
     allocate(damat(n*(n+1)/2),dcmat(n*(n+1)/2))

     ! We do not really have to care about the symmetry
     call random_number(damat)

     ! Just make sure that amat is positive definite
     do i = 1,n

        damat(rind(i,i,n)) = n
     end do

write(*,*) 'Start!'


     call cpu_time(stime)
     call modcholvec_d(n,n*(n+1)/2,damat,dcmat)
     call cpu_time(etime)
     ttime = etime - stime

     deallocate(damat,dcmat)

  case ('c')
     
     allocate(camat(n*(n+1)/2),ccmat(n*(n+1)/2),samat(n*(n+1)/2),scmat(n*(n+1)/2))

     ! We do not really have to care about the symmetry
     call random_number(samat)
     call random_number(scmat)

     camat = samat + (0.0,1.0) * scmat

     ! Just make sure that amat is positive definite
     do i = 1,n
        camat(rind(i,i,n)) = n
     end do

write(*,*) 'Start!'


     call cpu_time(stime)
     call modcholvec_c(n,n*(n+1)/2,camat,ccmat)
     call cpu_time(etime)
     ttime = etime - stime

     deallocate(camat,ccmat,samat,scmat)


  case ('z')
     
     allocate(zamat(n*(n+1)/2),zcmat(n*(n+1)/2),damat(n*(n+1)/2),dcmat(n*(n+1)/2))

     ! We do not really have to care about the symmetry
     call random_number(damat)
     call random_number(dcmat)

     zamat = damat + (0.0,1.0) * dcmat

     ! Just make sure that amat is positive definite
     do i = 1,n
        zamat(rind(i,i,n)) = n
     end do

write(*,*) 'Start!'


     call cpu_time(stime)
     call modcholvec_z(n,n*(n+1)/2,zamat,zcmat)
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



end program cholspeedvec
  
